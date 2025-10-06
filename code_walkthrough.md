## faasr_test.R — Detailed Walkthrough

### Overview
- Purpose: Load a FaaSr workflow JSON and execute functions in InvokeNext order locally.
- Key features:
  - Sources user functions from `faasr_data/R` and local APIs from `R/`.
  - One-time validation: schema + cycle detection.
  - Per-node predecessor gating via `.done` markers (unconditional predecessors only).
  - Per-node working directories under `faasr_data/temp/<node>`.
  - **Rank support**: Functions can execute multiple parallel ranks via `FuncName(N)` notation.
  - **Invocation ID**: Generates and exposes workflow invocation ID via `faasr_invocation_id()`.
  - **Conditional branching**: Supports `{True: [...], False: [...]}` branches based on function return values.
  - **Clean state**: Clears temp and files directories at start to avoid stale artifacts.

### 1) Setup and JSON loading
Reads the workflow JSON into `wf` and validates required fields.

```r
faasr_test <- function(json_path) {
  if (!file.exists(json_path)) stop(sprintf("Workflow JSON not found: %s", json_path))
  wf <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)
  if (is.null(wf$ActionList)) stop("Invalid workflow JSON: missing ActionList")
  if (is.null(wf$FunctionInvoke)) stop("Invalid workflow JSON: missing FunctionInvoke")
}
```

### 2) Source user functions and APIs
Loads all `.R` files from `faasr_data/R` (user functions) and `R/` (local APIs) into the global environment.

```r
src_dirs <- c(file.path("faasr_data", "R"), "R")
for (d in src_dirs) {
  if (dir.exists(d)) {
    rfiles <- list.files(d, pattern = "\\.R$", full.names = TRUE)
    for (f in rfiles) {
      try(source(f, local = .GlobalEnv), silent = TRUE)
    }
  }
}
```

### 3) Initialize global state
Sets up rank info and invocation ID in the global environment for API access.

```r
# Initialize global environment for rank info
assign("FAASR_CURRENT_RANK_INFO", NULL, envir = .GlobalEnv)

# Generate and set invocation ID
invocation_id <- .faasr_generate_invocation_id(wf)
assign("FAASR_INVOCATION_ID", invocation_id, envir = .GlobalEnv)
```

The invocation ID generator:
```r
.faasr_generate_invocation_id <- function(wf) {
  # Check if InvocationID is already set in the workflow
  if (!is.null(wf$InvocationID) && nzchar(wf$InvocationID)) {
    return(wf$InvocationID)
  }
  
  # Check if InvocationIDFromDate format is specified
  if (!is.null(wf$InvocationIDFromDate) && nzchar(wf$InvocationIDFromDate)) {
    date_format <- wf$InvocationIDFromDate
    return(format(Sys.time(), date_format))
  }
  
  # Default: generate a UUID-like ID
  paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-", 
         paste(sample(c(0:9, letters[1:6]), 8, replace = TRUE), collapse = ""))
}
```

### 4) Queue and working directories
Initializes a queue with rank-aware items: each item is `list(name, rank_current, rank_max)`.

```r
# Build a simple queue for the functions in the workflow
# Each item is a list with: name, rank_current, rank_max
queue <- list(list(name = wf$FunctionInvoke, rank_current = 1, rank_max = 1))

faasr_data_wd <- file.path(getwd(), "faasr_data")
if (!dir.exists(faasr_data_wd)) dir.create(faasr_data_wd, recursive = TRUE)
temp_dir <- file.path(faasr_data_wd, "temp")

# Clean temp at start to avoid stale .done files affecting gating
if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)

state_dir <- file.path(temp_dir, "faasr_state_info")
files_dir <- file.path(faasr_data_wd, "files")
if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
if (!dir.exists(state_dir)) dir.create(state_dir, recursive = TRUE)
if (!dir.exists(files_dir)) dir.create(files_dir, recursive = TRUE)
```

### 5) Clean files directory
Removes all previous outputs to ensure clean runs.

```r
# Clean files outputs at start to avoid stale artifacts between runs
files_to_remove <- try(list.files(files_dir,
  all.files = TRUE,
  full.names = TRUE,
  include.dirs = TRUE,
  recursive = FALSE
), silent = TRUE)
if (!inherits(files_to_remove, "try-error") && length(files_to_remove)) {
  base_names <- basename(files_to_remove)
  keep <- base_names %in% c(".", "..", ".gitkeep")
  to_delete <- files_to_remove[!keep]
  if (length(to_delete)) unlink(to_delete, recursive = TRUE, force = TRUE)
}
```

### 6) Schema handling and one-time global validation
Downloads a fallback schema if needed, then validates the workflow once for JSON schema and graph correctness (cycles).

```r
schema_repo_path <- file.path(getwd(), "schema.json")
schema_temp_path <- file.path(temp_dir, "FaaSr.schema.json")
if (!file.exists(schema_temp_path)) {
  schema_url <- "https://raw.githubusercontent.com/FaaSr/FaaSr-package/main/schema/FaaSr.schema.json"
  try(utils::download.file(schema_url, schema_temp_path, quiet = TRUE), silent = TRUE)
}

cfg_once <- faasr_configuration_check(wf, state_dir)
if (!identical(cfg_once, TRUE)) stop(cfg_once)
```

What `faasr_configuration_check` does:

```r
faasr_configuration_check <- function(faasr, state_dir) {
  # Basic JSON sanity
  if (is.null(faasr$ActionList) || is.null(faasr$FunctionInvoke)) {
    return("JSON parsing error")
  }

  # JSON schema validation (prefers ./schema.json, falls back to temp)
  schema_file <- file.path(getwd(), "schema.json")
  if (!file.exists(schema_file)) {
    temp_dir <- dirname(state_dir)
    cand <- file.path(temp_dir, "FaaSr.schema.json")
    if (file.exists(cand)) schema_file <- cand
  }
  if (file.exists(schema_file) && requireNamespace("jsonvalidate", quietly = TRUE)) {
    json_txt <- try(jsonlite::toJSON(faasr, auto_unbox = TRUE), silent = TRUE)
    if (!inherits(json_txt, "try-error")) {
      ok <- try(jsonvalidate::json_validate(json = json_txt, schema = schema_file), silent = TRUE)
      if (!inherits(ok, "try-error") && isFALSE(ok)) {
        return("JSON parsing error")
      }
    }
  }

  # Workflow cycle check (DFS on ActionList graph)
  graph_ok <- try(.faasr_check_workflow_cycle_local(faasr, faasr$FunctionInvoke), silent = TRUE)
  if (inherits(graph_ok, "try-error")) {
    return("cycle errors")
  }

  TRUE
}
```

### 7) Main execution loop
Dequeues nodes with rank information, enforces predecessor gating, then executes the user function in an isolated directory.

```r
visited <- character()
while (length(queue) > 0) {
  # Get the next function in the queue
  queue_item <- queue[[1]]; queue <- queue[-1]
  name <- queue_item$name
  rank_current <- queue_item$rank_current
  rank_max <- queue_item$rank_max
  
  # Create unique visited key including rank
  visited_key <- paste0(name, "_rank_", rank_current, "/", rank_max)
  if (visited_key %in% visited) next
  
  node <- wf$ActionList[[name]]
  if (is.null(node)) next

  # Get the function name and arguments
  func_name <- node$FunctionName
  args <- node$Arguments %||% list()

  # Predecessor gating prior to execution (skip until ready)
  cfg <- faasr_predecessor_gate(wf$ActionList, name, state_dir)
  if (identical(cfg, "next")) {
    next
  }
  if (!identical(cfg, TRUE)) {
    stop(cfg)
  }

  # Set rank info in global environment before execution
  if (rank_max > 1) {
    rank_info <- paste0(rank_current, "/", rank_max)
    assign("FAASR_CURRENT_RANK_INFO", rank_info, envir = .GlobalEnv)
    cli::cli_h2(sprintf("Running %s (%s) - Rank %s", name, func_name, rank_info))
  } else {
    assign("FAASR_CURRENT_RANK_INFO", NULL, envir = .GlobalEnv)
    cli::cli_h2(sprintf("Running %s (%s)", name, func_name))
  }
  
  # Check if the function exists
  if (!exists(func_name, mode = "function", envir = .GlobalEnv)) {
    stop(sprintf("Function not found in environment: %s (node %s)", func_name, name))
  }

  # Create a temporary directory for the function
  run_dir <- file.path(temp_dir, name)
  if (!dir.exists(run_dir)) dir.create(run_dir, recursive = TRUE)
  orig_wd <- getwd()

  # Set the working directory to the temporary directory
  on.exit(setwd(orig_wd), add = TRUE)
  # Fix the data root so faasr_* uses faasr_data/files regardless of WD
  Sys.setenv(FAASR_DATA_ROOT = faasr_data_wd)
  setwd(run_dir)

  # Get the function and execute it
  f <- get(func_name, envir = .GlobalEnv)
  res <- try(do.call(f, args), silent = TRUE)
  if (inherits(res, "try-error")) {
    cli::cli_alert_danger(as.character(res))
    stop(sprintf("Error executing %s", func_name))
  }
  
  # Mark success similar to package's .done files (only once per function name, not per rank)
  if (rank_current == rank_max) {
    utils::write.table("TRUE", file = file.path(state_dir, paste0(name, ".done")), row.names = FALSE, col.names = FALSE)
  }

  visited <- c(visited, visited_key)
  
  # Enqueue next functions (only from the last rank to avoid duplicates)
  if (rank_current == rank_max) {
    nexts <- node$InvokeNext %||% list()
    # flatten possible list
    if (is.character(nexts)) nexts <- as.list(nexts)
    for (nx in nexts) {
      if (is.character(nx)) {
        # Parse the string to extract rank notation
        parsed <- .faasr_parse_invoke_next_string(nx)
        for (r in 1:parsed$rank) {
          queue <- c(queue, list(list(name = parsed$func_name, rank_current = r, rank_max = parsed$rank)))
        }
      } else if (is.list(nx)) {
        # if conditional object {True:[], False:[]},
        branch <- if (isTRUE(res) && !is.null(nx$True)) nx$True else if (identical(res, FALSE) && !is.null(nx$False)) nx$False else NULL
        if (is.null(branch)) next
        for (b in branch) {
          parsed <- .faasr_parse_invoke_next_string(b)
          for (r in 1:parsed$rank) {
            queue <- c(queue, list(list(name = parsed$func_name, rank_current = r, rank_max = parsed$rank)))
          }
        }
      }
    }
  }
}

cli::cli_alert_success("Workflow completed (R-only, sequential)")
TRUE
```

### 8) Helper: Null-coalescing operator
Convenience to default NULL to a value.

```r
`%||%` <- function(x, y) if (is.null(x)) y else x
```

### 9) Helper: Parse InvokeNext strings
Extracts function name and rank from strings like "FuncName" or "FuncName(3)".

```r
.faasr_parse_invoke_next_string <- function(invoke_string) {
  s <- trimws(invoke_string)

  # Reject any bracketed conditions or unsupported syntax
  if (grepl("\\[|\\]", s)) {
    stop("Invalid InvokeNext format: only 'FuncName' and 'FuncName(N)' are supported")
  }

  # Match optional (N) at the end; capture name and digits
  m <- regexec("^(.*?)(?:\\((\\d+)\\))?$", s)
  mm_all <- regmatches(s, m)
  if (!length(mm_all) || length(mm_all[[1]]) != 3) {
    stop("Invalid InvokeNext format")
  }
  mm <- mm_all[[1]]

  func_name <- trimws(mm[2])
  if (!nzchar(func_name)) {
    stop("Invalid InvokeNext: empty function name")
  }

  rank <- 1
  if (nzchar(mm[3])) {
    rank <- as.integer(mm[3])
  }

  list(func_name = func_name, condition = NULL, rank = rank)
}
```

### 10) Helper: Predecessor discovery and gating
Finds predecessors of a node by scanning all `InvokeNext` values. Gating only enforces **unconditional** predecessors to avoid blocking on unexecuted conditional branches.

```r
.faasr_find_predecessors <- function(action_list, target_func, unconditional_only = FALSE) {
  preds <- character()
  for (nm in names(action_list)) {
    nx <- action_list[[nm]]$InvokeNext %||% list()
    # normalize to character vector of names
    next_names <- character()
    is_conditional <- FALSE
    if (is.character(nx)) {
      next_names <- sub("\\(.*$", "", nx)
    } else if (is.list(nx)) {
      for (item in nx) {
        if (is.character(item)) {
          next_names <- c(next_names, sub("\\(.*$", "", item))
        } else if (is.list(item)) {
          # This is a conditional branch
          is_conditional <- TRUE
          if (!unconditional_only) {
            if (!is.null(item$True)) next_names <- c(next_names, sub("\\(.*$", "", unlist(item$True)))
            if (!is.null(item$False)) next_names <- c(next_names, sub("\\(.*$", "", unlist(item$False)))
          }
        }
      }
    }
    if (length(next_names) && any(next_names == target_func)) {
      preds <- c(preds, nm)
    }
  }
  unique(preds)
}

# Predecessor gating: if multiple UNCONDITIONAL predecessors, require all .done present
# Conditional branches are handled by the branching logic itself, not gating
faasr_predecessor_gate <- function(action_list, current_func, state_dir) {
  # Only gate on unconditional predecessors to avoid blocking on unexecuted conditional branches
  predecessors <- .faasr_find_predecessors(action_list, current_func, unconditional_only = TRUE)
  if (length(predecessors) > 1) {
    done_list <- try(list.files(state_dir), silent = TRUE)
    if (inherits(done_list, "try-error")) done_list <- character()
    for (p in predecessors) {
      if (!(paste0(p, ".done") %in% done_list)) {
        return("next")
      }
    }
  }
  TRUE
}
```

**Key insight**: When a function appears in a conditional branch (`{True: [...], False: [...]}`), we don't enforce predecessor gating because only one branch will execute. The branching logic handles which path to take based on the return value.

### 11) Helper: Build adjacency list and cycle detection
Builds a graph from the workflow and detects cycles via DFS.

```r
.faasr_build_adjacency <- function(action_list) {
  adj <- list()
  for (nm in names(action_list)) {
    nx <- action_list[[nm]]$InvokeNext %||% list()
    next_names <- character()
    if (is.character(nx)) {
      next_names <- sub("\\(.*$", "", nx)
    } else if (is.list(nx)) {
      for (item in nx) {
        if (is.character(item)) {
          next_names <- c(next_names, sub("\\(.*$", "", item))
        } else if (is.list(item)) {
          if (!is.null(item$True)) next_names <- c(next_names, sub("\\(.*$", "", unlist(item$True)))
          if (!is.null(item$False)) next_names <- c(next_names, sub("\\(.*$", "", unlist(item$False)))
        }
      }
    }
    if (length(next_names)) adj[[nm]] <- unique(next_names)
  }
  adj
}

.faasr_check_workflow_cycle_local <- function(faasr, start_node) {
  action_list <- faasr$ActionList
  if (is.null(action_list) || !length(action_list)) {
    stop("invalid action list")
  }
  if (is.null(start_node) || !nzchar(start_node) || !(start_node %in% names(action_list))) {
    stop("invalid start node")
  }

  adj <- .faasr_build_adjacency(action_list)

  visited <- new.env(parent = emptyenv())
  stack <- new.env(parent = emptyenv())

  is_cyclic <- function(node) {
    if (!(node %in% names(action_list))) {
      stop(sprintf("invalid function trigger: %s", node))
    }
    if (isTRUE(get0(node, envir = stack, inherits = FALSE, ifnotfound = FALSE))) {
      return(TRUE)
    }
    assign(node, TRUE, envir = stack)
    assign(node, TRUE, envir = visited)
    nbrs <- adj[[node]]
    if (!is.null(nbrs) && length(nbrs)) {
      for (nbr in nbrs) {
        if (!isTRUE(get0(nbr, envir = visited, inherits = FALSE, ifnotfound = FALSE))) {
          if (is_cyclic(nbr)) return(TRUE)
        } else if (isTRUE(get0(nbr, envir = stack, inherits = FALSE, ifnotfound = FALSE))) {
          return(TRUE)
        }
      }
    }
    assign(node, FALSE, envir = stack)
    FALSE
  }

  if (is_cyclic(start_node)) {
    stop("cycle detected")
  }

  TRUE
}
```

### Key Features Summary

#### 1. Rank Support
- Functions can specify parallel execution with `FuncName(N)` notation
- Each rank (1 to N) is enqueued and executed separately
- `FAASR_CURRENT_RANK_INFO` is set for each rank execution
- Accessible via `faasr_rank()` which returns `list(Rank = "1", MaxRank = "3")`
- `.done` file is written only after the final rank completes

#### 2. Conditional Branching
- `InvokeNext` can contain `{True: [...], False: [...]}` objects
- Functions must return exactly `TRUE` or `FALSE` for branching
- Branch selection uses `isTRUE(res)` for True path, `identical(res, FALSE)` for False path
- Only the selected branch is enqueued

#### 3. Invocation ID
- Generated at workflow start via `.faasr_generate_invocation_id()`
- Respects `InvocationID` if set in JSON
- Uses `InvocationIDFromDate` format if specified (e.g., `"%Y%m%d%H%M"`)
- Defaults to timestamp + random hex string
- Accessible via `faasr_invocation_id()`

#### 4. Predecessor Gating
- Only enforces gating on **unconditional** predecessors
- Conditional branches don't block successors (handled by branching logic)
- Prevents deadlocks when functions appear in multiple conditional paths

#### 5. Clean State
- `temp/` directory is cleared at start (removes stale `.done` files)
- `files/` directory is cleared at start (removes previous outputs)
- Preserves `.gitkeep` files

### R Tips (for newcomers)
- **Vectors and lists**: `c(...)` concatenates vectors; `list(...)` builds heterogeneous containers.
- **Queue operations**: `queue[[1]]` gets first item; `queue[-1]` returns all but first.
- **Name resolution and calls**: `exists(name, mode="function", envir=.GlobalEnv)`, `get(name, envir=.GlobalEnv)`, `do.call(func, args)`.
- **Errors**: `try(expr, silent=TRUE)` captures errors as values; check with `inherits(x, "try-error")`.
- **Working directory safety**: `orig <- getwd(); on.exit(setwd(orig), add=TRUE)` ensures cleanup.
- **File markers**: writing `<node>.done` files allows cheap cross-step synchronization.
- **Environment variables**: `Sys.setenv(VAR = value)` sets env vars; `Sys.getenv("VAR", unset = "")` reads them.
- **Global state**: `assign(name, value, envir = .GlobalEnv)` sets globals; `get0(name, envir = .GlobalEnv, ifnotfound = NULL)` reads them.

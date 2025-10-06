## faasr_test.R — Code Walkthrough

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

> **For detailed algorithm explanations** (DFS cycle detection, parsing, predecessor gating), see [algorithms.md](algorithms.md).

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

The invocation ID generator follows this priority:
1. Use `InvocationID` from workflow JSON if set
2. Use `InvocationIDFromDate` format if specified (e.g., `"%Y%m%d%H%M"`)
3. Default: timestamp + random hex string

### 4) Queue and working directories
Initializes a queue with rank-aware items: each item is `list(name, rank_current, rank_max)`.

```r
# Build a simple queue for the functions in the workflow
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

**Directory structure:**
- `faasr_data/temp/` — Per-function working directories
- `faasr_data/temp/faasr_state_info/` — `.done` marker files
- `faasr_data/files/` — Shared data files

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

### 6) Schema validation and cycle detection
One-time validation checks JSON schema and detects workflow cycles.

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

**What `faasr_configuration_check` does:**
1. Validates basic JSON structure (ActionList, FunctionInvoke)
2. JSON schema validation (if jsonvalidate package available)
3. **Cycle detection** via DFS traversal (see [algorithms.md](algorithms.md#cycle-detection))

### 7) Main execution loop
The core workflow engine that processes functions from the queue one by one.

```r
visited <- character()
while (length(queue) > 0) {
  # Dequeue next function with rank info
  queue_item <- queue[[1]]; queue <- queue[-1]
  name <- queue_item$name
  rank_current <- queue_item$rank_current
  rank_max <- queue_item$rank_max
  
  # Skip if already executed
  visited_key <- paste0(name, "_rank_", rank_current, "/", rank_max)
  if (visited_key %in% visited) next
  
  node <- wf$ActionList[[name]]
  if (is.null(node)) next

  func_name <- node$FunctionName
  args <- node$Arguments %||% list()

  # Predecessor gating: wait for all unconditional predecessors
  cfg <- faasr_predecessor_gate(wf$ActionList, name, state_dir)
  if (identical(cfg, "next")) next  # Not ready yet, skip
  if (!identical(cfg, TRUE)) stop(cfg)

  # Set rank context for user functions
  if (rank_max > 1) {
    rank_info <- paste0(rank_current, "/", rank_max)
    assign("FAASR_CURRENT_RANK_INFO", rank_info, envir = .GlobalEnv)
    cli::cli_h2(sprintf("Running %s (%s) - Rank %s", name, func_name, rank_info))
  } else {
    assign("FAASR_CURRENT_RANK_INFO", NULL, envir = .GlobalEnv)
    cli::cli_h2(sprintf("Running %s (%s)", name, func_name))
  }
  
  # Validate function exists
  if (!exists(func_name, mode = "function", envir = .GlobalEnv)) {
    stop(sprintf("Function not found in environment: %s (node %s)", func_name, name))
  }

  # Create isolated working directory
  run_dir <- file.path(temp_dir, name)
  if (!dir.exists(run_dir)) dir.create(run_dir, recursive = TRUE)
  orig_wd <- getwd()
  on.exit(setwd(orig_wd), add = TRUE)
  Sys.setenv(FAASR_DATA_ROOT = faasr_data_wd)
  setwd(run_dir)

  # Execute function
  f <- get(func_name, envir = .GlobalEnv)
  res <- try(do.call(f, args), silent = TRUE)
  if (inherits(res, "try-error")) {
    cli::cli_alert_danger(as.character(res))
    stop(sprintf("Error executing %s", func_name))
  }
  
  # Mark completion (only after final rank)
  if (rank_current == rank_max) {
    utils::write.table("TRUE", file = file.path(state_dir, paste0(name, ".done")), 
                       row.names = FALSE, col.names = FALSE)
  }

  visited <- c(visited, visited_key)
  
  # Enqueue successors (only from final rank)
  if (rank_current == rank_max) {
    nexts <- node$InvokeNext %||% list()
    if (is.character(nexts)) nexts <- as.list(nexts)
    
    for (nx in nexts) {
      if (is.character(nx)) {
        # Simple successor: "funcB" or "funcB(3)"
        parsed <- .faasr_parse_invoke_next_string(nx)
        for (r in 1:parsed$rank) {
          queue <- c(queue, list(list(name = parsed$func_name, 
                                      rank_current = r, rank_max = parsed$rank)))
        }
      } else if (is.list(nx)) {
        # Conditional branching: {True: [...], False: [...]}
        branch <- if (isTRUE(res) && !is.null(nx$True)) nx$True 
                  else if (identical(res, FALSE) && !is.null(nx$False)) nx$False 
                  else NULL
        if (is.null(branch)) next
        for (b in branch) {
          parsed <- .faasr_parse_invoke_next_string(b)
          for (r in 1:parsed$rank) {
            queue <- c(queue, list(list(name = parsed$func_name, 
                                        rank_current = r, rank_max = parsed$rank)))
          }
        }
      }
    }
  }
}

cli::cli_alert_success("Workflow completed (R-only, sequential)")
TRUE
```

**Key steps:**
1. **Dequeue** with rank info
2. **Check visited** to prevent re-execution
3. **Predecessor gating** waits for unconditional parents
4. **Set rank context** for `faasr_rank()` API
5. **Isolated execution** in per-function directory
6. **Capture result** for conditional branching
7. **Mark completion** via `.done` file
8. **Enqueue successors** with rank expansion and conditional branching

> See [algorithms.md](algorithms.md#main-execution-loop) for detailed breakdown.

### 8) Helper functions

#### Null-coalescing operator
```r
`%||%` <- function(x, y) if (is.null(x)) y else x
```

#### Parse InvokeNext strings
Extracts function name and rank from `"FuncName"` or `"FuncName(N)"`.

```r
.faasr_parse_invoke_next_string <- function(invoke_string) {
  s <- trimws(invoke_string)
  if (grepl("\\[|\\]", s)) {
    stop("Invalid InvokeNext format: only 'FuncName' and 'FuncName(N)' are supported")
  }
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

> See [algorithms.md](algorithms.md#parsing-invokenext-strings) for regex breakdown.

#### Predecessor discovery and gating
Finds predecessors and enforces synchronization for functions with multiple unconditional parents.

```r
.faasr_find_predecessors <- function(action_list, target_func, unconditional_only = FALSE) {
  preds <- character()
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

faasr_predecessor_gate <- function(action_list, current_func, state_dir) {
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

**Key insight:** Only gates on **unconditional** predecessors to avoid deadlocks from unexecuted conditional branches.

> See [algorithms.md](algorithms.md#predecessor-gating) for detailed explanation with examples.

#### Cycle detection
Builds workflow graph and detects cycles via DFS.

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

**Algorithm:** Depth-First Search (DFS) with two tracking structures:
- `visited`: All explored nodes
- `stack`: Current recursion path (detects back edges = cycles)

> See [algorithms.md](algorithms.md#cycle-detection) for complete DFS walkthrough.

#### Generate invocation ID
```r
.faasr_generate_invocation_id <- function(wf) {
  if (!is.null(wf$InvocationID) && nzchar(wf$InvocationID)) {
    return(wf$InvocationID)
  }
  if (!is.null(wf$InvocationIDFromDate) && nzchar(wf$InvocationIDFromDate)) {
    date_format <- wf$InvocationIDFromDate
    return(format(Sys.time(), date_format))
  }
  paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-", 
         paste(sample(c(0:9, letters[1:6]), 8, replace = TRUE), collapse = ""))
}
```

### Key Features Summary

#### 1. Rank Support
- Parallel execution via `FuncName(N)` notation
- Each rank (1 to N) executed separately
- `faasr_rank()` returns `list(Rank = "2", MaxRank = "3")`
- `.done` file written only after final rank completes

#### 2. Conditional Branching
- `{True: [...], False: [...]}` in InvokeNext
- Function must return exactly `TRUE` or `FALSE`
- Only selected branch is enqueued

#### 3. Invocation ID
- Accessible via `faasr_invocation_id()`
- Respects JSON configuration or generates timestamp-based ID

#### 4. Predecessor Gating
- Enforces synchronization for multiple unconditional predecessors
- Avoids deadlocks from conditional branches
- Uses `.done` marker files

#### 5. Clean State
- Clears `temp/` and `files/` at startup
- Isolated per-function working directories

### R Tips (for newcomers)
- **Lists**: `list(a=1, b=2)` creates named list; `list[[1]]` extracts first element
- **Queue operations**: `queue[[1]]` gets head; `queue[-1]` returns tail
- **Dynamic calls**: `get("func_name", envir=.GlobalEnv)` retrieves function by name; `do.call(f, args)` calls it
- **Error handling**: `try(expr, silent=TRUE)` captures errors; `inherits(x, "try-error")` checks for errors
- **Environments**: `assign(name, value, envir=.GlobalEnv)` sets globals; `get0(name, envir, ifnotfound=NULL)` reads them
- **Regex**: `regexec()` finds pattern matches; `regmatches()` extracts matched strings
- **Working directory**: `on.exit(setwd(orig), add=TRUE)` ensures cleanup on function exit

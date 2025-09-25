## faasr_test.R — Detailed Walkthrough

### Overview
- Purpose: Load a FaaSr workflow JSON and execute functions in InvokeNext order locally.
- Key features:
  - Sources user functions from `faasr_data/R`.
  - One-time validation: schema + cycle/reachability.
  - Per-node predecessor gating via `.done` markers.
  - Per-node working directories under `faasr_data/temp/<node>`.

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

### 2) Source user functions
Loads all `.R` files from `faasr_data/R` into the global environment so they can be invoked by name.

```r
src_dirs <- c(file.path("faasr_data", "R"))
for (d in src_dirs) {
  if (dir.exists(d)) {
    rfiles <- list.files(d, pattern = "\\.R$", full.names = TRUE)
    for (f in rfiles) {
      try(source(f, local = .GlobalEnv), silent = TRUE)
    }
  }
}
```

### 3) Queue and working directories
Initializes a simple FIFO queue and ensures the local workspace folders exist.

```r
queue <- c(wf$FunctionInvoke)

faasr_data_wd <- file.path(getwd(), "faasr_data")
if (!dir.exists(faasr_data_wd)) dir.create(faasr_data_wd, recursive = TRUE)

temp_dir  <- file.path(faasr_data_wd, "temp")
state_dir <- file.path(temp_dir, "faasr_state_info")
files_dir <- file.path(faasr_data_wd, "files")
for (p in c(temp_dir, state_dir, files_dir)) if (!dir.exists(p)) dir.create(p, recursive = TRUE)
```

### 4) Schema handling and one-time global validation
Downloads a fallback schema if needed, then validates the workflow once for JSON schema and graph correctness (cycles/unreachables).

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
  if (is.null(faasr$ActionList) || is.null(faasr$FunctionInvoke)) return("JSON parsing error")

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
      if (!inherits(ok, "try-error") && isFALSE(ok)) return("JSON parsing error")
    }
  }

  # Cycle + reachability check (DFS)
  graph_ok <- try(.faasr_check_workflow_cycle_local(faasr, faasr$FunctionInvoke), silent = TRUE)
  if (inherits(graph_ok, "try-error")) return("cycle/unreachable faasr_state_info errors")

  TRUE
}
```

Cycle/reachability uses helpers:

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
          if (!is.null(item$True))  next_names <- c(next_names, sub("\\(.*$", "", unlist(item$True)))
          if (!is.null(item$False)) next_names <- c(next_names, sub("\\(.*$", "", unlist(item$False)))
        }
      }
    }
    if (length(next_names)) adj[[nm]] <- unique(next_names)
  }
  adj
}
```

```r
.faasr_check_workflow_cycle_local <- function(faasr, start_node) {
  action_list <- faasr$ActionList
  adj <- .faasr_build_adjacency(action_list)

  visited <- new.env(parent = emptyenv())
  stack   <- new.env(parent = emptyenv())

  is_cyclic <- function(node) {
    if (isTRUE(get0(node, envir = stack, ifnotfound = FALSE))) return(TRUE)
    assign(node, TRUE, envir = stack)
    assign(node, TRUE, envir = visited)
    for (nbr in adj[[node]] %||% character()) {
      if (!isTRUE(get0(nbr, envir = visited, ifnotfound = FALSE))) {
        if (is_cyclic(nbr)) return(TRUE)
      } else if (isTRUE(get0(nbr, envir = stack, ifnotfound = FALSE))) {
        return(TRUE)
      }
    }
    assign(node, FALSE, envir = stack)
    FALSE
  }

  if (is_cyclic(start_node)) stop("cycle detected")

  unreachable <- setdiff(names(action_list), ls(visited))
  if (length(unreachable)) stop(sprintf("unreachable actions: %s", paste(unreachable, collapse = ", ")))
  TRUE
}
```

### 5) Main execution loop
Dequeues nodes, enforces predecessor gating, then executes the user function in an isolated directory.

```r
visited <- character()
while (length(queue) > 0) {
  name <- queue[1]; queue <- queue[-1]
  if (name %in% visited) next
  node <- wf$ActionList[[name]]
  if (is.null(node)) next

  func_name <- node$FunctionName
  args <- node$Arguments %||% list()
  cli::cli_h2(sprintf("Running %s (%s)", name, func_name))

  # Predecessor gating (skip until all predecessors are done)
  cfg <- faasr_predecessor_gate(wf$ActionList, name, state_dir)
  if (identical(cfg, "next")) { cli::cli_alert_info("Skipping execution; waiting for predecessors"); next }
  if (!identical(cfg, TRUE)) stop(cfg)

  # Ensure function exists and set up per-node working directory
  if (!exists(func_name, mode = "function", envir = .GlobalEnv)) stop(sprintf("Function not found: %s", func_name))
  run_dir <- file.path(temp_dir, name)
  if (!dir.exists(run_dir)) dir.create(run_dir, recursive = TRUE)
  orig_wd <- getwd(); on.exit(setwd(orig_wd), add = TRUE)
  Sys.setenv(FAASR_DATA_ROOT = faasr_data_wd)
  setwd(run_dir)

  # Execute user function
  f <- get(func_name, envir = .GlobalEnv)
  res <- try(do.call(f, args), silent = TRUE)
  if (inherits(res, "try-error")) { cli::cli_alert_danger(as.character(res)); stop(sprintf("Error executing %s", func_name)) }

  # Mark success for gating
  utils::write.table("TRUE", file = file.path(state_dir, paste0(name, ".done")), row.names = FALSE, col.names = FALSE)

  visited <- c(visited, name)

  # Enqueue next (strings or conditional True/False lists)
  nexts <- node$InvokeNext %||% list()
  if (is.character(nexts)) nexts <- as.list(nexts)
  for (nx in nexts) {
    if (is.character(nx)) {
      queue <- c(queue, sub("\\(.*$", "", nx))
    } else if (is.list(nx)) {
      branch <- if (isTRUE(res) && !is.null(nx$True)) nx$True else if (identical(res, FALSE) && !is.null(nx$False)) nx$False else NULL
      if (!is.null(branch)) for (b in branch) queue <- c(queue, sub("\\(.*$", "", b))
    }
  }
}
```

### 6) Helper: Null-coalescing operator
Convenience to default NULL to a value.

```r
`%||%` <- function(x, y) if (is.null(x)) y else x
```

### 7) Helper: Predecessor discovery and gating
Finds predecessors of a node by scanning all `InvokeNext` values; gating ensures all predecessor `.done` files exist before running.

```r
.faasr_find_predecessors <- function(action_list, target_func) {
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
          if (!is.null(item$True))  next_names <- c(next_names, sub("\\(.*$", "", unlist(item$True)))
          if (!is.null(item$False)) next_names <- c(next_names, sub("\\(.*$", "", unlist(item$False)))
        }
      }
    }
    if (length(next_names) && any(next_names == target_func)) preds <- c(preds, nm)
  }
  unique(preds)
}

faasr_predecessor_gate <- function(action_list, current_func, state_dir) {
  predecessors <- .faasr_find_predecessors(action_list, current_func)
  if (length(predecessors) > 1) {
    done_list <- try(list.files(state_dir), silent = TRUE)
    if (inherits(done_list, "try-error")) done_list <- character()
    for (p in predecessors) if (!(paste0(p, ".done") %in% done_list)) return("next")
  }
  TRUE
}
```

### R Tips (for newcomers)
- Vectors and lists: `c(...)` concatenates vectors; `list(...)` builds heterogeneous containers.
- Name resolution and calls: `exists(name, mode="function", envir=.GlobalEnv)`, `get(name, envir=.GlobalEnv)`, `do.call(func, args)`.
- Errors: `try(expr, silent=TRUE)` captures errors as values; check with `inherits(x, "try-error")`.
- Working directory safety: `orig <- getwd(); on.exit(setwd(orig), add=TRUE)` ensures cleanup.
- File markers: writing `<node>.done` files allows cheap cross-step synchronization.



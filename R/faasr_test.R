#' faasr_test: Run a workflow locally and sequentially (R), with optional Docker
#'
#' This function loads a FaaSr workflow JSON and executes functions sequentially
#' in InvokeNext order (no concurrency). It sources user functions from common
#' local locations (./R, ./functions). Docker mode temporarily disabled.
#'
#' @param json_path path to workflow JSON
#' @return TRUE if all functions run successfully; stops on error
#' @import jsonlite
#' @import cli
#' @export
faasr_test <- function(json_path) {
  if (!file.exists(json_path)) stop(sprintf("Workflow JSON not found: %s", json_path))
  wf <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)
  if (is.null(wf$ActionList)) stop("Invalid workflow JSON: missing ActionList")
  if (is.null(wf$FunctionInvoke)) stop("Invalid workflow JSON: missing FunctionInvoke")
  
  # Docker mode removed for now; always run locally


  # Source user R files from common locations
  src_dirs <- c("R", "functions", ".", file.path("faasr_data", "R"))
  for (d in src_dirs) {
    if (dir.exists(d)) {
      rfiles <- list.files(d, pattern = "\\.R$", full.names = TRUE)
      for (f in rfiles) {
        try(source(f, local = .GlobalEnv), silent = TRUE)
      }
    }
  }

  # Build a simple queue for sequential BFS-like execution.

  queue <- c(wf$FunctionInvoke)

  # Prepare faasr_data folders to mirror original output structure
  faasr_data_wd <- file.path(getwd(), "faasr_data")
  if (!dir.exists(faasr_data_wd)) dir.create(faasr_data_wd, recursive = TRUE)
  temp_dir <- file.path(faasr_data_wd, "temp")
  state_dir <- file.path(temp_dir, "faasr_state_info")
  files_dir <- file.path(faasr_data_wd, "files")
  if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
  if (!dir.exists(state_dir)) dir.create(state_dir, recursive = TRUE)
  if (!dir.exists(files_dir)) dir.create(files_dir, recursive = TRUE)

  # Schema path preference: local repo schema.json, else temp schema
  schema_repo_path <- file.path(getwd(), "schema.json")
  schema_temp_path <- file.path(temp_dir, "FaaSr.schema.json")
  if (!file.exists(schema_temp_path)) {
    schema_url <- "https://raw.githubusercontent.com/FaaSr/FaaSr-package/main/schema/FaaSr.schema.json"
    try(utils::download.file(schema_url, schema_temp_path, quiet = TRUE), silent = TRUE)
  }

  visited <- character()
  while (length(queue) > 0) {
    name <- queue[1]; queue <- queue[-1]
    if (name %in% visited) next
    node <- wf$ActionList[[name]]
    if (is.null(node)) next

    func_name <- node$FunctionName
    args <- node$Arguments %||% list()

    cli::cli_h2(sprintf("Running %s (%s)", name, func_name))

    # Configuration check prior to execution
    cfg <- faasr_configuration_check(wf, name, state_dir)
    if (identical(cfg, "next")) {
      cli::cli_alert_info("Skipping execution; waiting for predecessors")
      next
    }
    if (!identical(cfg, TRUE)) {
      stop(cfg)
    }
    # Local R execution with original-style working directory and state marker
    if (!exists(func_name, mode = "function", envir = .GlobalEnv)) {
      stop(sprintf("Function not found in environment: %s (node %s)", func_name, name))
    }
    run_dir <- file.path(temp_dir, name)
    if (!dir.exists(run_dir)) dir.create(run_dir, recursive = TRUE)
    orig_wd <- getwd()
    on.exit(setwd(orig_wd), add = TRUE)
    # Fix the data root so faasr_* uses faasr_data/files regardless of WD
    Sys.setenv(FAASR_DATA_ROOT = faasr_data_wd)
    setwd(run_dir)

    f <- get(func_name, envir = .GlobalEnv)
    res <- try(do.call(f, args), silent = TRUE)
    if (inherits(res, "try-error")) {
      cli::cli_alert_danger(as.character(res))
      stop(sprintf("Error executing %s", func_name))
    }
    # Mark success similar to package's .done files
    utils::write.table("TRUE", file = file.path(state_dir, paste0(name, ".done")), row.names = FALSE, col.names = FALSE)

    visited <- c(visited, name)
    # enqueue next
    nexts <- node$InvokeNext %||% list()
    # flatten possible list
    if (is.character(nexts)) nexts <- as.list(nexts)
    for (nx in nexts) {
      if (is.character(nx)) {
        queue <- c(queue, sub("\\(.*$", "", nx))
      } else if (is.list(nx)) {
        # if conditional object {True:[], False:[]}, default to True path when res is TRUE
        branch <- if (isTRUE(res) && !is.null(nx$True)) nx$True else if (identical(res, FALSE) && !is.null(nx$False)) nx$False else NULL
        if (is.null(branch)) next
        for (b in branch) queue <- c(queue, sub("\\(.*$", "", b))
      }
    }
  }

  cli::cli_alert_success("Workflow completed (R-only, sequential)")
  TRUE
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# Simple configuration check mirroring the package's behavior
faasr_configuration_check <- function(faasr, current_func, state_dir) {
  # Basic JSON sanity
  if (is.null(faasr$ActionList) || is.null(faasr$FunctionInvoke)) {
    return("JSON parsing error")
  }

  # JSON schema validation if jsonvalidate is available
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

  # Workflow cycle and reachability check (DFS on ActionList graph)
  graph_ok <- try(.faasr_check_workflow_cycle_local(faasr, faasr$FunctionInvoke), silent = TRUE)
  if (inherits(graph_ok, "try-error")) {
    return("cycle/unreachable faasr_state_info errors")
  }

  # Data store endpoint checks
  if (!is.null(faasr$DataStores)) {
    for (datastore in names(faasr$DataStores)) {
      endpoint <- faasr$DataStores[[datastore]]$Endpoint
      if ((!is.null(endpoint)) && (nzchar(endpoint)) && !startsWith(endpoint, "https")) {
        return("data store errors")
      }
    }
  }

  # default/logging data store presence
  if (!is.null(faasr$DefaultDataStore)) {
    if (is.null(faasr$DataStores) || !faasr$DefaultDataStore %in% names(faasr$DataStores)) {
      return("default server errors")
    }
  }
  if (!is.null(faasr$LoggingDataStore)) {
    if (is.null(faasr$DataStores) || !faasr$LoggingDataStore %in% names(faasr$DataStores)) {
      return("logging server errors")
    }
  }

  # Predecessor gating: if multiple predecessors, require all .done
  predecessors <- .faasr_find_predecessors(faasr$ActionList, current_func)
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

.faasr_find_predecessors <- function(action_list, target_func) {
  preds <- character()
  for (nm in names(action_list)) {
    nx <- action_list[[nm]]$InvokeNext %||% list()
    # normalize to character vector of names
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
    if (length(next_names) && any(next_names == target_func)) {
      preds <- c(preds, nm)
    }
  }
  unique(preds)
}

# Build adjacency list from ActionList using InvokeNext references
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

# Detect cycles and unreachable nodes starting from FunctionInvoke
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

  # reachability: ensure all nodes are visited
  all_nodes <- names(action_list)
  visited_nodes <- ls(visited)
  unreachable <- setdiff(all_nodes, visited_nodes)
  if (length(unreachable)) {
    stop(sprintf("unreachable actions: %s", paste(unreachable, collapse = ", ")))
  }

  TRUE
}

#' faasr_test: Run an R workflow locally 
#'
#' This function loads a FaaSr workflow JSON and executes functions sequentially
#' in InvokeNext order
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
  

  # Source user and local API R files
  src_dirs <- c(file.path("faasr_data", "R"), "R")
  for (d in src_dirs) {
    if (dir.exists(d)) {
      rfiles <- list.files(d, pattern = "\\.R$", full.names = TRUE)
      for (f in rfiles) {
        try(source(f, local = .GlobalEnv), silent = TRUE)
      }
    }
  }

  # Initialize global environment for rank info
  assign("FAASR_CURRENT_RANK_INFO", NULL, envir = .GlobalEnv)

  # Build a simple queue for the functions in the workflow
  # Each item is a list with: name, rank_current, rank_max
  queue <- list(list(name = wf$FunctionInvoke, rank_current = 1, rank_max = 1))

  # Prepare faasr_data folders 
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

  # Schema path preference: local repo schema.json, else temp schema
  schema_repo_path <- file.path(getwd(), "schema.json")
  schema_temp_path <- file.path(temp_dir, "FaaSr.schema.json")
  if (!file.exists(schema_temp_path)) {
    schema_url <- "https://raw.githubusercontent.com/FaaSr/FaaSr-package/main/schema/FaaSr.schema.json"
    try(utils::download.file(schema_url, schema_temp_path, quiet = TRUE), silent = TRUE)
  }
  # Go through the functions in the workflow
  # One-time global configuration validation: schema + cycle detection
  cfg_once <- faasr_configuration_check(wf, state_dir)
  if (!identical(cfg_once, TRUE)) {
    stop(cfg_once)
  }

  visited <- character()
  while (length(queue) > 0) {

    #Get the next function in the queue
    queue_item <- queue[[1]]; queue <- queue[-1]
    name <- queue_item$name
    rank_current <- queue_item$rank_current
    rank_max <- queue_item$rank_max
    
    # Create unique visited key including rank
    visited_key <- paste0(name, "_rank_", rank_current, "/", rank_max)
    if (visited_key %in% visited) next
    
    node <- wf$ActionList[[name]]
    if (is.null(node)) next

    #Get the function name and arguments
    func_name <- node$FunctionName
    args <- node$Arguments %||% list()

    # Predecessor gating prior to execution 
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
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# Simple configuration check mirroring the package's behavior
faasr_configuration_check <- function(faasr, state_dir) {
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

  # Workflow cycle check (DFS on ActionList graph)
  graph_ok <- try(.faasr_check_workflow_cycle_local(faasr, faasr$FunctionInvoke), silent = TRUE)
  if (inherits(graph_ok, "try-error")) {
    return("cycle errors")
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

# Predecessor gating: if multiple predecessors, require all .done present
faasr_predecessor_gate <- function(action_list, current_func, state_dir) {
  predecessors <- .faasr_find_predecessors(action_list, current_func)
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

# Detect cycles starting from FunctionInvoke
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

# Parse InvokeNext string to extract function name and rank
# Accepted formats only: "FuncName" or "FuncName(N)"
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

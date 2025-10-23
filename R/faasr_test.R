#' @name .faasr_write_rank_info
#' @title Write rank information to temporary file
#' @description
#' Internal function to write rank information to a temporary file for the current execution context.
#' @param rank_info Character string rank information in format "current/max" or NULL
#' @param state_dir Character string path to the state directory
#' @return Invisibly returns TRUE on success
#' @keywords internal
.faasr_write_rank_info <- function(rank_info, state_dir) {
  rank_file <- file.path(state_dir, "current_rank_info.txt")
  if (is.null(rank_info)) {
    if (file.exists(rank_file)) unlink(rank_file)
  } else {
    writeLines(rank_info, rank_file)
  }
  invisible(TRUE)
}

#' @name .faasr_write_invocation_id
#' @title Write invocation ID to temporary file
#' @description
#' Internal function to write invocation ID to a temporary file for the current execution context.
#' @param invocation_id Character string invocation ID
#' @param state_dir Character string path to the state directory
#' @return Invisibly returns TRUE on success
#' @keywords internal
.faasr_write_invocation_id <- function(invocation_id, state_dir) {
  inv_file <- file.path(state_dir, "current_invocation_id.txt")
  writeLines(invocation_id, inv_file)
  invisible(TRUE)
}

#' @name faasr_test
#' @title faasr_test
#' @description
#' This function loads a FaaSr workflow JSON and executes functions sequentially
#' in InvokeNext order. It provides a local testing environment for FaaSr workflows
#' without requiring cloud infrastructure.
#' @param json_path path to workflow JSON
#' @return TRUE if all functions run successfully; stops on error
#' @import jsonlite
#' @import cli
#' @importFrom utils download.file write.table
#' @importFrom uuid UUIDgenerate
#' @export
#' @examples
#' \dontrun{
#' faasr_test("workflow.json")
#' }
faasr_test <- function(json_path) {
  if (!file.exists(json_path)) stop(sprintf("Workflow JSON not found: %s", json_path))
  
  # Parse JSON with better error handling
  wf <- try(jsonlite::fromJSON(json_path, simplifyVector = FALSE), silent = TRUE)
  if (inherits(wf, "try-error")) {
    stop(sprintf("JSON parsing error in %s: %s", json_path, as.character(wf)))
  }
  
  # Validate required fields with specific error messages
  if (is.null(wf$ActionList)) stop("Invalid workflow JSON: missing required field 'ActionList'")
  if (is.null(wf$FunctionInvoke)) stop("Invalid workflow JSON: missing required field 'FunctionInvoke'")
  
  # Validate ActionList structure
  if (!is.list(wf$ActionList)) stop("Invalid workflow JSON: 'ActionList' must be an object")
  if (length(wf$ActionList) == 0) stop("Invalid workflow JSON: 'ActionList' cannot be empty")
  
  # Validate each action in ActionList
  for (action_name in names(wf$ActionList)) {
    action <- wf$ActionList[[action_name]]
    if (!is.list(action)) {
      stop(sprintf("Invalid workflow JSON: action '%s' must be an object", action_name))
    }
    if (is.null(action$FunctionName) || !is.character(action$FunctionName) || nchar(action$FunctionName) == 0) {
      stop(sprintf("Invalid workflow JSON: action '%s' is missing required field 'FunctionName'", action_name))
    }
    if (is.null(action$FaaSServer) || !is.character(action$FaaSServer) || nchar(action$FaaSServer) == 0) {
      stop(sprintf("Invalid workflow JSON: action '%s' is missing required field 'FaaSServer'", action_name))
    }
    if (is.null(action$Type) || !is.character(action$Type) || nchar(action$Type) == 0) {
      stop(sprintf("Invalid workflow JSON: action '%s' is missing required field 'Type'", action_name))
    }
  }
  

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
  
  # Generate and write invocation ID to temporary file
  invocation_id <- .faasr_generate_invocation_id(wf)
  .faasr_write_invocation_id(invocation_id, state_dir)
  
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

    # Set rank info in temporary file before execution
    if (rank_max > 1) {
      rank_info <- paste0(rank_current, "/", rank_max)
      .faasr_write_rank_info(rank_info, state_dir)
      cli::cli_h2(sprintf("Running %s (%s) - Rank %s", name, func_name, rank_info))
    } else {
      .faasr_write_rank_info(NULL, state_dir)
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

  # Clean up temporary files
  rank_file <- file.path(state_dir, "current_rank_info.txt")
  inv_file <- file.path(state_dir, "current_invocation_id.txt")
  if (file.exists(rank_file)) unlink(rank_file)
  if (file.exists(inv_file)) unlink(inv_file)
  
  cli::cli_alert_success("Workflow completed")
  TRUE
}

#' @name %||%
#' @title Default value operator
#' @description
#' Internal operator that returns the second argument if the first is NULL.
#' @param x First argument to check
#' @param y Default value to return if x is NULL
#' @return x if not NULL, otherwise y
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

#' @name faasr_configuration_check
#' @title Check FaaSr workflow configuration
#' @description
#' Validates a FaaSr workflow configuration for basic sanity, JSON schema compliance,
#' and cycle detection. This function mirrors the behavior of the production FaaSr package.
#' @param faasr List containing the parsed workflow configuration
#' @param state_dir Character string path to the state directory
#' @return TRUE if configuration is valid, error message string otherwise
#' @keywords internal
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

  # Predecessor type consistency check
  pred_consistency <- try(.faasr_check_predecessor_consistency(faasr$ActionList), silent = TRUE)
  if (inherits(pred_consistency, "try-error")) {
    return(as.character(pred_consistency))
  }

  TRUE
}

#' @name .faasr_check_predecessor_consistency
#' @title Check predecessor type consistency in workflow
#' @description
#' Internal function to validate that all predecessors of each function are of the same type.
#' Predecessors must be either all unconditional, all from the same conditional source,
#' or no predecessors (starting node).
#' @param action_list List containing the workflow action definitions
#' @return TRUE if consistent, stops with error if inconsistent
#' @keywords internal
.faasr_check_predecessor_consistency <- function(action_list) {
  for (target_func in names(action_list)) {
    predecessors <- .faasr_find_predecessors_with_types(action_list, target_func)
    
    if (length(predecessors) == 0) {
      # No predecessors - this is valid (starting node)
      next
    }
    
    # Check if all predecessors are of the same type
    pred_types <- sapply(predecessors, function(p) p$type)
    unique_types <- unique(pred_types)
    
    if (length(unique_types) > 1) {
      # Mixed predecessor types - this is invalid
      unconditional_preds <- predecessors[pred_types == "unconditional"]
      conditional_preds <- predecessors[pred_types == "conditional"]
      
      error_msg <- sprintf("Function '%s' has mixed predecessor types:\n", target_func)
      
      if (length(unconditional_preds) > 0) {
        unconditional_names <- sapply(unconditional_preds, function(p) p$name)
        error_msg <- paste0(error_msg, "  - Unconditional: ", paste(unconditional_names, collapse = ", "), "\n")
      }
      
      if (length(conditional_preds) > 0) {
        conditional_sources <- unique(sapply(conditional_preds, function(p) p$source))
        for (source in conditional_sources) {
          source_preds <- conditional_preds[sapply(conditional_preds, function(p) p$source == source)]
          source_names <- sapply(source_preds, function(p) p$name)
          source_branches <- unique(sapply(source_preds, function(p) p$branch))
          error_msg <- paste0(error_msg, "  - Conditional from '", source, "' (", paste(source_branches, collapse = ", "), "): ", paste(source_names, collapse = ", "), "\n")
        }
      }
      
      error_msg <- paste0(error_msg, "\nAll predecessors must be either:\n")
      error_msg <- paste0(error_msg, "1. All unconditional edges, OR\n")
      error_msg <- paste0(error_msg, "2. All from the same conditional source (same action, same or different branches), OR\n")
      error_msg <- paste0(error_msg, "3. No predecessors (starting node)")
      
      stop(error_msg)
    }
  }
  
  TRUE
}

#' @name .faasr_find_predecessors_with_types
#' @title Find predecessor functions with their types in workflow
#' @description
#' Internal function to find all predecessor functions for a given target function
#' and categorize them by type (unconditional vs conditional).
#' @param action_list List containing the workflow action definitions
#' @param target_func Character string name of the target function
#' @return List of predecessor information with name, type, source, and branch
#' @keywords internal
.faasr_find_predecessors_with_types <- function(action_list, target_func) {
  predecessors <- list()
  
  for (nm in names(action_list)) {
    nx <- action_list[[nm]]$InvokeNext %||% list()
    
    if (is.character(nx)) {
      # Unconditional edge
      next_names <- sub("\\(.*$", "", nx)
      if (any(next_names == target_func)) {
        predecessors <- c(predecessors, list(list(
          name = nm,
          type = "unconditional",
          source = nm,
          branch = NA
        )))
      }
    } else if (is.list(nx)) {
      for (item in nx) {
        if (is.character(item)) {
          # Unconditional edge in list
          next_names <- sub("\\(.*$", "", item)
          if (any(next_names == target_func)) {
            predecessors <- c(predecessors, list(list(
              name = nm,
              type = "unconditional",
              source = nm,
              branch = NA
            )))
          }
        } else if (is.list(item)) {
          # Conditional edge
          if (!is.null(item$True)) {
            true_names <- sub("\\(.*$", "", unlist(item$True))
            if (any(true_names == target_func)) {
              predecessors <- c(predecessors, list(list(
                name = nm,
                type = "conditional",
                source = nm,
                branch = "True"
              )))
            }
          }
          if (!is.null(item$False)) {
            false_names <- sub("\\(.*$", "", unlist(item$False))
            if (any(false_names == target_func)) {
              predecessors <- c(predecessors, list(list(
                name = nm,
                type = "conditional",
                source = nm,
                branch = "False"
              )))
            }
          }
        }
      }
    }
  }
  
  # Remove duplicates based on name
  unique_predecessors <- list()
  seen_names <- character()
  for (pred in predecessors) {
    if (!(pred$name %in% seen_names)) {
      unique_predecessors <- c(unique_predecessors, list(pred))
      seen_names <- c(seen_names, pred$name)
    }
  }
  
  unique_predecessors
}

# IMPROVED VERSION 
# This version only gates on unconditional predecessors to prevent deadlocks
# when a function appears in both conditional and unconditional paths.
# Example deadlock scenario:
#   funcA --True--> funcB
#     |
#     +----False--> funcC
#   funcB ---------> funcC
# If funcA returns FALSE, funcC should run but would deadlock waiting for funcB.done
#
# .faasr_find_predecessors <- function(action_list, target_func, unconditional_only = FALSE) {
#   preds <- character()
#   for (nm in names(action_list)) {
#     nx <- action_list[[nm]]$InvokeNext %||% list()
#     next_names <- character()
#     is_conditional <- FALSE
#     if (is.character(nx)) {
#       next_names <- sub("\\(.*$", "", nx)
#     } else if (is.list(nx)) {
#       for (item in nx) {
#         if (is.character(item)) {
#           next_names <- c(next_names, sub("\\(.*$", "", item))
#         } else if (is.list(item)) {
#           is_conditional <- TRUE
#           if (!unconditional_only) {
#             if (!is.null(item$True)) next_names <- c(next_names, sub("\\(.*$", "", unlist(item$True)))
#             if (!is.null(item$False)) next_names <- c(next_names, sub("\\(.*$", "", unlist(item$False)))
#           }
#         }
#       }
#     }
#     if (length(next_names) && any(next_names == target_func)) {
#       preds <- c(preds, nm)
#     }
#   }
#   unique(preds)
# }
#
# faasr_predecessor_gate <- function(action_list, current_func, state_dir) {
#   predecessors <- .faasr_find_predecessors(action_list, current_func, unconditional_only = TRUE)
#   if (length(predecessors) > 1) {
#     done_list <- try(list.files(state_dir), silent = TRUE)
#     if (inherits(done_list, "try-error")) done_list <- character()
#     for (p in predecessors) {
#       if (!(paste0(p, ".done") %in% done_list)) {
#         return("next")
#       }
#     }
#   }
#   TRUE
# }

#' @name .faasr_find_predecessors
#' @title Find predecessor functions in workflow
#' @description
#' Internal function to find all predecessor functions for a given target function
#' in the workflow action list. This matches the original package behavior but may
#' deadlock with conditional branches.
#' @param action_list List containing the workflow action definitions
#' @param target_func Character string name of the target function
#' @return Character vector of predecessor function names
#' @keywords internal
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
          # Include ALL branches (both True and False)
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

#' @name faasr_predecessor_gate
#' @title Gate function execution based on predecessors
#' @description
#' Internal function to check if all predecessor functions have completed before
#' allowing the current function to execute. Uses .done files to track completion.
#' @param action_list List containing the workflow action definitions
#' @param current_func Character string name of the current function
#' @param state_dir Character string path to the state directory
#' @return "next" if predecessors not ready, TRUE if ready to execute
#' @keywords internal
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

#' @name .faasr_build_adjacency
#' @title Build adjacency list from ActionList
#' @description
#' Internal function to build an adjacency list representation of the workflow
#' graph from the ActionList using InvokeNext references.
#' @param action_list List containing the workflow action definitions
#' @return List where each element contains the names of functions that can be invoked next
#' @keywords internal
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

#' @name .faasr_check_workflow_cycle_local
#' @title Detect cycles in workflow graph
#' @description
#' Internal function to detect cycles in the workflow graph using depth-first search.
#' Workflows must be acyclic (DAG) to be valid.
#' @param faasr List containing the parsed workflow configuration
#' @param start_node Character string name of the starting function
#' @return TRUE if no cycles detected, stops with error if cycle found
#' @keywords internal
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

#' @name .faasr_parse_invoke_next_string
#' @title Parse InvokeNext string to extract function name and rank
#' @description
#' Internal function to parse InvokeNext strings that may contain rank notation.
#' Supports formats like "FunctionName" and "FunctionName(3)" for parallel execution.
#' @param invoke_string Character string to parse
#' @return List containing func_name, condition, and rank
#' @keywords internal
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

#' @name .faasr_generate_invocation_id
#' @title Generate invocation ID based on workflow configuration
#' @description
#' Internal function to generate a unique invocation ID for the workflow execution.
#' Priority: 1) Use InvocationID if provided, 2) Use InvocationIDFromDate if valid, 3) Generate UUID
#' @param wf List containing the parsed workflow configuration
#' @return Character string invocation ID
#' @keywords internal
.faasr_generate_invocation_id <- function(wf) {
  # Priority 1: Check if InvocationID is already set in the workflow
  if (!is.null(wf$InvocationID) && nzchar(trimws(wf$InvocationID))) {
    return(trimws(wf$InvocationID))
  }
  
  # Priority 2: Check if InvocationIDFromDate format is specified and valid
  if (!is.null(wf$InvocationIDFromDate) && nzchar(trimws(wf$InvocationIDFromDate))) {
    date_format <- trimws(wf$InvocationIDFromDate)
    # Validate the date format by checking if it contains valid format specifiers
    # Valid format specifiers are % followed by letters (Y, m, d, H, M, S, etc.)
    if (!grepl("^[%a-zA-Z0-9\\s:._-]+$", date_format) || !grepl("%[a-zA-Z]", date_format)) {
      stop(sprintf("Invalid InvocationIDFromDate format '%s': must contain valid date format specifiers (e.g., %%Y%%m%%d)", date_format))
    }
    
    # Try to use the format and validate the result
    tryCatch({
      test_result <- format(Sys.time(), date_format)
      if (nzchar(test_result) && test_result != date_format) {
        return(test_result)
      } else {
        stop("Invalid date format - no valid date specifiers found")
      }
    }, error = function(e) {
      stop(sprintf("Invalid InvocationIDFromDate format '%s': %s", date_format, e$message))
    })
  }
  
  # Priority 3: Generate UUID if both are blank or invalid
  if (requireNamespace("uuid", quietly = TRUE)) {
    return(uuid::UUIDgenerate())
  } else {
    # Fallback to timestamp-based ID if uuid package not available
    paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-", 
           paste(sample(c(0:9, letters[1:6]), 8, replace = TRUE), collapse = ""))
  }
}

#' @name faasr_test
#' @title FaaSr test execution
#' @description
#' Workflow execution that dynamically handles conditional branching
#' and predecessor dependencies without precomputation.
#' @param json_path path to workflow JSON
#' @return TRUE if all functions run successfully; stops on error
#' @export
faasr_test <- function(json_path) {
  if (!file.exists(json_path)) stop(sprintf("Workflow JSON not found: %s", json_path))

  # Parse JSON
  wf <- try(jsonlite::fromJSON(json_path, simplifyVector = FALSE), silent = TRUE)
  if (inherits(wf, "try-error")) {
    stop(sprintf("JSON parsing error in %s: %s", json_path, as.character(wf)))
  }

  # Validate required fields
  if (is.null(wf$ActionList)) stop("Invalid workflow JSON: missing required field 'ActionList'")
  if (is.null(wf$FunctionInvoke)) stop("Invalid workflow JSON: missing required field 'FunctionInvoke'")

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

  # Setup directories
  faasr_data_wd <- file.path(getwd(), "faasr_data")
  if (!dir.exists(faasr_data_wd)) dir.create(faasr_data_wd, recursive = TRUE)
  temp_dir <- file.path(faasr_data_wd, "temp")
  if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)
  state_dir <- file.path(temp_dir, "faasr_state_info")
  files_dir <- file.path(faasr_data_wd, "files")
  if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
  if (!dir.exists(state_dir)) dir.create(state_dir, recursive = TRUE)
  if (!dir.exists(files_dir)) dir.create(files_dir, recursive = TRUE)

  # Generate and write invocation ID
  invocation_id <- .faasr_generate_invocation_id(wf)
  .faasr_write_invocation_id(invocation_id, state_dir)

  # Clean files outputs
  files_to_remove <- try(list.files(files_dir, all.files = TRUE, full.names = TRUE,
                                    include.dirs = TRUE, recursive = FALSE), silent = TRUE)
  if (!inherits(files_to_remove, "try-error") && length(files_to_remove)) {
    base_names <- basename(files_to_remove)
    keep <- base_names %in% c(".", "..", ".gitkeep")
    to_delete <- files_to_remove[!keep]
    if (length(to_delete)) unlink(to_delete, recursive = TRUE, force = TRUE)
  }

  # Download schema if needed (only if not present at repo root)
  schema_repo_path <- file.path(getwd(), "schema.json")
  if (!file.exists(schema_repo_path)) {
    schema_url <- "https://raw.githubusercontent.com/FaaSr/FaaSr-Backend/main/FaaSr_py/FaaSr.schema.json"
    try(utils::download.file(schema_url, schema_repo_path, quiet = TRUE), silent = TRUE)
  }

  # Validate workflow configuration (schema validation + cycle detection)
  # Use simplified validation without predecessor consistency check
  cfg_check <- .faasr_configuration_check_simple(wf, state_dir)
  if (!identical(cfg_check, TRUE)) {
    stop(cfg_check)
  }

  # Build reverse dependency map: which functions does each function depend on?
  reverse_deps <- .faasr_build_reverse_deps(wf$ActionList)

  # Track completed functions (as sets of action_name + rank)
  completed <- character()

  # Track which actions have been enqueued (added to execution plan)
  # This helps us distinguish between "not yet executed" vs "never will execute"
  enqueued_actions <- character()

  # Queue of functions to execute: list(action_name, rank_current, rank_max)
  queue <- list(list(action_name = wf$FunctionInvoke, rank_current = 1, rank_max = 1))
  enqueued_actions <- c(enqueued_actions, wf$FunctionInvoke)

  # Main execution loop
  while (length(queue) > 0) {
    # Dequeue next item
    item <- queue[[1]]
    queue <- queue[-1]

    action_name <- item$action_name
    rank_current <- item$rank_current
    rank_max <- item$rank_max

    # Create unique key for this execution
    exec_key <- paste0(action_name, "_rank_", rank_current, "/", rank_max)

    # Skip if already executed
    if (exec_key %in% completed) next

    # Get action definition
    action <- wf$ActionList[[action_name]]
    if (is.null(action)) next

    # Check if all required predecessors have completed
    # Only wait for predecessors that have been enqueued
    ready <- .faasr_check_ready_simple(action_name, reverse_deps, completed, enqueued_actions)
    if (!ready) {
      # Not ready yet - add back to end of queue
      queue <- c(queue, list(item))
      next
    }

    # Execute the function
    func_name <- action$FunctionName
    args <- action$Arguments %||% list()

    # Set rank info
    if (rank_max > 1) {
      rank_info <- paste0(rank_current, "/", rank_max)
      .faasr_write_rank_info(rank_info, state_dir)
      cli::cli_h2(sprintf("Running %s (%s) - Rank %s", action_name, func_name, rank_info))
    } else {
      .faasr_write_rank_info(NULL, state_dir)
      cli::cli_h2(sprintf("Running %s (%s)", action_name, func_name))
    }

    # Check function exists
    if (!exists(func_name, mode = "function", envir = .GlobalEnv)) {
      stop(sprintf("Function not found in environment: %s (action %s)", func_name, action_name))
    }

    # Create isolated working directory
    run_dir <- file.path(temp_dir, action_name)
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

    # Mark this execution as complete
    completed <- c(completed, exec_key)

    # Mark action as done (only after final rank)
    if (rank_current == rank_max) {
      utils::write.table("TRUE", file = file.path(state_dir, paste0(action_name, ".done")),
                        row.names = FALSE, col.names = FALSE)

      # Enqueue successors based on InvokeNext
      nexts <- action$InvokeNext %||% list()
      if (is.character(nexts)) nexts <- as.list(nexts)

      for (nx in nexts) {
        if (is.character(nx)) {
          # Simple successor: "funcB" or "funcB(3)"
          parsed <- .faasr_parse_invoke_next_string(nx)
          # Mark as enqueued (only track action name, not ranks)
          if (!(parsed$func_name %in% enqueued_actions)) {
            enqueued_actions <- c(enqueued_actions, parsed$func_name)
          }
          for (r in 1:parsed$rank) {
            queue <- c(queue, list(list(
              action_name = parsed$func_name,
              rank_current = r,
              rank_max = parsed$rank
            )))
          }
        } else if (is.list(nx)) {
          # Conditional: {True: [...], False: [...]}
          # Only follow the branch that matches the result
          branch <- NULL
          if (isTRUE(res) && !is.null(nx$True)) {
            branch <- nx$True
          } else if (identical(res, FALSE) && !is.null(nx$False)) {
            branch <- nx$False
          }

          if (!is.null(branch)) {
            for (b in branch) {
              parsed <- .faasr_parse_invoke_next_string(b)
              # Mark as enqueued
              if (!(parsed$func_name %in% enqueued_actions)) {
                enqueued_actions <- c(enqueued_actions, parsed$func_name)
              }
              for (r in 1:parsed$rank) {
                queue <- c(queue, list(list(
                  action_name = parsed$func_name,
                  rank_current = r,
                  rank_max = parsed$rank
                )))
              }
            }
          }
        }
      }
    }
  }

  # Cleanup
  rank_file <- file.path(state_dir, "current_rank_info.txt")
  inv_file <- file.path(state_dir, "current_invocation_id.txt")
  if (file.exists(rank_file)) unlink(rank_file)
  if (file.exists(inv_file)) unlink(inv_file)

  cli::cli_alert_success("Workflow completed")
  TRUE
}

# ============================================================================
# Helper Functions
# ============================================================================

#' @name %||%
#' @title Default value operator
#' @description
#' Internal operator that returns the second argument if the first is NULL.
#' @param x First argument to check
#' @param y Default value to return if x is NULL
#' @return x if not NULL, otherwise y
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

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

# ============================================================================
# Reverse Dependency and Readiness Checking
# ============================================================================

#' @name .faasr_build_reverse_deps
#' @title Build reverse dependency map
#' @description
#' Builds a map showing which actions each action depends on (its predecessors).
#' Only includes UNCONDITIONAL dependencies - conditional branches are handled dynamically.
#' @param action_list List containing the workflow action definitions
#' @return Named list where each element is a character vector of predecessor action names
#' @keywords internal
.faasr_build_reverse_deps <- function(action_list) {
  reverse_deps <- list()

  # Initialize all actions with empty dependency list
  for (action_name in names(action_list)) {
    reverse_deps[[action_name]] <- character()
  }

  # Scan all actions to find unconditional InvokeNext edges
  for (action_name in names(action_list)) {
    nx <- action_list[[action_name]]$InvokeNext %||% list()

    if (is.character(nx)) {
      # All string successors are unconditional dependencies
      for (succ in nx) {
        succ_name <- sub("\\(.*$", "", succ)
        if (succ_name %in% names(reverse_deps)) {
          reverse_deps[[succ_name]] <- c(reverse_deps[[succ_name]], action_name)
        }
      }
    } else if (is.list(nx)) {
      for (item in nx) {
        if (is.character(item)) {
          # String items in list are unconditional
          succ_name <- sub("\\(.*$", "", item)
          if (succ_name %in% names(reverse_deps)) {
            reverse_deps[[succ_name]] <- c(reverse_deps[[succ_name]], action_name)
          }
        }
        # Skip list items (conditionals) - they're handled dynamically
      }
    }
  }

  # Remove duplicates
  for (action_name in names(reverse_deps)) {
    reverse_deps[[action_name]] <- unique(reverse_deps[[action_name]])
  }

  reverse_deps
}

#' @name .faasr_check_ready_simple
#' @title Check if action is ready to execute
#' @description
#' Checks if all required predecessors have completed by looking at the completed set.
#' Only waits for predecessors that have been enqueued (added to execution plan).
#' This handles conditional branching where some predecessors may never execute.
#' @param action_name Character string name of the action to check
#' @param reverse_deps Named list mapping actions to their predecessor actions
#' @param completed Character vector of completed execution keys
#' @param enqueued_actions Character vector of action names that have been enqueued
#' @return TRUE if ready to execute, FALSE otherwise
#' @keywords internal
.faasr_check_ready_simple <- function(action_name, reverse_deps, completed, enqueued_actions) {
  # Get predecessors for this action
  preds <- reverse_deps[[action_name]]

  # If no predecessors, ready to execute
  if (length(preds) == 0) return(TRUE)

  # Check if all predecessors that have been enqueued have completed
  # (Ignore predecessors that were never added to the execution plan due to conditionals)
  for (pred in preds) {
    # Skip if this predecessor was never enqueued (e.g., different conditional branch)
    if (!(pred %in% enqueued_actions)) next

    # Check if any rank of this predecessor has completed
    pred_completed <- any(grepl(paste0("^", pred, "_rank_"), completed))
    if (!pred_completed) {
      return(FALSE)
    }
  }

  TRUE
}

#' @name .faasr_configuration_check_simple
#' @title Simplified FaaSr workflow configuration check
#' @description
#' Validates a FaaSr workflow configuration for JSON schema compliance and cycle detection.
#' Unlike the full faasr_configuration_check, this does not check predecessor consistency
#' because the simplified execution approach handles mixed predecessors correctly.
#' @param faasr List containing the parsed workflow configuration
#' @param state_dir Character string path to the state directory
#' @return TRUE if configuration is valid, error message string otherwise
#' @keywords internal
.faasr_configuration_check_simple <- function(faasr, state_dir) {
  # Basic JSON sanity
  if (is.null(faasr$ActionList) || is.null(faasr$FunctionInvoke)) {
    return("JSON parsing error")
  }

  # JSON schema validation if jsonvalidate is available
  schema_file <- file.path(getwd(), "schema.json")
  if (file.exists(schema_file) && requireNamespace("jsonvalidate", quietly = TRUE)) {
    json_txt <- try(jsonlite::toJSON(faasr, auto_unbox = TRUE), silent = TRUE)
    if (!inherits(json_txt, "try-error")) {
      ok <- try(jsonvalidate::json_validate(json = json_txt, schema = schema_file, verbose = TRUE), silent = TRUE)
      if (!inherits(ok, "try-error") && isFALSE(ok)) {
        # Get detailed error message for debugging
        error_result <- try({
          jsonvalidate::json_validate(json = json_txt, schema = schema_file, 
                                     verbose = TRUE, error = TRUE)
        }, silent = TRUE)
        if (inherits(error_result, "try-error")) {
          # Extract the actual error message from the try-error object
          error_msg <- gsub("^Error.*?: ", "", as.character(error_result))
          return(paste0("JSON schema validation failed:\n", error_msg))
        }
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
  # While the enqueued tracking approach technically handles mixed predecessors
  # without deadlocking, mixing unconditional and conditional predecessors
  # creates ambiguous workflow semantics that should be flagged as an error
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
#' or no predecessors (starting node). Mixed predecessor types create ambiguous semantics.
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

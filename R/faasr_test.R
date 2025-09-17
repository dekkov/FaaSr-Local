#' faasr_test: Run a workflow locally and sequentially (R), with optional Docker
#'
#' This function loads a FaaSr workflow JSON and executes functions sequentially
#' in InvokeNext order (no concurrency). It sources user functions from common
#' local locations (./R, ./functions). Optionally, it can run each function in
#' a Docker container by bind mounting ./faasr_data to /faasr_data and passing
#' the full JSON payload to the container entrypoint (mirroring original faasr_test).
#'
#' @param json_path path to workflow JSON
#' @return TRUE if all functions run successfully; stops on error
#' @import jsonlite
#' @import cli
#' @export
docker_default_version <- "latest"
docker_default_image <- "ghcr.io/faasr/local-test"
faasr_test <- function(json_path) {
  if (!file.exists(json_path)) stop(sprintf("Workflow JSON not found: %s", json_path))
  wf <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)
  if (is.null(wf$ActionList)) stop("Invalid workflow JSON: missing ActionList")
  if (is.null(wf$FunctionInvoke)) stop("Invalid workflow JSON: missing FunctionInvoke")
  
  # Default to local mode; allow enabling Docker via env var
  use_docker <- list(
    use = FALSE,
    image = docker_default_image,
    version = docker_default_version
  )
  use_docker_env <- Sys.getenv("FAASR_LOCAL_TEST_USE_DOCKER", unset = "")
  if (tolower(use_docker_env) %in% c("1", "true", "yes")) {
    use_docker$use <- TRUE
  }


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

  visited <- character()
  while (length(queue) > 0) {
    name <- queue[1]; queue <- queue[-1]
    if (name %in% visited) next
    node <- wf$ActionList[[name]]
    if (is.null(node)) next

    func_name <- node$FunctionName
    args <- node$Arguments %||% list()

    cli::cli_h2(sprintf("Running %s (%s)", name, func_name))

    # Decide local R vs Docker
    if (isTRUE(use_docker$use)) {
      # Ensure faasr_data exists and copy local R/ into faasr_data/R for container to source
      faasr_data_wd <- file.path(getwd(), "faasr_data")
      if (!dir.exists(faasr_data_wd)) dir.create(faasr_data_wd, recursive = TRUE)
      r_host_dir <- file.path(faasr_data_wd, "R")
      if (!dir.exists(r_host_dir)) dir.create(r_host_dir, recursive = TRUE)
      if (dir.exists("R")) {
        rfiles <- list.files("R", pattern = "\\.R$", full.names = TRUE)
        for (rf in rfiles) file.copy(rf, file.path(r_host_dir, basename(rf)), overwrite = TRUE)
      }

      # Ensure temp/faasr_state_info exists (align with package test harness)
      temp_state_dir <- file.path(faasr_data_wd, "temp", "faasr_state_info")
      if (!dir.exists(temp_state_dir)) dir.create(temp_state_dir, recursive = TRUE)

      # Ensure schema exists in temp (align with package downloading behavior)
      schema_path <- file.path(faasr_data_wd, "temp", "FaaSr.schema.json")
      if (!file.exists(schema_path)) {
        schema_url <- "https://raw.githubusercontent.com/FaaSr/FaaSr-package/main/schema/FaaSr.schema.json"
        try(utils::download.file(schema_url, schema_path, quiet = TRUE), silent = TRUE)
      }

      # Determine image:version (use a single test image, like the package)
      image_tag <- paste0(use_docker$image, ":", use_docker$version)

      # Original style: set current function and pass full payload JSON as single arg
      wf$FunctionInvoke <- name
      faasr_input <- jsonlite::toJSON(wf, auto_unbox = TRUE)
      docker_cmd <- paste0(
        "docker run --rm --platform=linux/amd64 --name faasr-", name,
        " --mount type=bind,source='", faasr_data_wd, "',target='/faasr_data' ",
        image_tag, " '", faasr_input, "'"
      )

      res_lines <- try(system(docker_cmd, intern = TRUE, ignore.stderr = FALSE, ignore.stdout = FALSE), silent = TRUE)
      if (inherits(res_lines, "try-error")) {
        cli::cli_alert_danger(as.character(res_lines))
        stop(sprintf("Docker execution failed for %s", name))
      }
      # Interpret last line as TRUE/FALSE if present; otherwise treat as TRUE
      last <- if (length(res_lines)) res_lines[length(res_lines)] else "TRUE"
      res <- if (identical(last, "TRUE") || identical(last, TRUE)) TRUE else if (identical(last, "FALSE") || identical(last, FALSE)) FALSE else TRUE
    } else {
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
    }

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

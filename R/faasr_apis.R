#' Local filesystem implementations of faasr_* APIs
#' These mirror the signatures but operate on a local workspace folder.
#' The workspace root is 'faasr_data'; files live under 'faasr_data/files'.
#' Logs live under 'faasr_data/logs'.


.fa_local_root <- function() {
  env_root <- Sys.getenv("FAASR_DATA_ROOT", unset = "")
  if (nzchar(env_root)) {
    root <- env_root
  } else {
    root <- file.path(getwd(), "faasr_data")
  }
  if (!dir.exists(root)) dir.create(root, recursive = TRUE)
  root
}

.fa_files_root <- function() {
  files <- file.path(.fa_local_root(), "files")
  if (!dir.exists(files)) dir.create(files, recursive = TRUE)
  files
}

.fa_logs_root <- function() {
  logs <- file.path(.fa_local_root(), "logs")
  if (!dir.exists(logs)) dir.create(logs, recursive = TRUE)
  logs
}

#' Put (upload) a file to local storage
faasr_put_file <- function(server_name = NULL, local_folder = ".", local_file,
                           remote_folder = "", remote_file) {
  # Clean inputs
  local_folder <- sub("/+$", "", local_folder)
  local_folder <- if (nzchar(local_folder)) local_folder else "."
  src <- if (identical(local_folder, ".")) local_file else file.path(local_folder, local_file)
  if (!file.exists(src)) stop(sprintf("Local file not found: %s", src))

  remote_folder <- sub("^/+", "", sub("/+$", "", remote_folder))
  dest_dir <- if (nzchar(remote_folder)) file.path(.fa_files_root(), remote_folder) else .fa_files_root()
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
  dest <- file.path(dest_dir, remote_file)
  ok <- file.copy(src, dest, overwrite = TRUE)
  if (!ok) stop("Failed to put file")
  invisible(TRUE)
}

#' Get (download) a file from local storage
faasr_get_file <- function(server_name = NULL, remote_folder = "", remote_file,
                           local_folder = ".", local_file) {
  remote_folder <- sub("^/+", "", sub("/+$", "", remote_folder))
  src_dir <- if (nzchar(remote_folder)) file.path(.fa_files_root(), remote_folder) else .fa_files_root()
  src <- file.path(src_dir, remote_file)
  if (!file.exists(src)) stop(sprintf("Remote file not found: %s", file.path(remote_folder, remote_file)))

  local_folder <- sub("/+$", "", local_folder)
  local_folder <- if (nzchar(local_folder)) local_folder else "."
  if (!dir.exists(local_folder)) dir.create(local_folder, recursive = TRUE)
  dest <- if (identical(local_folder, ".")) local_file else file.path(local_folder, local_file)
  ok <- file.copy(src, dest, overwrite = TRUE)
  if (!ok) stop("Failed to get file")
  invisible(TRUE)
}

#' Delete a file from local storage
faasr_delete_file <- function(server_name = NULL, remote_folder = "", remote_file) {
  remote_folder <- sub("^/+", "", sub("/+$", "", remote_folder))
  src_dir <- if (nzchar(remote_folder)) file.path(.fa_files_root(), remote_folder) else .fa_files_root()
  target <- file.path(src_dir, remote_file)
  if (file.exists(target)) unlink(target)
  invisible(TRUE)
}

#' List files in local storage with optional prefix
faasr_get_folder_list <- function(server_name = NULL, faasr_prefix = "") {
  files <- list.files(.fa_files_root(), recursive = TRUE, all.files = FALSE)
  files <- files[!dir.exists(file.path(.fa_files_root(), files))]
  if (nzchar(faasr_prefix)) files <- files[startsWith(files, sub("^/+", "", faasr_prefix))]
  files
}

#' Append a log message to local logs
faasr_log <- function(log_message) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  logfile <- file.path(.fa_logs_root(), paste0("faasr_", format(Sys.time(), "%Y%m%d"), ".log"))
  cat(sprintf("[%s] %s\n", ts, log_message), file = logfile, append = TRUE)
  invisible(TRUE)
}

#' Get current rank information for the executing function
#' @return List with Rank and MaxRank, or empty list if not running in ranked mode
#' @export
faasr_rank <- function() {
  rank_info <- get0("FAASR_CURRENT_RANK_INFO", envir = .GlobalEnv, inherits = FALSE, ifnotfound = NULL)
  
  if (is.null(rank_info)) {
    return(list())
  }
  
  # Parse rank_info which is in format "1/3"
  parts <- unlist(strsplit(rank_info, "[/]"))
  if (length(parts) == 2) {
    return(list(Rank = parts[1], MaxRank = parts[2]))
  }
  
  return(list())
}

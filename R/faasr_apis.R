#' @name .fa_local_root
#' @title Get local FaaSr data root directory
#' @description
#' Internal function to get the root directory for local FaaSr data storage.
#' Uses the FAASR_DATA_ROOT environment variable if set, otherwise defaults to 'faasr_data' in the current working directory.
#' @return Character string path to the root directory
#' @keywords internal
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

#' @name .fa_files_root
#' @title Get local FaaSr files directory
#' @description
#' Internal function to get the files directory for local FaaSr data storage.
#' Creates the directory if it doesn't exist.
#' @return Character string path to the files directory
#' @keywords internal
.fa_files_root <- function() {
  files <- file.path(.fa_local_root(), "files")
  if (!dir.exists(files)) dir.create(files, recursive = TRUE)
  files
}

#' @name .fa_logs_root
#' @title Get local FaaSr logs directory
#' @description
#' Internal function to get the logs directory for local FaaSr data storage.
#' Creates the directory if it doesn't exist.
#' @return Character string path to the logs directory
#' @keywords internal
.fa_logs_root <- function() {
  logs <- file.path(.fa_local_root(), "logs")
  if (!dir.exists(logs)) dir.create(logs, recursive = TRUE)
  logs
}

#' @name faasr_put_file
#' @title Put (upload) a file to local storage
#' @description
#' Uploads a file from the local filesystem to the local FaaSr storage.
#' This function mirrors the signature of the production FaaSr API but operates on local filesystem.
#' @param server_name Character string specifying the server name (ignored in local implementation)
#' @param local_folder Character string path to the local folder containing the file to upload
#' @param local_file Character string name of the local file to upload
#' @param remote_folder Character string path to the remote folder where the file will be stored
#' @param remote_file Character string name for the file in remote storage
#' @return Invisibly returns TRUE on success
#' @export
#' @examples
#' \dontrun{
#' # Upload a local file to remote storage
#' faasr_put_file(local_file = "data.csv", remote_file = "input/data.csv")
#' 
#' # Upload from a specific local folder
#' faasr_put_file(local_folder = "input", local_file = "data.csv", 
#'                remote_folder = "processed", remote_file = "data.csv")
#' }
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

#' @name faasr_get_file
#' @title Get (download) a file from local storage
#' @description
#' Downloads a file from the local FaaSr storage to the local filesystem.
#' This function mirrors the signature of the production FaaSr API but operates on local filesystem.
#' @param server_name Character string specifying the server name (ignored in local implementation)
#' @param remote_folder Character string path to the remote folder containing the file to download
#' @param remote_file Character string name of the remote file to download
#' @param local_folder Character string path to the local folder where the file will be saved
#' @param local_file Character string name for the file in local storage
#' @return Invisibly returns TRUE on success
#' @export
#' @examples
#' \dontrun{
#' # Download a file from remote storage
#' faasr_get_file(remote_file = "data.csv", local_file = "downloaded_data.csv")
#' 
#' # Download to a specific local folder
#' faasr_get_file(remote_folder = "processed", remote_file = "data.csv",
#'                local_folder = "output", local_file = "data.csv")
#' }
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

#' @name faasr_delete_file
#' @title Delete a file from local storage
#' @description
#' Deletes a file from the local FaaSr storage.
#' This function mirrors the signature of the production FaaSr API but operates on local filesystem.
#' @param server_name Character string specifying the server name (ignored in local implementation)
#' @param remote_folder Character string path to the remote folder containing the file to delete
#' @param remote_file Character string name of the remote file to delete
#' @return Invisibly returns TRUE on success
#' @export
#' @examples
#' \dontrun{
#' # Delete a file from remote storage
#' faasr_delete_file(remote_file = "temp_data.csv")
#' 
#' # Delete a file from a specific remote folder
#' faasr_delete_file(remote_folder = "temp", remote_file = "data.csv")
#' }
faasr_delete_file <- function(server_name = NULL, remote_folder = "", remote_file) {
  remote_folder <- sub("^/+", "", sub("/+$", "", remote_folder))
  src_dir <- if (nzchar(remote_folder)) file.path(.fa_files_root(), remote_folder) else .fa_files_root()
  target <- file.path(src_dir, remote_file)
  if (file.exists(target)) unlink(target)
  invisible(TRUE)
}

#' @name faasr_get_folder_list
#' @title List files in local storage with optional prefix
#' @description
#' Lists all files in the local FaaSr storage, optionally filtered by a prefix.
#' This function mirrors the signature of the production FaaSr API but operates on local filesystem.
#' @param server_name Character string specifying the server name (ignored in local implementation)
#' @param faasr_prefix Character string prefix to filter file names (optional)
#' @return Character vector of file names matching the criteria
#' @export
#' @examples
#' \dontrun{
#' # List all files in storage
#' files <- faasr_get_folder_list()
#' 
#' # List files with a specific prefix
#' csv_files <- faasr_get_folder_list(faasr_prefix = "data_")
#' }
faasr_get_folder_list <- function(server_name = NULL, faasr_prefix = "") {
  files <- list.files(.fa_files_root(), recursive = TRUE, all.files = FALSE)
  files <- files[!dir.exists(file.path(.fa_files_root(), files))]
  if (nzchar(faasr_prefix)) files <- files[startsWith(files, sub("^/+", "", faasr_prefix))]
  files
}

#' @name faasr_log
#' @title Append a log message to local logs
#' @description
#' Appends a log message with timestamp to the local FaaSr log file.
#' This function mirrors the signature of the production FaaSr API but operates on local filesystem.
#' @param log_message Character string message to log
#' @return Invisibly returns TRUE on success
#' @export
#' @examples
#' \dontrun{
#' # Log a simple message
#' faasr_log("Starting data processing")
#' 
#' # Log with more detail
#' faasr_log(paste("Processing", nrow(data), "rows of data"))
#' }
faasr_log <- function(log_message) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  logfile <- file.path(.fa_logs_root(), paste0("faasr_", format(Sys.time(), "%Y%m%d"), ".log"))
  cat(sprintf("[%s] %s\n", ts, log_message), file = logfile, append = TRUE)
  invisible(TRUE)
}

#' @name faasr_rank
#' @title Get current rank information for the executing function
#' @description
#' Returns the current rank information for the executing function in a parallel workflow.
#' This function mirrors the signature of the production FaaSr API but operates on local filesystem.
#' @return List containing Rank and MaxRank, or empty list if no rank information is available
#' @export
#' @examples
#' \dontrun{
#' # Get current rank information
#' rank_info <- faasr_rank()
#' if (length(rank_info) > 0) {
#'   cat("Current rank:", rank_info$Rank, "of", rank_info$MaxRank, "\n")
#' }
#' }
faasr_rank <- function() {
  # Try to find the rank info file in the temp directory
  temp_dir <- file.path(.fa_local_root(), "temp", "faasr_state_info")
  rank_file <- file.path(temp_dir, "current_rank_info.txt")
  
  if (!file.exists(rank_file)) {
    return(list())
  }
  
  rank_info <- try(readLines(rank_file, warn = FALSE), silent = TRUE)
  if (inherits(rank_info, "try-error") || length(rank_info) == 0) {
    return(list())
  }
  
  # Parse rank_info which is in format "1/3"
  parts <- unlist(strsplit(rank_info[1], "[/]"))
  if (length(parts) == 2) {
    return(list(Rank = parts[1], MaxRank = parts[2]))
  }
  
  return(list())
}

#' @name faasr_invocation_id
#' @title Get the invocation ID for the current workflow
#' @description
#' Returns the invocation ID for the current workflow execution.
#' This function mirrors the signature of the production FaaSr API but operates on local filesystem.
#' @return Character string invocation ID, or NULL if not available
#' @export
#' @examples
#' \dontrun{
#' # Get current invocation ID
#' inv_id <- faasr_invocation_id()
#' if (!is.null(inv_id)) {
#'   cat("Invocation ID:", inv_id, "\n")
#' }
#' }
faasr_invocation_id <- function() {
  # Try to find the invocation ID file in the temp directory
  temp_dir <- file.path(.fa_local_root(), "temp", "faasr_state_info")
  inv_file <- file.path(temp_dir, "current_invocation_id.txt")
  
  if (!file.exists(inv_file)) {
    return(NULL)
  }
  
  invocation_id <- try(readLines(inv_file, warn = FALSE), silent = TRUE)
  if (inherits(invocation_id, "try-error") || length(invocation_id) == 0) {
    return(NULL)
  }
  
  invocation_id[1]
}

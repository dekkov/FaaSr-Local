# Test function that demonstrates rank functionality
test_rank_function <- function(message = "Testing rank") {
  # Get rank information
  rank_info <- faasr_rank()
  
  if (length(rank_info) > 0) {
    # Running with rank
    cat(sprintf("%s - Rank: %s/%s\n", message, rank_info$Rank, rank_info$MaxRank))
    
    # Create a file specific to this rank
    filename <- sprintf("rank_%s_of_%s.txt", rank_info$Rank, rank_info$MaxRank)
    
    # Write the output
    writeLines(
      sprintf("Executed rank %s of %s at %s", rank_info$Rank, rank_info$MaxRank, Sys.time()),
      filename
    )
    
    # Upload to storage
    faasr_put_file(
      local_folder = ".",
      local_file = filename,
      remote_folder = "rank_test",
      remote_file = filename
    )
  } 
  
  return(TRUE)
}


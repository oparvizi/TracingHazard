# Define the folder path
folder_path <- "C:/Users/o.parvizi/Desktop/NCBI_Data"

# List all files in the folder
files <- list.files(folder_path, full.names = TRUE)

# Define keywords to search for
keywords <- c("html", "blocking")

# Loop through each file
for (file in files) {
  # Only process text-readable files (basic assumption)
  if (file.info(file)$isdir == FALSE) {
    # Read the file safely (ignoring warnings)
    file_content <- tryCatch(readLines(file, warn = FALSE), error = function(e) return(NULL))
    
    if (!is.null(file_content)) {
      # Check if any keyword is found
      if (any(grepl(paste(keywords, collapse = "|"), file_content, ignore.case = TRUE))) {
        # Delete the file
        file.remove(file)
        cat("Deleted:", file, "\n")
      }
    }
  }
}

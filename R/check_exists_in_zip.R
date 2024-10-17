check_exists_in_zip <- function(zip_path, file_name) {
  if(is.null(zip_path) || zip_path == "") return(rep(FALSE, length(file_name)))
  # List files in the zip
  zip_files <- unzip(zip_path, list = TRUE)$Name
  # Check if file_name is in the list of files
  file_exists <- file_name %in% zip_files
  return(file_exists)
}

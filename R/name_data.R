# This function extracts the names of the datasets provided in the input files.
# If specific data names are provided, they are used, otherwise the function tries
# to extract the names from the files themselves.

name_data <- function(files_data, data_names = NULL){
  #Grab the names of the datasets.
  if(isTruthy(data_names)){
    data_names <- gsub("(\\..*$)", "", gsub("(.*/)", "", data_names))
  }
  else if(all(grepl("(\\.xlsx$)", ignore.case = T, as.character(files_data))) & length(as.character(files_data)) == 1){
    data_names <- excel_sheets(files_data)
  }
  else{
    data_names <- gsub("(\\..*$)", "", gsub("(.*/)", "", files_data))
  }
  data_names
}
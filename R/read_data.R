# Read and format data from csv or xlsx files
read_data <- function(files_data, data_names = NULL){
  # Read in all csv files from files_data as a list.
  if(is.list(files_data)){
    data_formatted <- files_data
  }
  else{
    if(all(grepl("(\\.csv$)", ignore.case = T, as.character(files_data)))){
      data_formatted <- tryCatch(lapply(files_data, function(x){read.csv(x)}),
                                 warning = function(w) {warning(w$message)},
                                 error = function(e) {stop(e$message)})
    }
    
    else if(all(grepl("(\\.xlsx$)", ignore.case = T, as.character(files_data)))){
      if(length(as.character(files_data)) > 1){
        data_formatted <- tryCatch(lapply(files_data, function(x){read_excel(x)}),
                                   warning = function(w) {warning(w$message)},
                                   error = function(e) {stop(e$message)})
        message("When multiple Excel files are uploaded only the first sheet from each is used.")
      }
      if(length(as.character(files_data)) == 1){
        sheets <- excel_sheets(files_data)
        sheets <- sheets[!sheets %in% c("LOOKUP", "RULES")]
        if(is.null(data_names)){
          data_names <- sheets
        }
        data_formatted <- tryCatch(lapply(sheets, function(x){read_excel(files_data, sheet =  x)}),
                                   warning = function(w) {warning(w$message)},
                                   error = function(e) {stop(e$message)})    
      }
    }
    
    else{
      stop("You cannot mix data types, choose either csv or xlsx for all datasets.")
    }    
  }
  
  #Check if there is a warning when reading in the data.
  if (inherits(data_formatted, "simpleWarning") | inherits(data_formatted, "simpleError")){
    stop(paste0("There was an error that said ", data_formatted$message))
  }
  
  #Names the data with the file names.
  names(data_formatted) <- name_data(files_data, data_names)
  
  data_formatted
}

# Function for reading in data file and then determining if there is a column 
# that contains .png/.jpg or any image extension
# then attach that image to the data -- bind it

files_data <- "/Users/mippr/Documents/test-data_data_validator/test-images/valid_data (2)/data.xlsx"
file_rules <- "/Users/mippr/Documents/test-data_data_validator/test-images/rules (2).csv"
zip_data <- "/Users/mippr/Documents/test-data_data_validator/test-images/valid_data (2)/extra_files.zip"

data_formatted <- read_data(files_data = files_data)


rules <- read_rules(file_rules)

rules1 <- rules |>
  mutate(dataset = names(data_formatted))

data("test_rules")
data("valid_example")
check <- reformat_rules(rules = rules, data_formatted = data_formatted)

image_check_add <- function(data_formated = NULL){
  
  image_detected <- any(sapply(data_formatted, function(x) {
   # text column
    if (is.character(data_formatted[x]) || is.factor(data_formatted[x])) {  # Ensure it's a text column
      any(str_detect(tolower(as.character(x)), "\\.png$|\\.jpg$"), na.rm = TRUE)
    } else {
      FALSE
    }
  }))
  
  # # Add column
  # data_formatted <- data_formated |> mutate(Image = ifelse(image_detected, "y","n"))
  # return(data_formatted)
  
  print(image_detected)
} 
  

report <- lapply(names(data_formatted), function(x){
  validate::confront(data_formatted[[x]], validate::validator(.data=rules |> dplyr::filter(.data$dataset == x)))
})
#-------
# Load Images as base64
unzip_files <- unzip(zip_data, exdir = tempdir())
# If there is a zip file -- need to add that
if(any(str_detect(unzip_files, "(?i)\\.png|\\.jpg"))) {
  # image_files <- if(any(grepl(".zip$", input$file$datapath))){gsub("\\\\", "/",input$file$datapath[grepl(".zip$", input$file$datapath)])}
  file_names <- unzip_files[str_detect(unzip_files,"(?i)\\.png|\\.jpg")]
  #image_files <- 
  }

zip_data


# Determine which data table has image file paths
image_detection <- lapply(names(data_formatted), function(x){

  image_present <- any(str_detect((as.character(data_formatted[[x]])), "(?i)\\.png|\\.jpg"), na.rm = TRUE)
  if(image_present){
    data_formatted[[x]] <- data_formatted[[x]] |> 
      mutate(Image = "y")
    
    print(colnames(data_formatted[[x]]))
  }

})
  
  

  

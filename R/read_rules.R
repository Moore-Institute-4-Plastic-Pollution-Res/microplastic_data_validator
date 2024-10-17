# Read rules from a file or a data frame
# Should have the column names "name", "description", "dataset", "valid example", "severity", "rule".
# The function also checks that the rules do not contain sensitive words and that


read_rules <- function(file_rules){
  # Reads the rules file.
  if(is.data.frame(file_rules)){
    rules <- file_rules
  }
  else{
    if(grepl("(\\.csv$)", ignore.case = TRUE, as.character(file_rules))){
      rules <- read.csv(file_rules)
    }
    else if(grepl("(\\.xlsx$)", ignore.case = TRUE, as.character(file_rules))){
      rules <- read_excel(file_rules)
    }
    else{
      stop('Uploaded rules format is not currently supported, please provide a rules file in csv or xlsx format.')  
    }
  }
  
  # Test that rules file has the correct required column names.
  if (!all(c("name", "description", "severity", "rule") %in% names(rules))) {
    stop('Uploaded rules format is not currently supported, please provide a rules file with column names, "name", "description", "severity", "rule".')
  }
  
  # Tests that the rules do not contain sensitive words that may be malicious.
  if (any(grepl("config|secret", rules$rule))) {
    stop('At this time we are unable to support any rules with the words "config" or "secret" in them as they could be malicious.')
  }
  
  # Tests that the rules severity is only warning or error
  if (!all(grepl("(error)|(warning)", rules$severity))) {
    stop('severity in the rules file can only be "error" or "warning"')
  }
  
  # Checks that all the rules fields are character type.
  if (!all(sapply(rules, is.character))) {
    stop('Uploaded rules format is not currently supported, please provide a rules file with columns that are all character type.')
  }
  
  return(rules)
}

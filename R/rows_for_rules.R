# Get the rows in the data that violate the specified rules.
rows_for_rules <- function(data_formatted,
                           report,
                           broken_rules,
                           rows){
  tryCatch({
    violating(data_formatted, report[broken_rules[rows, "name"]])
  }, warning = function(w) {
    if(broken_rules[rows, "items"] == 0){
      warning("Column being assessed by the rule is not in the dataset.")
      return(data_formatted)
    }
    if(broken_rules[rows, "items"] == 1 & nrow(data_formatted) != 1){
      warning("This rule applies to the entire dataset.")
      return(data_formatted)
    }
    
  }, error = function(e) {
    if(broken_rules[rows, "items"] == 0){
      warning("Column being assessed by the rule is not in the dataset.")
      return(data_formatted)
    }
    if(broken_rules[rows, "items"] == 1 & nrow(data_formatted) != 1){
      warning("This rule applies to the entire dataset.")
      return(data_formatted)
    }
  })
}

# This function is responsible for handling the rule reformatting, dataset handling
# and foreign key checks.


reformat_rules <- function(rules, data_formatted, zip_data = NULL){
  #Add dataset if one doesn't exist so that everything else works.
  if (!"dataset" %in% names(rules)){
    rules <- rules |>
      mutate(dataset = names(data_formatted))
  }
  
  #Check for special function checking additional files
  rules <- rules |>
    dplyr::mutate(rule = ifelse(grepl("check_exists_in_zip(.*)", .data$rule),
                                paste0('check_exists_in_zip(zip_path = "', zip_data, '", file_name = ', gsub("(check_exists_in_zip\\()|(\\))", "", .data$rule), ') == TRUE'),
                                .data$rule))
  
  #Circle back to add logic for multiple dfs
  #Check for special character "___" which is for assessing every column.
  
  do_to_all <- rules |>
    dplyr::filter(grepl("___", .data$rule))
  
  if(nrow(do_to_all) > 0){
    rules <- lapply(names(data_formatted), function(x){
      rules_sub <- do_to_all |> dplyr::filter(.data$dataset == x)
      lapply(colnames(data_formatted[[x]]), function(new_name){
        rules_sub |>
          dplyr::mutate(rule = gsub("___", new_name, .data$rule)) |>
          dplyr::mutate(name = paste0(new_name, "_", .data$name))}) |>
        data.table::rbindlist()}) |>
      data.table::rbindlist() |>
      dplyr::bind_rows(rules |> dplyr::filter(!grepl("___", .data$rule)))
  }
  
  # Check special character of is_foreign_key and if so then testing that foreign keys are exact.
  foreign_keys <- rules |>
    dplyr::filter(grepl("is_foreign_key(.*)", .data$rule))
  
  if(nrow(foreign_keys) > 0){
    columns <- gsub("(is_foreign_key\\()|(\\))", "", foreign_keys[["rule"]])
    
    rules <- lapply(1:nrow(foreign_keys), function(x){
      foreign_keys[x,] |>
        mutate(rule = paste0(columns[x],
                             ' %in% c("',
                             paste(
                               lapply(data_formatted, function(y){
                                 y[[columns[x]]]
                               }) |>
                                 unlist() |>
                                 unique(),
                               collapse = '", "',
                               sep = ""),
                             '")'))
    }) |>
      rbindlist() |>
      bind_rows(rules |> filter(!grepl("is_foreign_key(.*)", .data$rule)))
  }
  rules
}


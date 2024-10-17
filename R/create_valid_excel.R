create_valid_excel <- function(file_rules,
                               negStyle  = createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE"),
                               posStyle  = createStyle(fontColour = "#006100", bgFill = "#C6EFCE"),
                               row_num   = 1000){
  #Reads the rules file.
  if(is.data.frame(file_rules)){
    rules <- file_rules
  }
  else{
    if(grepl("(\\.csv$)", ignore.case = T, as.character(file_rules))){
      rules <- read.csv(file_rules)
    }
    
    if(grepl("(\\.xlsx$)", ignore.case = T, as.character(file_rules))){
      rules <- read_excel(file_rules)
    }
  }
  #Grab the names of the datasets.
  data_names <- if("dataset" %in% names(rules)){
    unique(rules$dataset)
  }
  else{
    name <- gsub("(.*/)|(\\..*)", "", file_rules)
    rules$dataset <- name
    name
  }
  
  #Circle back to add logic for multiple dfs
  #Check for special character "___" which is for assessing every column.
  
  do_to_all <- rules |>
    filter(grepl("___", .data$rule))
  
  if(nrow(do_to_all) > 0){
    rules <- lapply(data_names, function(x){
      rules_sub <- do_to_all |> filter(.data$dataset == x)
      rules_sub_variables <- variables(validator(.data=rules_sub))
      lapply(rules_sub_variables, function(new_name){
        rules_sub |>
          mutate(rule = gsub("___", new_name, .data$rule)) |>
          mutate(name = paste0(new_name, "_", .data$name))
      }) |>
        rbindlist()
    }) |>
      rbindlist() |>
      bind_rows(rules |> filter(!grepl("___", .data$rule)))
  }
  
  rules <- rules |>
    mutate(rule = gsub("(is_foreign_key\\()|(check_exists_in_zip\\()", "!is.na\\(", .data$rule))
  
  lookup_column_index <- 1
  wb <- createWorkbook()
  addWorksheet(wb, "RULES")
  writeData(wb, sheet = "RULES", x = rules, startCol = 1)
  for(sheet_num in 1:length(data_names)){ #Sheet level for loop
    rules_all_raw <- rules |> filter(.data$dataset == data_names[sheet_num])
    rules_all <- validator(.data = rules_all_raw)
    rule_variables <- variables(rules_all)
    sheet_name <- data_names[sheet_num]
    addWorksheet(wb, sheet_name)
    freezePane(wb, sheet_name, firstRow = TRUE) ## shortcut to freeze first row for every table.
    for(col_name in rule_variables){#Setup the column names with empty rows.
      df <- as_tibble(rep("", row_num))
      names(df) <- col_name
      column_index_startup <- which(rule_variables == col_name)
      writeData(wb, sheet = sheet_name, x = df, startCol = column_index_startup)
    }
    for(col_num in 1:length(rules_all)){
      rule_test <- rules_all[[col_num]]
      expression <- rule_test@expr
      column_index <- which(rule_variables == variables(rule_test))
      if(any(grepl("(%vin%)|(%in%)", expression))){
        if(lookup_column_index == 1){
          addWorksheet(wb, "LOOKUP")
        }
        values <- unlist(strsplit(gsub('(")|(\\))|(.*c\\()', "", as.character(expression[3])), ", "))
        lookup_col <- LETTERS[lookup_column_index]
        df_lookup <- tibble(values)
        names(df_lookup) <- paste0(variables(rule_test), "_lookup")
        writeData(wb,
                  sheet = "LOOKUP",
                  x = df_lookup,
                  startCol = lookup_column_index)
        dataValidation(wb,
                       sheet = sheet_name,
                       cols = column_index,
                       rows = 2:row_num,
                       type = "list",
                       value = paste0("LOOKUP!$", lookup_col, "$2:$", lookup_col, "$", length(values) +1))  
        lookup_column_index = lookup_column_index + 1
      }
      if(any(grepl("is_unique\\(.*\\)", expression))){
        conditionalFormatting(wb,
                              sheet_name,
                              cols = column_index,
                              rows = 2:row_num,
                              type = "duplicates",
                              style = negStyle)
      }
      if(sum(grepl("!|is.na(.*)", expression)) == 2){ #Not working yet.
        dataValidation(wb,
                       sheet_name,
                       cols = column_index,
                       rows = 2:row_num,
                       type = "textlength",
                       operator = "greaterThanOrEqual",
                       value = "1",
                       allowBlank = FALSE)
      }
      if(any(grepl("in_range(.*)", expression))){
        dataValidation(wb,
                       sheet_name,
                       cols = column_index,
                       rows = 2:row_num,
                       type = "decimal",
                       operator = "between",
                       value = c(as.numeric(as.character(expression)[grepl("^-|[0-9]+$", as.character(expression))][1]),
                                 as.numeric(as.character(expression)[grepl("^-|[0-9]+$", as.character(expression))][2])))
      }
      if(any(grepl("grepl(.*)", expression))){ #could be improved with begins with and ends with logic.  
        good_conditions <- unlist(strsplit(gsub('(\\[[0-9]*-[0-9]*\\])|(\\])|(\\[)|(\\\\)|(\\^)|(\\$)|(\\))|(\\()', "",  as.character(expression)[2]), split = "\\|"))
        for(contain_condition in good_conditions){
          conditionalFormatting(wb,
                                sheet_name,
                                cols = column_index,
                                rows = 2:row_num,
                                type = "contains",
                                rule = contain_condition,
                                style = posStyle)
        }
      }
      if(any(grepl("(%vin%)|(%in%)", expression))){
        protectWorksheet(
          wb,
          "LOOKUP",
          protect = TRUE) #Protects the lookup table without a password just to prevent accidents.
      }
      #Need better way to deal with foreign keys, currently not working well.
      
    }
  }
  wb
}

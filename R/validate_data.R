validate_data <- function(files_data, data_names = NULL, file_rules = NULL, zip_data = NULL) {
  if(check_for_malicious_files(c(files_data, file_rules))){
    stop(paste("Data or rules files cannot be of any of these types:", "_exe", "a6p", "ac", "acr", "action", "air", "apk", "app",
               "applescript", "awk", "bas", "bat", "bin", "cgi", "chm",
               "cmd", "com", "cpl", "crt", "csh", "dek", "dld", "dll",
               "dmg", "drv", "ds", "ebm", "elf", "emf", "esh", "exe",
               "ezs", "fky", "frs", "fxp", "gadget", "gpe", "gpu", "hlp",
               "hms", "hta", "icd", "iim", "inf", "ins", "inx", "ipa",
               "ipf", "isp", "isu", "jar", "js", "jse", "jsp", "jsx",
               "kix", "ksh", "lib", "lnk", "mcr", "mel", "mem", "mpkg",
               "mpx", "mrc", "ms", "msc", "msi", "msp", "mst", "mxe",
               "obs", "ocx", "pas", "pcd", "pex", "pif", "pkg", "pl",
               "plsc", "pm", "prc", "prg", "pvd", "pwc", "py", "pyc",
               "pyo", "qpx", "rbx", "reg", "rgs", "rox", "rpj", "scar",
               "scpt", "scr", "script", "sct", "seed", "sh", "shb",
               "shs", "spr", "sys", "thm", "tlb", "tms", "u3p", "udf",
               "url", "vb", "vbe", "vbs", "vbscript", "vdo", "vxd",
               "wcm", "widget", "wmf", "workflow", "wpk", "ws", "wsc",
               "wsf", "wsh", "xap", "xqt", "zlq"))
  }
  if(!is.null(zip_data)){
    if(check_for_malicious_files(utils::unzip(zip_data, list = T)$Name)){
      stop(paste("Data or rules files cannot be of any of these types:",
                 "_exe", "a6p", "ac", "acr", "action", "air", "apk", "app",
                 "applescript", "awk", "bas", "bat", "bin", "cgi", "chm",
                 "cmd", "com", "cpl", "crt", "csh", "dek", "dld", "dll",
                 "dmg", "drv", "ds", "ebm", "elf", "emf", "esh", "exe",
                 "ezs", "fky", "frs", "fxp", "gadget", "gpe", "gpu", "hlp",
                 "hms", "hta", "icd", "iim", "inf", "ins", "inx", "ipa",
                 "ipf", "isp", "isu", "jar", "js", "jse", "jsp", "jsx",
                 "kix", "ksh", "lib", "lnk", "mcr", "mel", "mem", "mpkg",
                 "mpx", "mrc", "ms", "msc", "msi", "msp", "mst", "mxe",
                 "obs", "ocx", "pas", "pcd", "pex", "pif", "pkg", "pl",
                 "plsc", "pm", "prc", "prg", "pvd", "pwc", "py", "pyc",
                 "pyo", "qpx", "rbx", "reg", "rgs", "rox", "rpj", "scar",
                 "scpt", "scr", "script", "sct", "seed", "sh", "shb",
                 "shs", "spr", "sys", "thm", "tlb", "tms", "u3p", "udf",
                 "url", "vb", "vbe", "vbs", "vbscript", "vdo", "vxd",
                 "wcm", "widget", "wmf", "workflow", "wpk", "ws", "wsc",
                 "wsf", "wsh", "xap", "xqt", "zlq"))
    }
  }
  
  rules <- read_rules(file_rules)
  
  data_formatted <- read_data(files_data = files_data, data_names = data_names)
  
  if (!"dataset" %in% names(rules) & length(names(data_formatted)) > 1) {
    stop("If there is more than one dataset then a dataset column must be specified in the rules file to describe which rule applies to which dataset.")
  }
  
  if ("dataset" %in% names(rules)) {
    if (!all(unique(rules$dataset) %in% names(data_formatted))) {
      stop(paste0("If there is a dataset column in the rules file it needs to pertain to the names of the datasets being tested. The rules file lists datasets ", paste(setdiff(unique(rules$dataset), names(data_formatted)), collapse = ", "),  " that do not match the datasets shared."))
    }
  }
  
  rules <- reformat_rules(rules = rules, data_formatted = data_formatted, zip_data = zip_data)
  
  rules_formatted <- tryCatch(validate::validator(.data=rules),
                              warning = function(w) {
                                warning(w)
                                NULL
                              },
                              error = function(e) {
                                stop(e)
                              })
  
  if (is.null(rules_formatted) || (length(class(rules_formatted)) != 1 || !inherits(rules_formatted, "validator"))) {
    stop("There was an error with reading the rules file.")
  }
  
  all_variables <- unique(c(validate::variables(rules_formatted), unlist(lapply(data_formatted, names))))
  
  if (!(all(all_variables %in% validate::variables(rules_formatted)) & all(all_variables %in% unlist(lapply(data_formatted, names))))) {
    warning(paste0("All variables in the rules csv should be in the data csv and vice versa for the validation to work correctly. Download the Data Template for an example of correctly formatted upload. Ignoring these unmatched variables ", paste0(all_variables[!(all_variables %in% validate::variables(rules_formatted)) | !(all_variables %in% unlist(lapply(data_formatted, names)))], collapse = ", ")))
  }
  
  report <- lapply(names(data_formatted), function(x){
    validate::confront(data_formatted[[x]], validate::validator(.data=rules |> dplyr::filter(.data$dataset == x)))
  })
  
  results <- lapply(report, function(x) {
    validate::summary(x) |>
      dplyr::left_join(rules, by = "name") |>
      dplyr::mutate(status = ifelse((.data$fails > 0 & .data$severity == "error") | .data$error | .data$warning , "error", "success")) |>
      mutate(status = ifelse(.data$fails > 0 & .data$severity == "warning", "warning", .data$status))
  })
  
  any_issues <- vapply(results, function(x) {
    any(x$status == "error")
  }, FUN.VALUE = TRUE)
  
  rules_list_formatted <- tryCatch(lapply(names(data_formatted), function(x) {
    validator(.data=rules |> filter(.data$dataset == x))
  }),
  warning = function(w) {
    warning(w)
    NULL
  },
  error = function(e) {
    stop(e)
  })
  
  list(data_formatted = data_formatted,
       data_names = names(data_formatted),
       zip_data = if(exists("zip_data")){zip_data} else{NULL},
       report = report,
       results = results,
       rules = rules_list_formatted,
       issues = any_issues)
}

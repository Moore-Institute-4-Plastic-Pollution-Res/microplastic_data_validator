# Filter the results of validation to show only broken rules, optionally including successful decisions.

rules_broken <- function(results, show_decision){
  results |>
    dplyr::filter(if(show_decision){.data$status %in% c("error", "warning")} else{.data$status %in% c("error", "warning", "success")}) |>
    select("description", "status", "name", "expression", everything())
}

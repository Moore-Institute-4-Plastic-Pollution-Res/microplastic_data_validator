# This function checks if the input string contains a non-image hyperlink and formats it as an HTML anchor tag.
check_other_hyperlinks <- function(x){
  ifelse(grepl("https://", x) & !grepl("https://.*\\.png|https://.*\\.jpg", x),
         paste0('<a href ="', x, '">', x, '</a>'),
         x)
}

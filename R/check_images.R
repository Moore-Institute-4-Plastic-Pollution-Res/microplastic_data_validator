# This function checks if the input string contains an image URL (PNG or JPG) and formats it as an HTML img tag with a specified height.
check_images <- function(x){
  ifelse(grepl("https://.*\\.png|https://.*\\.jpg", x),
         paste0('<img src ="', x, '" height = "50"></img>'),
         x)
}


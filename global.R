library(shiny)
library(dplyr)
library(DT)
library(shinythemes)
library(shinyWidgets)
library(bs4Dash)
library(shinyjs)
library(bs4Dash)
library(validate)
library(digest)
library(data.table)
library(purrr)
library(shinyjs)
library(sentimentr)
library(listviewer)
library(readxl)
library(stringr)
library(openxlsx)

config <- config::get(file = "config_pl.yml")

# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}




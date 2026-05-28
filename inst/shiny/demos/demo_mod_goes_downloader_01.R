library(shiny)
library(shinyjs)
library(dplyr)
library(httr)
library(xml2)
library(stringr)
library(DT)
devtools::load_all()
#source("R/mod_goes_downloader.R")

ui <- fluidPage(
  shinyjs::useShinyjs(),

  # titlePanel("Mi app principal"),

  mod_goes_downloader_01_ui(id = "goesdl")
)


# the_folder <-  fn_my_folder_testing()


server <- function(input, output, session) {

  mod_goes_downloader_01_server(id = "goesdl", str_folder_path_data_raw = fn_my_folder_testing())

}

shinyApp(ui = ui, server = server)

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

  mod_goes_downloader_04_ui(id = "goesdl")
)


# the_folder <-  fn_my_folder_testing()


server <- function(input, output, session) {

  str_folder_data_raw <- "C:/Users/Legion/AppData/Local/LegionGOES_lab_0.0.2/data_raw"

  mod_goes_downloader_04_server(id = "goesdl", str_folder_path_data_raw = str_folder_data_raw)

}

shinyApp(ui = ui, server = server)

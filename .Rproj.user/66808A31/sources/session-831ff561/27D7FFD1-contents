library(shiny)
library(shinyjs)
library(DT)
library(dplyr)
library(bslib)

devtools::load_all()

ui <- bslib::page_fluid(

  title = "GOES Downloader",

  shinyjs::useShinyjs(),

  h2("GOES Downloader"),

  mod_goes_99_downloader_ui(id = "goes_download")
)

server <- function(input, output, session) {

  cfg <- legionGOES.config::legion_config_bootstrap()
  str_folder_path_data_raw <- cfg$paths$data_raw_dir

  goes_download <- mod_goes_99_downloader_server(
    id = "goes_download",
    str_folder_path_data_raw = str_folder_path_data_raw
  )
}

shinyApp(ui, server)

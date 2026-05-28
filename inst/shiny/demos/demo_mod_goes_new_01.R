library(shiny)
library(shinyjs)
library(DT)
library(dplyr)
library(bslib)

devtools::load_all()

ui <- bslib::page_navbar(

  title = "GOES Downloader",

  shinyjs::useShinyjs(),

  bslib::nav_panel(
    title = "1. Parameters",
    mod_goes_01_params_ui("goes_params")
  ),

  bslib::nav_panel(
    title = "2. Catalog & Download",
    mod_goes_02_catalog_download_ui("goes_catalog_download")
  )
)

server <- function(input, output, session) {

  cfg <- legionGOES.config::legion_config_bootstrap()
  str_folder_path_data_raw <- cfg$paths$data_raw_dir

  goes_params <- mod_goes_01_params_server("goes_params")

  goes_catalog_download <- mod_goes_02_catalog_download_server(
    id = "goes_catalog_download",
    params_r = goes_params,
    data_raw_dir = str_folder_path_data_raw,
    max_pages = 20
  )
}

shinyApp(ui, server)

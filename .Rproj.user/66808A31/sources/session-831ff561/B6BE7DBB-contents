# ==============================================================================
# MODULO SELECTOR - GOES DOWNLOADER
# Cambiar aquí qué downloader está "en cancha".
# ==============================================================================

#' Selector UI - GOES Downloader
#'
#' @param id ID del módulo Shiny.
#'
#' @export
mod_goes_downloader_selected_ui <- function(id) {
  ns <- shiny::NS(id)

  mod_goes_downloader_04_ui(ns("download_core"))
}


#' Selector UI - GOES Downloader
#'
#' @param id ID del módulo Shiny.
#'
#' @export
mod_goes_downloader_selected_server <- function(id, str_folder_path_data_raw) {
  shiny::moduleServer(id, function(input, output, session) {

    #mod_goes_downloader_01_server("download_core", str_folder_path_data_raw = str_folder_path_data_raw)
    mod_goes_downloader_04_server("download_core", str_folder_path_data_raw = str_folder_path_data_raw)
    # mod_goes_downloader_02_server("download_core", str_folder_path_data_raw = str_folder_path_data_raw)
  })
}

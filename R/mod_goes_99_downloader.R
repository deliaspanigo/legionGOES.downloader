# ==============================================================================
# MODULO SELECTOR - GOES DOWNLOADER
# Cambiar aquí qué downloader está "en cancha".
# ==============================================================================

#' Selector UI - GOES Downloader
#'
#' @param id ID del módulo Shiny.
#'
#' @export
mod_goes_99_downloader_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    shinyjs::useShinyjs(),

    bslib::navset_tab(

      bslib::nav_panel(
        title = "1. Parameters",
        mod_goes_01_params_ui(ns("goes_params"))
      ),

      bslib::nav_panel(
        title = "2. Download",
        mod_goes_02_download_ui(ns("goes_download"))
      )
    )
  )
}


#' Selector Server - GOES Downloader
#'
#' @param id ID del módulo Shiny.
#' @param str_folder_path_data_raw Path donde se guardarán los archivos raw.
#'
#' @export
mod_goes_99_downloader_server <- function(id, str_folder_path_data_raw) {

  shiny::moduleServer(id, function(input, output, session) {

    goes_params <- mod_goes_01_params_server("goes_params")

    goes_download <- mod_goes_02_download_server(
      id = "goes_download",
      params_r = goes_params,
      data_raw_dir = str_folder_path_data_raw,
      max_pages = 20
    )

    return(
      shiny::reactive({
        list(
          params = goes_params(),
          download = goes_download()
        )
      })
    )
  })
}

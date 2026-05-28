# ==============================================================================
# INSTALADOR DE PAQUETES PARA R PORTABLE
# ==============================================================================

portable_lib <- "C:/Users/Legion/bulk/MyTesis/f01_software/f01_installer_legion/App/R-Library"

dir.create(portable_lib, recursive = TRUE, showWarnings = FALSE)

.libPaths(c(portable_lib, .libPaths()))

options(repos = c(
  CRAN = "https://packagemanager.posit.co/cran/2025-11-15"
))

packages <- c(
  "shiny",
  "bslib",
  "shinyjs",
  "leaflet",
  "sf",
  "threejs",
  "igraph",
  "sp",
  "httr",
  "dplyr",
  "plotly",
  "ggplot2",
  "terra",
  "xml2",
  "stringr",
  "lubridate",
  "purrr",
  "png",
  "rnaturalearth",
  "maps",
  "htmlwidgets",
  "viridisLite",
  "jsonlite",
  "glue",
  "devtools",
  "remotes"
)

installed <- rownames(installed.packages(lib.loc = portable_lib))

to_install <- setdiff(packages, installed)

install.packages(
  to_install,
  lib = portable_lib,
  dependencies = TRUE
)

cat("\n========================================================\n")
cat("Paquetes instalados en:\n")
cat(portable_lib, "\n")
cat("========================================================\n")

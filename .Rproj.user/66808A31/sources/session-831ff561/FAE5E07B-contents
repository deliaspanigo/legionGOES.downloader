local({

  # R Home
  r_home <- R.home()

  # Legion Home
  legion_home <- dirname(dirname(r_home))

  # R SOT
  str_file_path_R_SOT <- file.path(legion_home, "f00_SOT", "R_SOT_WINDOWS.R")
  source(file = str_file_path_R_SOT)


  library("legionGOES.config")
  legionGOES.config::legion_config_bootstrap()

  invisible(TRUE)
})

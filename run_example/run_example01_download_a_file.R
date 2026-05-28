library(httr)
library(xml2)
library(stringr)
library(dplyr)


source("R/fn_my_folder_package.R")
source("R/fn_my_folder_testing.R")
source("R/goes_downloader_core.R")

str_testing <-  fn_my_folder_testing()
download_dir <- file.path(str_testing, "data_raw")

product_times <- list(
  list(
    product = "FDCF",
    hour = "00",
    minute = "00",
    second = "ALL"
  )
)

prepared <- goes_prepare_download_plan(
  position = "EAST",
  year = 2026,
  date_format = "gregorian",
  month = 5,
  day = 17,
  product_times = product_times,
  download_dir = download_dir
)

prepared$verification_table
prepared$download_plan

##################################################################################


download_result <- goes_download_plan(
  plan = prepared$download_plan,
  status_fun = message
)

download_result

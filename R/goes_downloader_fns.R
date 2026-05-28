# ==============================================================================
# GOES DOWNLOAD CORE
# Pure functions used by the download module
# ==============================================================================

goes_parse_start_time <- function(file) {

  stamp <- sub("^.*_s([0-9]{13,14}).*$", "\\1", file)
  bad <- !grepl("^[0-9]{13,14}$", stamp)

  out <- rep(
    as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"),
    length(file)
  )

  if (all(bad)) {
    return(out)
  }

  stamp_ok <- stamp[!bad]

  year <- as.integer(substr(stamp_ok, 1, 4))
  jday <- as.integer(substr(stamp_ok, 5, 7))
  hour <- as.integer(substr(stamp_ok, 8, 9))
  minute <- as.integer(substr(stamp_ok, 10, 11))
  second <- as.integer(substr(stamp_ok, 12, 13))

  day <- as.Date(
    jday - 1,
    origin = sprintf("%04d-01-01", year)
  )

  out[!bad] <- as.POSIXct(
    paste(day, sprintf("%02d:%02d:%02d", hour, minute, second)),
    tz = "UTC"
  )

  out
}


goes_empty_online_table <- function() {
  data.frame(
    product = character(),
    product_code = character(),
    position = character(),
    satellite = character(),
    bucket = character(),
    date = as.Date(character()),
    julian_day = character(),
    key = character(),
    file = character(),
    size_online = numeric(),
    url = character(),
    start_time_utc = as.POSIXct(character(), tz = "UTC"),
    stringsAsFactors = FALSE
  )
}


goes_list_online_product_day <- function(
    product,
    position,
    date,
    max_pages = 20
) {

  product_code <- goes_product_code(product)

  sat <- goes_resolve_satellite(
    position = position,
    date = as.Date(date)
  )

  julian_day <- as.integer(format(as.Date(date), "%j"))
  year <- as.integer(format(as.Date(date), "%Y"))

  prefix <- goes_make_day_prefix(
    product_code = product_code,
    year = year,
    julian_day = julian_day
  )

  online <- goes_list_s3_prefix_paginated(
    bucket = sat$bucket,
    prefix = prefix,
    max_pages = max_pages
  )

  if (is.null(online) || nrow(online) == 0) {
    return(goes_empty_online_table())
  }

  online$product <- product
  online$product_code <- product_code
  online$position <- position
  online$satellite <- sat$satellite
  online$bucket <- sat$bucket
  online$date <- as.Date(date)
  online$julian_day <- sprintf("%03d", julian_day)
  online$start_time_utc <- goes_parse_start_time(online$file)

  online <- online[!is.na(online$start_time_utc), , drop = FALSE]
  online <- online[order(online$start_time_utc, online$file), , drop = FALSE]

  online
}


goes_list_online_from_params <- function(params, max_pages = 20, status_fun = message) {

  products <- params$product

  if (length(products) == 0) {
    stop("No product selected.")
  }

  dates <- seq.Date(
    from = as.Date(params$from_utc, tz = "UTC"),
    to = as.Date(params$to_utc, tz = "UTC"),
    by = "day"
  )

  out <- list()
  k <- 1L

  for (date_now in dates) {
    for (product_now in products) {

      status_fun(sprintf(
        "Listing online files: %s | %s | %s",
        params$position,
        product_now,
        as.character(date_now)
      ))

      x <- tryCatch(
        goes_list_online_product_day(
          product = product_now,
          position = params$position,
          date = date_now,
          max_pages = max_pages
        ),
        error = function(e) {
          warning(conditionMessage(e))
          goes_empty_online_table()
        }
      )

      if (nrow(x) > 0) {
        out[[k]] <- x
        k <- k + 1L
      }
    }
  }

  if (length(out) == 0) {
    return(goes_empty_online_table())
  }

  dplyr::bind_rows(out)
}


goes_filter_online_from_params <- function(online, params) {

  if (is.null(online) || nrow(online) == 0) {
    return(goes_empty_online_table())
  }

  from_utc <- as.POSIXct(params$from_utc, tz = "UTC")
  to_utc <- as.POSIXct(params$to_utc, tz = "UTC")

  if (params$date_mode == "Now (Last file online)") {

    x <- online[
      online$start_time_utc <= from_utc,
      ,
      drop = FALSE
    ]

    if (nrow(x) == 0) {
      return(x)
    }

    split_product <- split(x, x$product)

    out <- lapply(split_product, function(z) {
      z <- z[order(z$start_time_utc, z$file), , drop = FALSE]
      z[nrow(z), , drop = FALSE]
    })

    return(dplyr::bind_rows(out))
  }

  if (params$time_mode == "Full day") {

    x <- online[
      online$start_time_utc >= from_utc &
        online$start_time_utc <= to_utc,
      ,
      drop = FALSE
    ]

    return(x)
  }

  if (params$time_mode == "Time range") {

    x <- online[
      online$start_time_utc >= from_utc &
        online$start_time_utc <= to_utc,
      ,
      drop = FALSE
    ]

    if (
      !is.null(params$time_range_files) &&
      params$time_range_files == "Only the first file of each hour of each day"
    ) {

      x$hour_key <- format(x$start_time_utc, "%Y-%m-%d %H", tz = "UTC")

      split_hour <- split(
        x,
        paste(x$product, x$date, x$hour_key)
      )

      out <- lapply(split_hour, function(z) {
        z <- z[order(z$start_time_utc, z$file), , drop = FALSE]
        z[1, , drop = FALSE]
      })

      x <- dplyr::bind_rows(out)
      x$hour_key <- NULL
    }

    return(x)
  }

  if (params$time_mode == "Single hour") {

    target <- from_utc

    split_product_date <- split(
      online,
      paste(online$product, online$date)
    )

    out <- lapply(split_product_date, function(z) {

      delta <- as.numeric(difftime(
        z$start_time_utc,
        target,
        units = "secs"
      ))

      rule <- params$single_hour_search

      if (rule == "Exact") {
        idx <- which(delta == 0)
      } else if (rule == "Less than or equal") {
        idx <- which(delta <= 0)
        if (length(idx) > 0) idx <- idx[which.max(z$start_time_utc[idx])]
      } else if (rule == "Greater than or equal") {
        idx <- which(delta >= 0)
        if (length(idx) > 0) idx <- idx[which.min(z$start_time_utc[idx])]
      } else if (rule == "Less than") {
        idx <- which(delta < 0)
        if (length(idx) > 0) idx <- idx[which.max(z$start_time_utc[idx])]
      } else if (rule == "Greater than") {
        idx <- which(delta > 0)
        if (length(idx) > 0) idx <- idx[which.min(z$start_time_utc[idx])]
      } else {
        idx <- integer()
      }

      if (length(idx) == 0) {
        return(z[0, , drop = FALSE])
      }

      z[idx[1], , drop = FALSE]
    })

    return(dplyr::bind_rows(out))
  }

  online[0, , drop = FALSE]
}


goes_data_raw_file_path <- function(
    download_dir,
    bucket,
    product_code,
    year,
    julian_day,
    start_time_utc,
    filename
) {

  hour <- format(start_time_utc, "%H", tz = "UTC")

  file.path(
    normalizePath(download_dir, winslash = "/", mustWork = FALSE),
    bucket,
    product_code,
    sprintf("%04d", as.integer(year)),
    sprintf("%03d", as.integer(julian_day)),
    hour,
    filename
  )
}


goes_safe_file_size <- function(path) {
  if (!file.exists(path)) {
    return(NA_real_)
  }

  as.numeric(file.info(path)$size)
}


goes_make_download_plan <- function(selected, download_dir) {

  if (is.null(selected) || nrow(selected) == 0) {
    return(list(
      verification = data.frame(),
      plan = data.frame()
    ))
  }

  year <- as.integer(format(as.Date(selected$date), "%Y"))

  destination <- goes_data_raw_file_path(
    download_dir = download_dir,
    bucket = selected$bucket,
    product_code = selected$product_code,
    year = year,
    julian_day = selected$julian_day,
    start_time_utc = selected$start_time_utc,
    filename = selected$file
  )

  local_found <- file.exists(destination)
  local_size <- vapply(destination, goes_safe_file_size, numeric(1))

  size_match <- local_found &
    !is.na(local_size) &
    !is.na(selected$size_online) &
    local_size == selected$size_online

  action <- ifelse(
    local_found & size_match,
    "OK",
    ifelse(local_found & !size_match, "Delete and Download", "Download")
  )

  verification <- data.frame(
    product = selected$product,
    product_code = selected$product_code,
    position = selected$position,
    satellite = selected$satellite,
    bucket = selected$bucket,
    date = as.character(selected$date),
    julian_day = selected$julian_day,
    start_time_utc = format(
      selected$start_time_utc,
      "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ),
    online_file = selected$file,
    online_size = selected$size_online,
    online_size_mb = round(selected$size_online / 1024^2, 3),
    local_found = local_found,
    local_size = local_size,
    local_size_mb = ifelse(
      is.na(local_size),
      NA_real_,
      round(local_size / 1024^2, 3)
    ),
    size_match = ifelse(local_found, size_match, NA),
    action = action,
    destination = destination,
    online_url = selected$url,
    online_key = selected$key,
    stringsAsFactors = FALSE
  )

  plan <- verification[
    verification$action %in% c("Download", "Delete and Download"),
    ,
    drop = FALSE
  ]

  list(
    verification = verification,
    plan = plan
  )
}


goes_download_one_file <- function(url, destination, overwrite = TRUE) {

  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(destination) && isTRUE(overwrite)) {
    unlink(destination)
  }

  utils::download.file(
    url = url,
    destfile = destination,
    mode = "wb",
    quiet = TRUE
  )

  file.exists(destination)
}


goes_run_download_plan <- function(plan, log_fun = message) {

  if (is.null(plan) || nrow(plan) == 0) {
    return(data.frame())
  }

  status <- data.frame(
    product = plan$product,
    file = plan$online_file,
    destination = plan$destination,
    status = rep("pending", nrow(plan)),
    detail = rep("", nrow(plan)),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(plan))) {

    log_fun(sprintf(
      "[%d/%d] Downloading %s",
      i,
      nrow(plan),
      plan$online_file[[i]]
    ))

    ok <- tryCatch(
      {
        goes_download_one_file(
          url = plan$online_url[[i]],
          destination = plan$destination[[i]],
          overwrite = TRUE
        )
      },
      error = function(e) {
        status$status[[i]] <- "error"
        status$detail[[i]] <- conditionMessage(e)
        FALSE
      }
    )

    if (isTRUE(ok)) {
      status$status[[i]] <- "ok"
      status$detail[[i]] <- "Downloaded"
    } else if (status$status[[i]] != "error") {
      status$status[[i]] <- "error"
      status$detail[[i]] <- "Download failed"
    }
  }

  status
}

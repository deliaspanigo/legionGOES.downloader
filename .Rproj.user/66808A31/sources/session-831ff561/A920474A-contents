# ==============================================================================
# GOES URL CORE
# ------------------------------------------------------------------------------
# Minimal functions to:
#   1. resolve GOES satellite/bucket from position + UTC time
#   2. resolve product code
#   3. build S3 prefix
#   4. list files for one UTC hour
#   5. parse GOES start time from filename
#   6. select one file using a rule
#   7. return the real URL for download
#
# Requires an existing function:
#   goes_list_s3_prefix_paginated(bucket, prefix, max_pages)
# ==============================================================================


# ------------------------------------------------------------------------------
# Satellite table
# ------------------------------------------------------------------------------

tb_goes_satellite_time <- data.frame(
  position = c("EAST", "EAST", "WEST", "WEST"),
  satellite = c("GOES-16", "GOES-19", "GOES-17", "GOES-18"),
  bucket = c("noaa-goes16", "noaa-goes19", "noaa-goes17", "noaa-goes18"),
  start_utc = as.POSIXct(
    c(
      "2017-12-18 00:00:00",
      "2025-04-07 15:00:00",
      "2019-02-12 00:00:00",
      "2023-01-04 18:00:00"
    ),
    tz = "UTC"
  ),
  end_utc = as.POSIXct(
    c(
      "2025-04-07 15:00:00",
      NA,
      "2023-01-04 18:00:00",
      NA
    ),
    tz = "UTC"
  ),
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# Resolve satellite and bucket
# ------------------------------------------------------------------------------

fn_goes_resolve_satellite_by_time <- function(position, time_utc) {

  position <- toupper(position)

  if (!position %in% c("EAST", "WEST")) {
    stop("position must be 'EAST' or 'WEST'.")
  }

  time_utc <- as.POSIXct(time_utc, tz = "UTC")

  out <- lapply(seq_along(time_utc), function(i) {

    x <- tb_goes_satellite_time[
      tb_goes_satellite_time$position == position,
      ,
      drop = FALSE
    ]

    end_aux <- x$end_utc
    end_aux[is.na(end_aux)] <- as.POSIXct(
      "9999-12-31 00:00:00",
      tz = "UTC"
    )

    ok <- time_utc[[i]] >= x$start_utc & time_utc[[i]] < end_aux

    if (!any(ok)) {
      return(data.frame(
        position = position,
        time_utc = time_utc[[i]],
        satellite = NA_character_,
        bucket = NA_character_,
        stringsAsFactors = FALSE
      ))
    }

    data.frame(
      position = position,
      time_utc = time_utc[[i]],
      satellite = x$satellite[ok][1],
      bucket = x$bucket[ok][1],
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, out)
}


# ------------------------------------------------------------------------------
# Product code
# ------------------------------------------------------------------------------

fn_goes_product_code <- function(product) {

  product <- toupper(product)

  switch(
    product,
    "FDCF"   = "ABI-L2-FDCF",
    "MCMIPF" = "ABI-L2-MCMIPF",
    "LSTF"   = "ABI-L2-LSTF",
    "GLM"    = "GLM-L2-LCFA",
    stop("Unknown product: ", product)
  )
}


# ------------------------------------------------------------------------------
# Build S3 prefix
# ------------------------------------------------------------------------------

fn_goes_make_s3_prefix <- function(product_code, time_utc) {

  time_utc <- as.POSIXct(time_utc, tz = "UTC")

  year <- format(time_utc, "%Y", tz = "UTC")
  julian_day <- format(time_utc, "%j", tz = "UTC")
  hour <- format(time_utc, "%H", tz = "UTC")

  paste0(
    product_code,
    "/",
    year,
    "/",
    julian_day,
    "/",
    hour,
    "/"
  )
}


# ------------------------------------------------------------------------------
# Parse GOES start time from filename
# ------------------------------------------------------------------------------

fn_goes_parse_start_time <- function(filename) {

  stamp <- sub("^.*_s([0-9]{13,14}).*$", "\\1", filename)

  out <- rep(
    as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"),
    length(filename)
  )

  ok <- grepl("^[0-9]{13,14}$", stamp)

  if (!any(ok)) {
    return(out)
  }

  stamp_ok <- stamp[ok]

  year <- as.integer(substr(stamp_ok, 1, 4))
  jday <- as.integer(substr(stamp_ok, 5, 7))
  hour <- as.integer(substr(stamp_ok, 8, 9))
  minute <- as.integer(substr(stamp_ok, 10, 11))
  second <- as.integer(substr(stamp_ok, 12, 13))

  date <- as.Date(
    jday - 1,
    origin = sprintf("%04d-01-01", year)
  )

  out[ok] <- as.POSIXct(
    paste(date, sprintf("%02d:%02d:%02d", hour, minute, second)),
    tz = "UTC"
  )

  out
}


# ------------------------------------------------------------------------------
# List files for one UTC hour
# ------------------------------------------------------------------------------

fn_goes_list_files_for_hour <- function(
    position,
    product,
    time_utc,
    max_pages = 20
) {

  time_utc <- as.POSIXct(time_utc, tz = "UTC")

  sat <- fn_goes_resolve_satellite_by_time(
    position = position,
    time_utc = time_utc
  )

  if (is.na(sat$bucket[[1]])) {
    return(data.frame())
  }

  product_code <- fn_goes_product_code(product)

  prefix <- fn_goes_make_s3_prefix(
    product_code = product_code,
    time_utc = time_utc
  )

  x <- goes_list_s3_prefix_paginated(
    bucket = sat$bucket[[1]],
    prefix = prefix,
    max_pages = max_pages
  )

  if (is.null(x) || nrow(x) == 0) {
    return(data.frame())
  }

  if (!"key" %in% names(x)) {
    stop("The S3 listing must contain a 'key' column.")
  }

  if (!"file" %in% names(x)) {
    x$file <- basename(x$key)
  }

  if (!"url" %in% names(x)) {
    x$url <- paste0(
      "https://",
      sat$bucket[[1]],
      ".s3.amazonaws.com/",
      x$key
    )
  }

  if (!"size_online" %in% names(x)) {
    if ("size" %in% names(x)) {
      x$size_online <- x$size
    } else {
      x$size_online <- NA_real_
    }
  }

  x$position <- sat$position[[1]]
  x$satellite <- sat$satellite[[1]]
  x$bucket <- sat$bucket[[1]]
  x$product <- toupper(product)
  x$product_code <- product_code
  x$prefix <- prefix
  x$start_time_utc <- fn_goes_parse_start_time(x$file)

  x <- x[!is.na(x$start_time_utc), , drop = FALSE]
  x <- x[order(x$start_time_utc, x$file), , drop = FALSE]

  rownames(x) <- NULL
  x
}


# ------------------------------------------------------------------------------
# Select one file using a search rule
# ------------------------------------------------------------------------------

fn_goes_select_file <- function(
    files,
    target_time_utc,
    rule = "less_equal"
) {

  if (is.null(files) || nrow(files) == 0) {
    return(data.frame())
  }

  target_time_utc <- as.POSIXct(target_time_utc, tz = "UTC")

  delta <- as.numeric(difftime(
    files$start_time_utc,
    target_time_utc,
    units = "secs"
  ))

  if (rule == "exact") {

    idx <- which(delta == 0)

  } else if (rule == "less_equal") {

    idx <- which(delta <= 0)
    if (length(idx) > 0) {
      idx <- idx[which.max(files$start_time_utc[idx])]
    }

  } else if (rule == "greater_equal") {

    idx <- which(delta >= 0)
    if (length(idx) > 0) {
      idx <- idx[which.min(files$start_time_utc[idx])]
    }

  } else if (rule == "less") {

    idx <- which(delta < 0)
    if (length(idx) > 0) {
      idx <- idx[which.max(files$start_time_utc[idx])]
    }

  } else if (rule == "greater") {

    idx <- which(delta > 0)
    if (length(idx) > 0) {
      idx <- idx[which.min(files$start_time_utc[idx])]
    }

  } else if (rule == "nearest") {

    idx <- which.min(abs(delta))

  } else {

    stop(
      "Unknown rule: ",
      rule,
      ". Use one of: exact, less_equal, greater_equal, less, greater, nearest."
    )
  }

  if (length(idx) == 0) {
    return(files[0, , drop = FALSE])
  }

  files[idx[1], , drop = FALSE]
}


# ------------------------------------------------------------------------------
# High-level function: get one candidate file URL
# ------------------------------------------------------------------------------

fn_goes_get_file_candidate <- function(
    position,
    product,
    time_utc,
    rule = "less_equal",
    max_pages = 20
) {

  files <- fn_goes_list_files_for_hour(
    position = position,
    product = product,
    time_utc = time_utc,
    max_pages = max_pages
  )

  fn_goes_select_file(
    files = files,
    target_time_utc = time_utc,
    rule = rule
  )
}


# ------------------------------------------------------------------------------
# Optional: build local destination path
# ------------------------------------------------------------------------------

fn_goes_make_local_path <- function(
    data_raw_dir,
    bucket,
    product_code,
    start_time_utc,
    filename
) {

  start_time_utc <- as.POSIXct(start_time_utc, tz = "UTC")

  year <- format(start_time_utc, "%Y", tz = "UTC")
  julian_day <- format(start_time_utc, "%j", tz = "UTC")
  hour <- format(start_time_utc, "%H", tz = "UTC")

  file.path(
    normalizePath(data_raw_dir, winslash = "/", mustWork = FALSE),
    bucket,
    product_code,
    year,
    julian_day,
    hour,
    filename
  )
}


#########################################################

fn_goes_get_latest_file_online <- function(
    position,
    product,
    time_utc = Sys.time(),
    lookback_hours = 72,
    max_pages = 20
) {

  time_utc <- as.POSIXct(time_utc, tz = "UTC")

  current_hour <- as.POSIXct(
    format(time_utc, "%Y-%m-%d %H:00:00", tz = "UTC"),
    tz = "UTC"
  )

  hours_seq <- seq(
    from = current_hour,
    by = "-1 hour",
    length.out = lookback_hours + 1
  )

  for (h in hours_seq) {

    files <- fn_goes_list_files_for_hour(
      position = position,
      product = product,
      time_utc = h,
      max_pages = max_pages
    )

    if (is.null(files) || nrow(files) == 0) {
      next
    }

    files <- files[
      files$start_time_utc <= time_utc,
      ,
      drop = FALSE
    ]

    if (nrow(files) == 0) {
      next
    }

    files <- files[order(files$start_time_utc, files$file), , drop = FALSE]
    return(files[nrow(files), , drop = FALSE])
  }

  data.frame()
}

#########################################################

fn_goes_get_urls_between <- function(
    position,
    product,
    from_utc,
    to_utc,
    max_pages = 20
) {

  from_utc <- as.POSIXct(from_utc, tz = "UTC")
  to_utc <- as.POSIXct(to_utc, tz = "UTC")

  if (is.na(from_utc) || is.na(to_utc)) {
    stop("from_utc and to_utc must be valid POSIXct dates.")
  }

  if (to_utc < from_utc) {
    stop("to_utc cannot be earlier than from_utc.")
  }

  first_hour <- as.POSIXct(
    format(from_utc, "%Y-%m-%d %H:00:00", tz = "UTC"),
    tz = "UTC"
  )

  last_hour <- as.POSIXct(
    format(to_utc, "%Y-%m-%d %H:00:00", tz = "UTC"),
    tz = "UTC"
  )

  hours_seq <- seq(
    from = first_hour,
    to = last_hour,
    by = "hour"
  )

  out <- lapply(hours_seq, function(h) {
    fn_goes_list_files_for_hour(
      position = position,
      product = product,
      time_utc = h,
      max_pages = max_pages
    )
  })

  out <- out[
    vapply(out, function(x) !is.null(x) && nrow(x) > 0, logical(1))
  ]

  if (length(out) == 0) {
    return(data.frame())
  }

  files <- do.call(rbind, out)

  files <- files[
    files$start_time_utc >= from_utc &
      files$start_time_utc <= to_utc,
    ,
    drop = FALSE
  ]

  if (nrow(files) == 0) {
    return(files)
  }

  files <- files[!duplicated(files$key), , drop = FALSE]
  files <- files[order(files$start_time_utc, files$file), , drop = FALSE]
  rownames(files) <- NULL

  files
}

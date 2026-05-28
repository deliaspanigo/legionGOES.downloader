# ==============================================================================
# MODULO SHINY - GOES DOWNLOADER 04
# Step-based selection + catalog_raw + plan_download + download
# ------------------------------------------------------------------------------
# Requires loading first:
#   source("R/goes_downloader_core.R")
#
# Uso:
#   ui <- bslib::page_fluid(
#     shinyjs::useShinyjs(),
#     mod_goes_downloader_04_ui("goesdl")
#   )
#
#   server <- function(input, output, session) {
#     mod_goes_downloader_04_server(
#       id = "goesdl",
#       str_folder_path_data_raw = "path/to/data_raw"
#     )
#   }
#
#   shiny::shinyApp(ui, server)
# ==============================================================================

goesdl_v04_value_or_default <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }

  if (length(x) == 1 && is.na(x)) {
    return(y)
  }

  x
}

goesdl_v04_nrow_or_zero <- function(x) {
  if (is.null(x) || !is.data.frame(x)) {
    return(0L)
  }

  nrow(x)
}

# ------------------------------------------------------------------------------
# Core validation
# ------------------------------------------------------------------------------

goesdl_v04_required_core_functions <- function() {
  c(
    "goes_product_specs",
    "goes_product_code",
    "goes_make_day_prefix",
    "goes_list_s3_prefix_paginated",
    "goes_resolve_satellite",
    "goes_format_bytes",
    "goes_download_plan"
  )
}

goesdl_v04_check_core <- function() {
  required <- goesdl_v04_required_core_functions()

  core_env <- tryCatch(
    asNamespace("legionGOES.downloader"),
    error = function(e) environment(goesdl_v04_check_core)
  )

  missing <- required[
    !vapply(
      required,
      function(fn) {
        exists(
          fn,
          envir = core_env,
          mode = "function",
          inherits = TRUE
        )
      },
      logical(1)
    )
  ]

  if (length(missing) > 0) {
    stop(
      "Missing core functions: ",
      paste(missing, collapse = ", ")
    )
  }

  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# General module helpers
# ------------------------------------------------------------------------------

goesdl_v04_fmt_hh <- function(x) {
  sprintf("%02d", as.integer(x))
}

goesdl_v04_fmt_mm <- function(x) {
  sprintf("%02d", as.integer(x))
}

goesdl_v04_fmt_ss <- function(x) {
  sprintf("%02d", as.integer(x))
}

goesdl_v04_safe_file_size <- function(path) {
  if (!file.exists(path)) {
    return(NA_real_)
  }

  as.numeric(file.info(path)$size)
}

# Fixed UTC date/time for automatic modes that need to reference.
goesdl_v04_now_utc <- function() {
  as.POSIXct(
    format(Sys.time(), tz = "UTC", usetz = FALSE),
    tz = "UTC"
  )
}

# Current UTC calendar day.
# Do not use Sys.Date() here, because Sys.Date() follows the machine timezone.
goesdl_v04_today_utc <- function() {
  as.Date(goesdl_v04_now_utc(), tz = "UTC")
}

# Format a POSIXct timestamp explicitly in UTC.
goesdl_v04_format_utc <- function(x, fmt = "%Y-%m-%d %H:%M:%S UTC") {
  format(as.POSIXct(x, tz = "UTC"), fmt, tz = "UTC")
}

goesdl_v04_empty_online <- function() {
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

goesdl_v04_parse_goes_start_time <- function(file) {
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

goesdl_v04_make_selected_dates <- function(
    search_mode,
    date_mode,
    single_date,
    date_range
) {
  if (identical(search_mode, "latest")) {
    return(goesdl_v04_today_utc())
  }

  if (identical(date_mode, "single")) {
    return(as.Date(single_date))
  }

  dr <- as.Date(date_range)

  if (length(dr) < 2 || any(is.na(dr))) {
    stop("Invalid date range.")
  }

  seq.Date(
    from = dr[[1]],
    to = dr[[2]],
    by = "day"
  )
}

goesdl_v04_make_target_time_utc <- function(date, hour, minute, second) {
  as.POSIXct(
    paste(
      as.Date(date),
      sprintf(
        "%02d:%02d:%02d",
        as.integer(hour),
        as.integer(minute),
        as.integer(second)
      )
    ),
    tz = "UTC"
  )
}

goesdl_v04_make_range_times_utc <- function(
    date,
    from_hour,
    from_minute,
    from_second,
    to_hour,
    to_minute,
    to_second
) {
  from_time <- goesdl_v04_make_target_time_utc(
    date,
    from_hour,
    from_minute,
    from_second
  )

  to_time <- goesdl_v04_make_target_time_utc(
    date,
    to_hour,
    to_minute,
    to_second
  )

  if (is.na(from_time) || is.na(to_time)) {
    stop("Invalid time range.")
  }

  if (to_time < from_time) {
    stop("The end time cannot be earlier than the start time for the same day.")
  }

  list(
    from = from_time,
    to = to_time
  )
}

goesdl_v04_list_online_product_day <- function(
    product,
    position,
    date,
    max_pages = 50
) {
  product_code <- goes_product_code(product)
  sat <- goes_resolve_satellite(position, date)
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
    return(goesdl_v04_empty_online())
  }

  online$product <- product
  online$product_code <- product_code
  online$position <- position
  online$satellite <- sat$satellite
  online$bucket <- sat$bucket
  online$date <- as.Date(date)
  online$julian_day <- sprintf("%03d", julian_day)
  online$start_time_utc <- goesdl_v04_parse_goes_start_time(online$file)

  online <- online[!is.na(online$start_time_utc), , drop = FALSE]
  online <- online[order(online$start_time_utc, online$file), , drop = FALSE]

  online
}

goesdl_v04_list_online_many <- function(
    products,
    position,
    dates,
    max_pages = 50,
    status_fun = message
) {
  out <- list()
  k <- 1L

  for (d_i in seq_along(dates)) {
    date_now <- as.Date(dates[[d_i]])

    for (p_i in seq_along(products)) {
      product_now <- products[[p_i]]

      status_fun(sprintf(
        "Listing online: %s | %s",
        product_now,
        as.character(date_now)
      ))

      x <- tryCatch(
        goesdl_v04_list_online_product_day(
          product = product_now,
          position = position,
          date = date_now,
          max_pages = max_pages
        ),
        error = function(e) {
          warning(conditionMessage(e))
          data.frame()
        }
      )

      if (!is.null(x) && nrow(x) > 0) {
        out[[k]] <- x
        k <- k + 1L
      }
    }
  }

  if (length(out) == 0) {
    return(data.frame())
  }

  dplyr::bind_rows(out)
}

goesdl_v04_select_exact_or_near <- function(online, target_time, rule) {
  if (is.null(online) || nrow(online) == 0) {
    return(online[0, , drop = FALSE])
  }

  dt <- as.numeric(difftime(
    online$start_time_utc,
    target_time,
    units = "secs"
  ))

  if (identical(rule, "exact")) {
    idx <- which(dt == 0)

    if (length(idx) == 0) {
      return(online[0, , drop = FALSE])
    }

    return(online[idx[1], , drop = FALSE])
  }

  if (identical(rule, "previous")) {
    idx <- which(dt <= 0)

    if (length(idx) == 0) {
      return(online[0, , drop = FALSE])
    }

    idx <- idx[which.max(online$start_time_utc[idx])]
    return(online[idx, , drop = FALSE])
  }

  if (identical(rule, "next")) {
    idx <- which(dt >= 0)

    if (length(idx) == 0) {
      return(online[0, , drop = FALSE])
    }

    idx <- idx[which.min(online$start_time_utc[idx])]
    return(online[idx, , drop = FALSE])
  }

  idx <- which.min(abs(dt))
  online[idx, , drop = FALSE]
}

goesdl_v04_select_first_each_hour <- function(online) {
  if (is.null(online) || nrow(online) == 0) {
    return(online[0, , drop = FALSE])
  }

  online$hour_key <- format(online$start_time_utc, "%Y-%m-%d %H")

  split_online <- split(
    online,
    paste(online$product, online$date, online$hour_key)
  )

  out <- lapply(split_online, function(x) {
    x <- x[order(x$start_time_utc, x$file), , drop = FALSE]
    x[1, , drop = FALSE]
  })

  out <- dplyr::bind_rows(out)
  out$hour_key <- NULL
  out
}

goesdl_v04_select_latest_available <- function(
    products,
    position,
    max_pages = 50,
    status_fun = message,
    reference_time_utc = NULL
) {
  if (is.null(reference_time_utc) || is.na(reference_time_utc)) {
    reference_time_utc <- goesdl_v04_now_utc()
  }

  # Latest available inside the same fixed UTC day.
  dates <- as.Date(reference_time_utc, tz = "UTC")

  online <- goesdl_v04_list_online_many(
    products = products,
    position = position,
    dates = dates,
    max_pages = max_pages,
    status_fun = status_fun
  )

  if (is.null(online) || nrow(online) == 0) {
    selected_empty <- data.frame()
    attr(selected_empty, "online_all") <- data.frame()
    return(selected_empty)
  }

  split_online <- split(online, online$product)

  out <- lapply(split_online, function(x) {
    x <- x[order(x$start_time_utc, x$file), , drop = FALSE]
    x[nrow(x), , drop = FALSE]
  })

  selected <- dplyr::bind_rows(out)
  attr(selected, "online_all") <- online
  selected
}

goesdl_v04_build_selection <- function(
    products,
    position,
    search_mode,
    date_mode,
    single_date,
    date_range,
    time_mode,
    target_hour,
    target_minute,
    target_second,
    point_rule,
    from_hour,
    from_minute,
    from_second,
    to_hour,
    to_minute,
    to_second,
    range_rule,
    max_pages,
    status_fun = message,
    latest_reference_time_utc = NULL
) {
  if (length(products) == 0) {
    stop("Select at least one product.")
  }

  if (identical(search_mode, "latest")) {
    selected <- goesdl_v04_select_latest_available(
      products = products,
      position = position,
      max_pages = max_pages,
      status_fun = status_fun,
      reference_time_utc = latest_reference_time_utc
    )

    if (nrow(selected) > 0) {
      selected$selection_mode <- "latest_available"
      selected$requested_time_utc <- as.POSIXct(
        NA_real_,
        origin = "1970-01-01",
        tz = "UTC"
      )
      selected$time_rule <- "latest"
    }

    return(selected)
  }

  dates <- goesdl_v04_make_selected_dates(
    search_mode = search_mode,
    date_mode = date_mode,
    single_date = single_date,
    date_range = date_range
  )

  online <- goesdl_v04_list_online_many(
    products = products,
    position = position,
    dates = dates,
    max_pages = max_pages,
    status_fun = status_fun
  )

  if (is.null(online) || nrow(online) == 0) {
    selected_empty <- data.frame()
    attr(selected_empty, "online_all") <- data.frame()
    return(selected_empty)
  }

  selected_list <- list()
  k <- 1L

  if (identical(time_mode, "single")) {
    for (d_i in seq_along(dates)) {
      date_now <- as.Date(dates[[d_i]])
      target <- goesdl_v04_make_target_time_utc(
        date_now,
        target_hour,
        target_minute,
        target_second
      )

      for (p_i in seq_along(products)) {
        product_now <- products[[p_i]]
        x <- online[
          online$product == product_now & online$date == date_now,
          ,
          drop = FALSE
        ]

        y <- goesdl_v04_select_exact_or_near(
          online = x,
          target_time = target,
          rule = point_rule
        )

        if (nrow(y) > 0) {
          y$selection_mode <- "single_time"
          y$requested_time_utc <- target
          y$time_rule <- point_rule
          selected_list[[k]] <- y
          k <- k + 1L
        }
      }
    }
  } else {
    for (d_i in seq_along(dates)) {
      date_now <- as.Date(dates[[d_i]])

      rt <- goesdl_v04_make_range_times_utc(
        date = date_now,
        from_hour = from_hour,
        from_minute = from_minute,
        from_second = from_second,
        to_hour = to_hour,
        to_minute = to_minute,
        to_second = to_second
      )

      x <- online[
        online$date == date_now &
          online$start_time_utc >= rt$from &
          online$start_time_utc <= rt$to,
        ,
        drop = FALSE
      ]

      if (nrow(x) > 0) {
        if (identical(range_rule, "first_each_hour")) {
          x <- goesdl_v04_select_first_each_hour(x)
        }

        x$selection_mode <- "time_range"
        x$requested_time_utc <- as.POSIXct(
          NA_real_,
          origin = "1970-01-01",
          tz = "UTC"
        )
        x$time_rule <- range_rule
        selected_list[[k]] <- x
        k <- k + 1L
      }
    }
  }

  if (length(selected_list) == 0) {
    selected_empty <- data.frame()
    attr(selected_empty, "online_all") <- online
    return(selected_empty)
  }

  selected <- dplyr::bind_rows(selected_list)
  selected <- selected[order(selected$product, selected$start_time_utc, selected$file), , drop = FALSE]
  attr(selected, "online_all") <- online
  selected
}


goesdl_v04_make_download_tables <- function(selected, download_dir) {
  if (is.null(selected) || nrow(selected) == 0) {
    empty <- data.frame()
    return(list(
      verification = empty,
      plan = empty
    ))
  }

  download_dir <- normalizePath(
    download_dir,
    winslash = "/",
    mustWork = FALSE
  )

  year <- as.integer(format(as.Date(selected$date), "%Y"))

  destination <- goesdl_v04_data_raw_file_path(
    download_dir = download_dir,
    bucket = selected$bucket,
    product_code = selected$product_code,
    year = year,
    julian_day = selected$julian_day,
    start_time_utc = selected$start_time_utc,
    filename = selected$file
  )

  local_size <- vapply(destination, goesdl_v04_safe_file_size, numeric(1))
  local_found <- file.exists(destination)

  size_match <- local_found &
    !is.na(local_size) &
    !is.na(selected$size_online) &
    local_size == selected$size_online

  action <- ifelse(
    local_found & size_match,
    "OK",
    ifelse(local_found & !size_match, "Delete and Download", "Download")
  )

  expected_stamp <- format(
    selected$start_time_utc,
    "%Y%j%H%M%S",
    tz = "UTC"
  )

  pos <- do.call(
    rbind,
    lapply(seq_len(nrow(selected)), function(i) {
      goesdl_v04_catalog_position_from_time(
        selected$product[[i]],
        selected$start_time_utc[i]
      )
    })
  )

  verification <- data.frame(
    catalog_file_index = pos$catalog_file_index,
    catalog_file_total = pos$catalog_file_total,
    catalog_file_index_label = pos$catalog_file_index_label,
    catalog_file_position_label = pos$catalog_file_position_label,
    product = selected$product,
    product_code = selected$product_code,
    position = selected$position,
    satellite = selected$satellite,
    bucket = selected$bucket,
    year = year,
    julian_day = selected$julian_day,
    date = as.character(selected$date),
    start_time_utc = format(
      selected$start_time_utc,
      "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ),
    selection_mode = selected$selection_mode,
    time_rule = selected$time_rule,
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
    expected_stamp = expected_stamp,
    stringsAsFactors = FALSE
  )

  plan <- verification[
    verification$action %in% c("Download", "Delete and Download"),
    ,
    drop = FALSE
  ]

  if (nrow(plan) > 0) {
    plan$file <- plan$online_file
    plan$bucket <- sub(
      pattern = "^https://([^/]+)\\.s3\\.amazonaws\\.com/.*$",
      replacement = "\\1",
      x = plan$online_url
    )
    plan_total <- nrow(plan)
    labels <- goesdl_v04_make_index_labels(seq_len(plan_total), plan_total)
    plan$plan_file_index <- seq_len(plan_total)
    plan$plan_file_total <- plan_total
    plan$plan_file_index_label <- labels$index_label
    plan$plan_file_position_label <- labels$position_label
  }

  list(
    verification = verification,
    plan = plan
  )
}


# ------------------------------------------------------------------------------
# Persistence V04

# ------------------------------------------------------------------------------
# Persistencia V04: catalog_raw y plan_download
# ------------------------------------------------------------------------------

goesdl_v04_project_root_from_data_raw <- function(data_raw_dir) {
  data_raw_dir <- normalizePath(data_raw_dir, winslash = "/", mustWork = FALSE)
  base_name <- basename(data_raw_dir)

  if (identical(base_name, "data_raw")) {
    return(normalizePath(dirname(data_raw_dir), winslash = "/", mustWork = FALSE))
  }

  normalizePath(dirname(data_raw_dir), winslash = "/", mustWork = FALSE)
}

goesdl_v04_catalog_raw_base_dir <- function(data_raw_dir) {
  file.path(goesdl_v04_project_root_from_data_raw(data_raw_dir), "catalog_raw")
}

goesdl_v04_plan_download_base_dir <- function(data_raw_dir) {
  file.path(goesdl_v04_project_root_from_data_raw(data_raw_dir), "plan_download")
}

goesdl_v04_write_json_safe <- function(x, path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    warning("jsonlite is not installed. JSON writing was skipped: ", path)
    return(FALSE)
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")

  ok <- tryCatch({
    jsonlite::write_json(
      x = x,
      path = tmp,
      dataframe = "rows",
      pretty = TRUE,
      auto_unbox = TRUE,
      na = "null"
    )
    TRUE
  }, error = function(e) {
    warning("Error writing JSON: ", conditionMessage(e))
    FALSE
  })

  if (!ok) {
    if (file.exists(tmp)) unlink(tmp)
    return(FALSE)
  }

  if (file.exists(path)) unlink(path)
  file.rename(tmp, path)
}

goesdl_v04_append_event_jsonl <- function(path, event) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (requireNamespace("jsonlite", quietly = TRUE)) {
    line <- jsonlite::toJSON(event, auto_unbox = TRUE, null = "null")
  } else {
    line <- paste0(
      '{"time_utc":"', event$time_utc, '",',
      '"event":"', event$event, '",',
      '"n_rows":', event$n_rows, '}'
    )
  }

  cat(as.character(line), "\n", file = path, append = TRUE)
  invisible(TRUE)
}


goesdl_v04_expected_total_for_product <- function(product) {
  if (identical(product, "LSTF")) {
    return(24L)
  }
  if (product %in% c("MCMIPF", "FDCF")) {
    return(24L * 6L)
  }
  if (identical(product, "GLM")) {
    return(24L * 60L * 3L)
  }
  NA_integer_
}

goesdl_v04_make_index_labels <- function(index, total) {
  width <- max(nchar(as.character(total)), 2L)
  idx_txt <- sprintf(paste0("%0", width, "d"), as.integer(index))
  list(
    index_label = paste0("file", idx_txt),
    position_label = paste0(idx_txt, " of ", as.integer(total))
  )
}

goesdl_v04_expected_day_grid <- function(
    product,
    product_code,
    position,
    satellite,
    bucket,
    date
) {
  date <- as.Date(date)
  year <- as.integer(format(date, "%Y"))
  julian_day <- sprintf("%03d", as.integer(format(date, "%j")))

  if (identical(product, "LSTF")) {
    times <- as.POSIXct(
      paste(date, sprintf("%02d:00:00", 0:23)),
      tz = "UTC"
    )
  } else if (product %in% c("MCMIPF", "FDCF")) {
    hh <- rep(0:23, each = 6)
    mm <- rep(seq(0, 50, by = 10), times = 24)
    times <- as.POSIXct(
      paste(date, sprintf("%02d:%02d:00", hh, mm)),
      tz = "UTC"
    )
  } else if (identical(product, "GLM")) {
    hh <- rep(0:23, each = 60 * 3)
    mm <- rep(rep(0:59, each = 3), times = 24)
    ss <- rep(c(0, 20, 40), times = 24 * 60)
    times <- as.POSIXct(
      paste(date, sprintf("%02d:%02d:%02d", hh, mm, ss)),
      tz = "UTC"
    )
  } else {
    times <- as.POSIXct(character(), tz = "UTC")
  }

  n <- length(times)
  if (n == 0) {
    return(data.frame())
  }

  labels <- goesdl_v04_make_index_labels(seq_len(n), n)

  data.frame(
    catalog_file_index = seq_len(n),
    catalog_file_total = n,
    catalog_file_index_label = labels$index_label,
    catalog_file_position_label = labels$position_label,
    bucket = bucket,
    satellite = satellite,
    position = position,
    product = product,
    product_code = product_code,
    year = year,
    julian_day = julian_day,
    gregorian_date = as.character(date),
    hour = format(times, "%H", tz = "UTC"),
    minute = format(times, "%M", tz = "UTC"),
    second = format(times, "%S", tz = "UTC"),
    expected_time_utc = format(times, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    start_time_utc_posix = times,
    expected = TRUE,
    stringsAsFactors = FALSE
  )
}

goesdl_v04_catalog_position_from_time <- function(product, start_time_utc) {
  if (is.null(start_time_utc) || length(start_time_utc) == 0) {
    return(data.frame(
      catalog_file_index = integer(),
      catalog_file_total = integer(),
      catalog_file_index_label = character(),
      catalog_file_position_label = character(),
      stringsAsFactors = FALSE
    ))
  }

  h <- as.integer(format(start_time_utc, "%H", tz = "UTC"))
  m <- as.integer(format(start_time_utc, "%M", tz = "UTC"))
  s <- as.integer(format(start_time_utc, "%S", tz = "UTC"))

  total <- goesdl_v04_expected_total_for_product(product)

  if (identical(product, "LSTF")) {
    idx <- h + 1L
  } else if (product %in% c("MCMIPF", "FDCF")) {
    idx <- h * 6L + floor(m / 10L) + 1L
  } else if (identical(product, "GLM")) {
    sec_slot <- match(s, c(0L, 20L, 40L))
    sec_slot[is.na(sec_slot)] <- 1L
    idx <- h * 180L + m * 3L + sec_slot
  } else {
    idx <- seq_along(start_time_utc)
    total <- length(start_time_utc)
  }

  labels <- goesdl_v04_make_index_labels(idx, total)

  data.frame(
    catalog_file_index = as.integer(idx),
    catalog_file_total = as.integer(total),
    catalog_file_index_label = labels$index_label,
    catalog_file_position_label = labels$position_label,
    stringsAsFactors = FALSE
  )
}

# Build the matching key used to connect online files with the expected daily grid.
# Important: ABI products may have real start seconds that are not exactly 00.
# For catalog completeness we match to the logical product slot:
#   LSTF          -> one slot per hour
#   MCMIPF/FDCF  -> one slot per 10-minute interval, ignoring seconds
#   GLM          -> one slot per 20-second interval
# This prevents a file such as *_s2026003000020... from being treated as missing
# just because the expected slot is 00:00:00.
goesdl_v04_catalog_slot_key <- function(product, time_utc) {
  time_utc <- as.POSIXct(time_utc, tz = "UTC")

  yj <- format(time_utc, "%Y%j", tz = "UTC")
  hh <- format(time_utc, "%H", tz = "UTC")
  mm_num <- as.integer(format(time_utc, "%M", tz = "UTC"))
  ss_num <- as.integer(format(time_utc, "%S", tz = "UTC"))

  if (identical(product, "LSTF")) {
    return(paste(yj, hh, sep = "_"))
  }

  if (product %in% c("MCMIPF", "FDCF")) {
    mm_slot <- sprintf("%02d", floor(mm_num / 10) * 10)
    return(paste(yj, hh, mm_slot, sep = "_"))
  }

  if (identical(product, "GLM")) {
    ss_slot <- ifelse(ss_num < 20, 0L, ifelse(ss_num < 40, 20L, 40L))
    return(paste(yj, hh, sprintf("%02d", mm_num), sprintf("%02d", ss_slot), sep = "_"))
  }

  format(time_utc, "%Y%j%H%M%S", tz = "UTC")
}


goesdl_v04_data_raw_file_path <- function(
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



goesdl_v04_make_catalog_raw_table <- function(online_all, download_dir, input_config = NULL) {
  download_dir <- normalizePath(download_dir, winslash = "/", mustWork = FALSE)
  checked_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  has_online <- !is.null(online_all) && is.data.frame(online_all) && nrow(online_all) > 0

  # Build the full product/day universe from the locked input configuration when possible.
  # This lets catalog_raw represent expected files even when no files are currently online.
  if (!is.null(input_config) && !is.null(input_config$products) && !is.null(input_config$position)) {
    dates <- if (identical(input_config$search_mode, "latest")) {
      ref <- input_config$latest_fixed_utc
      if (is.null(ref) || length(ref) == 0 || is.na(ref)) {
        goesdl_v04_today_utc()
      } else {
        as.Date(as.POSIXct(ref, tz = "UTC"), tz = "UTC")
      }
    } else {
      goesdl_v04_make_selected_dates(
        search_mode = input_config$search_mode,
        date_mode = input_config$date_mode,
        single_date = input_config$single_date,
        date_range = input_config$date_range
      )
    }

    product_dates <- expand.grid(
      product = input_config$products,
      date = as.Date(dates),
      stringsAsFactors = FALSE
    )
  } else if (has_online) {
    product_dates <- unique(data.frame(
      product = online_all$product,
      date = as.Date(online_all$date),
      stringsAsFactors = FALSE
    ))
  } else {
    return(data.frame())
  }

  out <- lapply(seq_len(nrow(product_dates)), function(i) {
    product <- product_dates$product[[i]]
    date <- as.Date(product_dates$date[[i]])
    product_code <- goes_product_code(product)
    position_input <- if (!is.null(input_config) && !is.null(input_config$position)) input_config$position else "EAST"
    sat <- goes_resolve_satellite(position_input, date)
    position <- position_input
    satellite <- sat$satellite
    bucket <- sat$bucket

    grid <- goesdl_v04_expected_day_grid(
      product = product,
      product_code = product_code,
      position = position,
      satellite = satellite,
      bucket = bucket,
      date = date
    )

    if (nrow(grid) == 0) {
      return(data.frame())
    }

    if (has_online) {
      online <- online_all[
        online_all$product == product & as.Date(online_all$date) == date,
        ,
        drop = FALSE
      ]
    } else {
      online <- data.frame()
    }

    if (!is.null(online) && nrow(online) > 0) {
      online <- online[order(online$start_time_utc, online$file), , drop = FALSE]
      grid$match_slot_key <- goesdl_v04_catalog_slot_key(product, grid$start_time_utc_posix)
      online$match_slot_key <- goesdl_v04_catalog_slot_key(product, online$start_time_utc)
      online <- online[!duplicated(online$match_slot_key), , drop = FALSE]
      m <- match(grid$match_slot_key, online$match_slot_key)
      online_found <- !is.na(m)
    } else {
      m <- rep(NA_integer_, nrow(grid))
      online_found <- rep(FALSE, nrow(grid))
    }

    online_key <- rep(NA_character_, nrow(grid))
    online_filename <- rep(NA_character_, nrow(grid))
    online_url <- rep(NA_character_, nrow(grid))
    online_size_bytes <- rep(NA_real_, nrow(grid))
    online_start_time_utc <- rep(NA_character_, nrow(grid))

    if (any(online_found)) {
      online_key[online_found] <- online$key[m[online_found]]
      online_filename[online_found] <- online$file[m[online_found]]
      online_url[online_found] <- online$url[m[online_found]]
      online_size_bytes[online_found] <- online$size_online[m[online_found]]
      online_start_time_utc[online_found] <- format(
        online$start_time_utc[m[online_found]],
        "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
      )
    }

    destination <- rep(NA_character_, nrow(grid))
    if (any(online_found)) {
      destination[online_found] <- goesdl_v04_data_raw_file_path(
        download_dir = download_dir,
        bucket = bucket,
        product_code = product_code,
        year = grid$year[online_found],
        julian_day = grid$julian_day[online_found],
        start_time_utc = grid$start_time_utc_posix[online_found],
        filename = online_filename[online_found]
      )
    }

    local_found <- !is.na(destination) & file.exists(destination)
    local_size <- rep(NA_real_, nrow(grid))
    if (any(local_found)) {
      local_size[local_found] <- vapply(destination[local_found], goesdl_v04_safe_file_size, numeric(1))
    }

    size_match <- local_found & !is.na(local_size) & !is.na(online_size_bytes) & local_size == online_size_bytes

    now_utc <- goesdl_v04_now_utc()
    is_future <- grid$start_time_utc_posix > now_utc

    catalog_status <- ifelse(
      online_found & local_found,
      "expected_online_local",
      ifelse(
        online_found & !local_found,
        "expected_online_missing_local",
        ifelse(is_future, "expected_future_time", "expected_missing_online")
      )
    )

    data.frame(
      catalog_file_index = grid$catalog_file_index,
      catalog_file_total = grid$catalog_file_total,
      catalog_file_index_label = grid$catalog_file_index_label,
      catalog_file_position_label = grid$catalog_file_position_label,
      bucket = grid$bucket,
      satellite = grid$satellite,
      position = grid$position,
      product = grid$product,
      product_code = grid$product_code,
      year = grid$year,
      julian_day = grid$julian_day,
      gregorian_date = grid$gregorian_date,
      hour = grid$hour,
      minute = grid$minute,
      second = grid$second,
      expected_time_utc = grid$expected_time_utc,
      expected = TRUE,
      online_found = online_found,
      online_key = online_key,
      online_filename = online_filename,
      online_url = online_url,
      online_size_bytes = online_size_bytes,
      online_start_time_utc = online_start_time_utc,
      local_found = local_found,
      local_path = ifelse(local_found, destination, NA_character_),
      local_size_bytes = local_size,
      size_match = ifelse(local_found, size_match, NA),
      catalog_status = catalog_status,
      last_checked_at_utc = checked_at,
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(out)
}

goesdl_v04_catalog_group_dir <- function(catalog_base_dir, bucket, product_code, year, julian_day) {
  file.path(
    catalog_base_dir,
    bucket,
    product_code,
    sprintf("%04d", as.integer(year)),
    sprintf("%03d", as.integer(julian_day))
  )
}

goesdl_v04_write_catalog_raw <- function(
    online_all,
    download_dir,
    catalog_base_dir,
    input_config = NULL,
    event = "catalog_updated"
) {
  catalog <- goesdl_v04_make_catalog_raw_table(
    online_all = online_all,
    download_dir = download_dir,
    input_config = input_config
  )

  if (is.null(catalog) || nrow(catalog) == 0) {
    return(data.frame())
  }

  catalog_base_dir <- normalizePath(catalog_base_dir, winslash = "/", mustWork = FALSE)
  split_key <- paste(catalog$bucket, catalog$product_code, catalog$year, catalog$julian_day, sep = "|")
  groups <- split(catalog, split_key)

  manifest <- lapply(groups, function(x) {
    x <- x[order(x$expected_time_utc, x$online_filename), , drop = FALSE]
    bucket <- x$bucket[[1]]
    product_code <- x$product_code[[1]]
    year <- x$year[[1]]
    julian_day <- x$julian_day[[1]]

    out_dir <- goesdl_v04_catalog_group_dir(
      catalog_base_dir = catalog_base_dir,
      bucket = bucket,
      product_code = product_code,
      year = year,
      julian_day = julian_day
    )

    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    csv_file <- file.path(out_dir, "catalog.csv")
    json_file <- file.path(out_dir, "catalog.json")
    events_file <- file.path(out_dir, "events.jsonl")

    utils::write.csv(x, csv_file, row.names = FALSE, fileEncoding = "UTF-8")

    json_payload <- list(
      metadata = list(
        type = "catalog_raw",
        bucket = bucket,
        product_code = product_code,
        year = as.integer(year),
        julian_day = sprintf("%03d", as.integer(julian_day)),
        updated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        n_files = nrow(x),
        input_config = input_config
      ),
      files = x
    )

    goesdl_v04_write_json_safe(json_payload, json_file)

    goesdl_v04_append_event_jsonl(
      events_file,
      list(
        time_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        event = event,
        n_rows = nrow(x),
        bucket = bucket,
        product_code = product_code,
        year = as.integer(year),
        julian_day = sprintf("%03d", as.integer(julian_day))
      )
    )

    data.frame(
      bucket = bucket,
      product_code = product_code,
      year = as.integer(year),
      julian_day = sprintf("%03d", as.integer(julian_day)),
      n_files = nrow(x),
      catalog_dir = normalizePath(out_dir, winslash = "/", mustWork = FALSE),
      catalog_csv = normalizePath(csv_file, winslash = "/", mustWork = FALSE),
      catalog_json = normalizePath(json_file, winslash = "/", mustWork = FALSE),
      events_jsonl = normalizePath(events_file, winslash = "/", mustWork = FALSE),
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(manifest)
}


goesdl_v04_next_plan_id <- function(plan_base_dir) {
  plan_base_dir <- normalizePath(plan_base_dir, winslash = "/", mustWork = FALSE)
  dir.create(plan_base_dir, recursive = TRUE, showWarnings = FALSE)

  existing <- list.dirs(plan_base_dir, recursive = FALSE, full.names = FALSE)
  nums <- sub("^plan_([0-9]{9}).*$", "\\1", existing)
  nums <- suppressWarnings(as.integer(nums[grepl("^[0-9]{9}$", nums)]))

  next_num <- if (length(nums) == 0 || all(is.na(nums))) {
    1L
  } else {
    max(nums, na.rm = TRUE) + 1L
  }

  sprintf(
    "plan_%09d_%s",
    next_num,
    format(Sys.time(), "%Y%m%d_%H%M%S", tz = "UTC")
  )
}

goesdl_v04_plan_group_dir <- function(plan_base_dir, verification, plan_id) {
  file.path(plan_base_dir, plan_id)
}

goesdl_v04_write_plan_download <- function(
    verification,
    plan,
    input_config,
    data_raw_dir,
    catalog_manifest,
    plan_base_dir
) {
  plan_base_dir <- normalizePath(plan_base_dir, winslash = "/", mustWork = FALSE)
  dir.create(plan_base_dir, recursive = TRUE, showWarnings = FALSE)

  plan_id <- goesdl_v04_next_plan_id(plan_base_dir)
  plan_dir <- goesdl_v04_plan_group_dir(plan_base_dir, verification, plan_id)
  dir.create(plan_dir, recursive = TRUE, showWarnings = FALSE)

  verification_file_csv <- file.path(plan_dir, "verification.csv")
  plan_file_csv <- file.path(plan_dir, "plan.csv")
  plan_file_json <- file.path(plan_dir, "plan.json")
  status_file_json <- file.path(plan_dir, "status.json")
  log_file <- file.path(plan_dir, "download.log")

  if (is.null(verification)) verification <- data.frame()
  if (is.null(plan)) plan <- data.frame()

  utils::write.csv(verification, verification_file_csv, row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(plan, plan_file_csv, row.names = FALSE, fileEncoding = "UTF-8")

  status_table <- if (nrow(plan) > 0) {
    data.frame(
      product = plan$product,
      file = plan$online_file,
      destination = plan$destination,
      status = rep("pending", nrow(plan)),
      detail = rep("", nrow(plan)),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      product = character(),
      file = character(),
      destination = character(),
      status = character(),
      detail = character(),
      stringsAsFactors = FALSE
    )
  }

  payload <- list(
    metadata = list(
      type = "plan_download",
      plan_id = plan_id,
      created_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      data_raw_dir = normalizePath(data_raw_dir, winslash = "/", mustWork = FALSE),
      n_selected = nrow(verification),
      n_to_download = nrow(plan),
      input_config = input_config,
      catalog_manifest = catalog_manifest
    ),
    verification = verification,
    files_to_download = plan
  )

  goesdl_v04_write_json_safe(payload, plan_file_json)

  status_payload <- list(
    metadata = list(
      type = "download_status",
      plan_id = plan_id,
      created_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      running = FALSE,
      done = FALSE,
      total = nrow(plan),
      n_ok = 0L,
      n_error = 0L
    ),
    files = status_table
  )

  goesdl_v04_write_json_safe(status_payload, status_file_json)
  writeLines(character(), log_file, useBytes = TRUE)

  list(
    plan_id = plan_id,
    plan_dir = normalizePath(plan_dir, winslash = "/", mustWork = FALSE),
    verification_csv = normalizePath(verification_file_csv, winslash = "/", mustWork = FALSE),
    plan_csv = normalizePath(plan_file_csv, winslash = "/", mustWork = FALSE),
    plan_json = normalizePath(plan_file_json, winslash = "/", mustWork = FALSE),
    status_json = normalizePath(status_file_json, winslash = "/", mustWork = FALSE),
    download_log = normalizePath(log_file, winslash = "/", mustWork = FALSE)
  )
}

# ==============================================================================
# UI MODULE
# ==============================================================================

mod_goes_downloader_04_ui <- function(id) {
  goesdl_v04_check_core()

  ns <- shiny::NS(id)
  product_specs <- goes_product_specs()

  product_reference_table <- function() {
    shiny::tags$table(
      class = "table table-sm table-bordered product-reference-table",
      shiny::tags$thead(
        shiny::tags$tr(
          shiny::tags$th("Use"),
          shiny::tags$th("Product"),
          shiny::tags$th("Code"),
          shiny::tags$th("Frequency")
        )
      ),
      shiny::tags$tbody(
        lapply(seq_len(nrow(product_specs)), function(i) {
          product <- product_specs$product[[i]]

          shiny::tags$tr(
            shiny::tags$td(
              shiny::checkboxInput(
                inputId = ns(paste0("use_", product)),
                label = NULL,
                value = identical(product, "FDCF"),
                width = "24px"
              )
            ),
            shiny::tags$td(product),
            shiny::tags$td(product_specs$product_code[[i]]),
            shiny::tags$td(product_specs$frequency_label[[i]])
          )
        })
      )
    )
  }

  time_selectors_single <- function() {
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::selectInput(
            inputId = ns("target_hour"),
            label = "UTC hour",
            choices = goesdl_v04_fmt_hh(0:23),
            selected = goesdl_v04_fmt_hh(as.integer(format(goesdl_v04_now_utc(), "%H", tz = "UTC")))
          )
        ),
        shiny::column(
          width = 4,
          shiny::selectInput(
            inputId = ns("target_minute"),
            label = "UTC minute",
            choices = goesdl_v04_fmt_mm(0:59),
            selected = "00"
          )
        ),
        shiny::column(
          width = 4,
          shiny::selectInput(
            inputId = ns("target_second"),
            label = "UTC second",
            choices = goesdl_v04_fmt_ss(0:59),
            selected = "00"
          )
        )
      ),
      shiny::radioButtons(
        inputId = ns("point_rule"),
        label = "File selection rule",
        choices = c(
          "Nearest" = "nearest",
          "Previous available" = "previous",
          "Next available" = "next",
          "Exact" = "exact"
        ),
        selected = "nearest"
      )
    )
  }

  time_selectors_range <- function() {
    time_range_row <- function(row_label, prefix, hour_selected, minute_selected, second_selected) {
      shiny::div(
        class = "goes-time-range-row",
        shiny::div(class = "goes-time-range-label", row_label),
        shiny::div(
          class = "goes-time-range-field",
          shiny::selectInput(
            inputId = ns(paste0(prefix, "_hour")),
            label = "Hour",
            choices = goesdl_v04_fmt_hh(0:23),
            selected = hour_selected,
            width = "100%"
          )
        ),
        shiny::div(
          class = "goes-time-range-field",
          shiny::selectInput(
            inputId = ns(paste0(prefix, "_minute")),
            label = "Minute",
            choices = goesdl_v04_fmt_mm(0:59),
            selected = minute_selected,
            width = "100%"
          )
        ),
        shiny::div(
          class = "goes-time-range-field",
          shiny::selectInput(
            inputId = ns(paste0(prefix, "_second")),
            label = "Second",
            choices = goesdl_v04_fmt_ss(0:59),
            selected = second_selected,
            width = "100%"
          )
        )
      )
    }

    shiny::tagList(
      time_range_row(
        row_label = "From",
        prefix = "from",
        hour_selected = "00",
        minute_selected = "00",
        second_selected = "00"
      ),
      time_range_row(
        row_label = "To",
        prefix = "to",
        hour_selected = "23",
        minute_selected = "59",
        second_selected = "59"
      ),
      shiny::radioButtons(
        inputId = ns("range_rule"),
        label = "Inside the range",
        choices = c(
          "All files" = "all_files",
          "First file of each hour" = "first_each_hour"
        ),
        selected = "all_files"
      )
    )
  }

  shiny::tagList(
    shinyjs::useShinyjs(),

    shiny::tags$head(
      shiny::tags$style(shiny::HTML("\n        .goes-downloader-root {\n          position: relative !important;\n          width: 100% !important;\n          min-height: 100vh !important;\n          height: 100% !important;\n          margin: 0 !important;\n          padding: 0 !important;\n          background: #f2f4f7 !important;\n          overflow-x: hidden !important;\n          box-sizing: border-box !important;\n        }\n\n        .goes-downloader-root > .app-title {\n          font-size: 28px !important;\n          font-weight: 800 !important;\n          margin-top: 18px !important;\n          margin-bottom: 4px !important;\n          margin-left: 16px !important;\n          margin-right: 16px !important;\n        }\n\n        .goes-downloader-root > .app-subtitle {\n          color: #666 !important;\n          margin-bottom: 18px !important;\n          margin-left: 16px !important;\n          margin-right: 16px !important;\n        }\n\n        .goes-downloader-root > .tabbable {\n          width: 100% !important;\n          min-height: calc(100vh - 82px) !important;\n          background: #f2f4f7 !important;\n          box-sizing: border-box !important;\n        }\n\n        .goes-downloader-root > .tabbable > .nav-tabs {\n          margin-left: 0 !important;\n          margin-right: 0 !important;\n          padding-left: 16px !important;\n          padding-right: 16px !important;\n          background: #f2f4f7 !important;\n          box-sizing: border-box !important;\n        }\n\n        .goes-downloader-root > .tabbable > .tab-content {\n          width: 100% !important;\n          min-height: calc(100vh - 130px) !important;\n          background: #f2f4f7 !important;\n          padding: 16px !important;\n          box-sizing: border-box !important;\n        }\n\n        .goes-downloader-root > .tabbable > .tab-content > .tab-pane {\n          min-height: calc(100vh - 162px) !important;\n          background: #f2f4f7 !important;\n          box-sizing: border-box !important;\n        }\n\n        .goes-downloader-root .row {\n          margin-left: 0 !important;\n          margin-right: 0 !important;\n        }\n\n        .goes-downloader-root [class^='col-'],\n        .goes-downloader-root [class*=' col-'] {\n          padding-left: 8px !important;\n          padding-right: 8px !important;\n          box-sizing: border-box !important;\n        }\n\n        .goes-downloader-root .card-box {\n          background: white !important;\n          border-radius: 14px !important;\n          padding: 16px !important;\n          margin-bottom: 16px !important;\n          box-shadow: 0 2px 12px rgba(0,0,0,0.08) !important;\n          box-sizing: border-box !important;\n        }\n\n        .goes-downloader-root .goes-clock-box {\n          background: #071018 !important;\n          color: #b8f7c1 !important;\n          border-radius: 12px !important;\n          padding: 10px 14px !important;\n          font-family: Consolas, monospace !important;\n          font-size: 14px !important;\n          margin-bottom: 16px !important;\n          box-sizing: border-box !important;\n        }\n\n        .goes-downloader-root .goes-muted {\n          color: #666 !important;\n          font-size: 13px !important;\n        }\n\n        .goes-downloader-root .goes-path,\n        .goes-downloader-root .summary-box {\n          background: #eef6ff !important;\n          border-left: 5px solid #2c7be5 !important;\n          padding: 12px !important;\n          border-radius: 8px !important;\n          font-family: Consolas, monospace !important;\n          font-size: 13px !important;\n          white-space: pre-wrap !important;\n          overflow-x: auto !important;\n          box-sizing: border-box !important;\n        }\n\n        .goes-downloader-root .download-log-box {\n          background: #071018 !important;\n          color: #b8f7c1 !important;\n          border-radius: 10px !important;\n          padding: 12px !important;\n          height: 320px !important;\n          overflow-y: auto !important;\n          font-family: Consolas, monospace !important;\n          font-size: 13px !important;\n          white-space: pre-wrap !important;\n          box-sizing: border-box !important;\n        }\n\n        .goes-downloader-root .product-reference-table {\n          width: 100% !important;\n          font-size: 12px !important;\n          margin-bottom: 0 !important;\n        }\n\n        .goes-downloader-root .product-reference-table th,\n        .goes-downloader-root .product-reference-table td {\n          padding: 5px !important;\n          vertical-align: middle !important;\n        }\n\n        .goes-downloader-root .product-reference-table .form-group,\n        .goes-downloader-root .product-reference-table .checkbox {\n          margin: 0 !important;\n        }\n\n        .goes-downloader-root .goes-input-lock-row {
          display: flex !important;
          align-items: center !important;
          gap: 12px !important;
        }

        .goes-downloader-root .goes-input-lock-row .btn {
          min-width: 160px !important;
          font-weight: 700 !important;
        }

        .goes-downloader-root .goes-input-lock-box pre {
          margin-top: 12px !important;
          margin-bottom: 0 !important;
          background: #f8f9fa !important;
          border: 1px solid #e9ecef !important;
          border-radius: 8px !important;
          padding: 10px !important;
          font-size: 12px !important;
        }

        .goes-downloader-root .goes-time-range-row {
          display: grid !important;
          grid-template-columns: 72px minmax(0, 1fr) minmax(0, 1fr) minmax(0, 1fr) !important;
          gap: 10px !important;
          align-items: end !important;
          margin-bottom: 8px !important;
          padding: 0 !important;
          background: transparent !important;
          border: none !important;
          border-radius: 0 !important;
        }

        .goes-downloader-root .goes-time-range-label {
          font-size: 12px !important;
          font-weight: 800 !important;
          color: #333 !important;
          padding-bottom: 8px !important;
          text-transform: uppercase !important;
          letter-spacing: 0.3px !important;
        }

        .goes-downloader-root .goes-time-range-field .form-group {
          margin-bottom: 0 !important;
        }

        .goes-downloader-root .goes-time-range-field label {
          font-size: 11px !important;
          margin-bottom: 2px !important;
          color: #555 !important;
          font-weight: 600 !important;
        }

        .goes-downloader-root .goes-frozen-time-note {\n          background: #f0fff4 !important;\n          border-left: 5px solid #2f9e44 !important;\n          padding: 10px 12px !important;\n          border-radius: 8px !important;\n          font-family: Consolas, monospace !important;\n          font-size: 13px !important;\n          white-space: pre-wrap !important;\n          margin-bottom: 10px !important;\n        }\n      ")),

      shiny::tags$script(shiny::HTML(sprintf("\n        (function() {\n          const utcId = '%s';\n          const localId = '%s';\n\n          function updateGoesClocks() {\n            const now = new Date();\n\n            const utc = now.getUTCFullYear() + '-' +\n              String(now.getUTCMonth() + 1).padStart(2, '0') + '-' +\n              String(now.getUTCDate()).padStart(2, '0') + ' ' +\n              String(now.getUTCHours()).padStart(2, '0') + ':' +\n              String(now.getUTCMinutes()).padStart(2, '0') + ':' +\n              String(now.getUTCSeconds()).padStart(2, '0') + ' UTC';\n\n            const local = now.getFullYear() + '-' +\n              String(now.getMonth() + 1).padStart(2, '0') + '-' +\n              String(now.getDate()).padStart(2, '0') + ' ' +\n              String(now.getHours()).padStart(2, '0') + ':' +\n              String(now.getMinutes()).padStart(2, '0') + ':' +\n              String(now.getSeconds()).padStart(2, '0') + ' local';\n\n            const utcEl = document.getElementById(utcId);\n            const localEl = document.getElementById(localId);\n\n            if (utcEl) utcEl.textContent = utc;\n            if (localEl) localEl.textContent = local;\n          }\n\n          setInterval(updateGoesClocks, 1000);\n          document.addEventListener('DOMContentLoaded', updateGoesClocks);\n          updateGoesClocks();\n        })();\n      ", ns("clock_utc"), ns("clock_local"))))
    ),

    shiny::div(
      id = ns("goes_downloader_root"),
      class = "goes-downloader-root",

      shiny::div(class = "app-title", "LegionGOES Downloader"),
      shiny::div(
        class = "app-subtitle",
        "Single temporal selection, raw catalog, download plan and download. The destination folder is defined by the main app and is not modified here."
      ),

      bslib::layout_columns(
        col_widths = c(6, 6),
        shiny::div(
          class = "goes-clock-box",
          shiny::strong("UTC time"),
          shiny::br(),
          shiny::span(id = ns("clock_utc"))
        ),
        shiny::div(
          class = "goes-clock-box",
          shiny::strong("Local system time"),
          shiny::br(),
          shiny::span(id = ns("clock_local"))
        )
      ),

      shiny::tabsetPanel(
        id = ns("main_tabs"),
        type = "tabs",

        shiny::tabPanel(
          title = "1. Satellite",
          value = "tab_satellite",
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 5,
              shiny::div(
                class = "card-box goes-input-lock-box",
                shiny::div(
                  class = "goes-input-lock-row",
                  shiny::uiOutput(ns("satellite_lock_button")),
                  shiny::div(
                    class = "goes-muted",
                    "Close the lock to freeze the satellite position. Opening it clears downstream steps."
                  )
                ),
                shiny::verbatimTextOutput(ns("satellite_summary"))
              ),
              shiny::div(
                class = "card-box",
                shiny::h4("Satellite"),
                shiny::selectInput(
                  inputId = ns("position"),
                  label = "Satellite position",
                  choices = c("EAST", "WEST"),
                  selected = "EAST"
                )
              )
            ),
            shiny::column(
              width = 7,
              shiny::div(
                class = "card-box",
                shiny::h4("State"),
                shiny::div(
                  class = "summary-box",
                  "Step 1: select EAST or WEST and close the lock.\nThen continue with Products."
                )
              )
            )
          )
        ),

        shiny::tabPanel(
          title = "2. Products",
          value = "tab_products",
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 5,
              shiny::div(
                class = "card-box goes-input-lock-box",
                shiny::div(
                  class = "goes-input-lock-row",
                  shiny::uiOutput(ns("products_lock_button")),
                  shiny::div(
                    class = "goes-muted",
                    "Close the lock to freeze products. Opening it clears downstream steps."
                  )
                ),
                shiny::verbatimTextOutput(ns("products_summary"))
              )
            ),
            shiny::column(
              width = 7,
              shiny::div(
                class = "card-box",
                shiny::h4("Satellite products"),
                product_reference_table()
              )
            )
          )
        ),

        shiny::tabPanel(
          title = "3. Time",
          value = "tab_time",
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::div(
                class = "card-box goes-input-lock-box",
                shiny::div(
                  class = "goes-input-lock-row",
                  shiny::uiOutput(ns("time_lock_button")),
                  shiny::div(
                    class = "goes-muted",
                    "Close the lock to freeze the time criteria. Opening it clears downstream steps."
                  )
                ),
                shiny::verbatimTextOutput(ns("time_summary"))
              )
            ),
            shiny::column(
              width = 8,
              shiny::div(
                class = "card-box",
                shiny::h4("Search time"),
                shiny::radioButtons(
                  inputId = ns("search_mode"),
                  label = "Main mode",
                  choices = c(
                    "Latest online file available" = "latest",
                    "Define search time" = "defined"
                  ),
                  selected = "defined"
                ),
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] == 'latest'", ns("search_mode")),
                  shiny::uiOutput(ns("latest_fixed_utc_box")),
                  shiny::div(
                    class = "goes-muted",
                    "The UTC date/time is fixed when this mode is selected. The latest online file is searched only inside that UTC day."
                  )
                ),
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] == 'defined'", ns("search_mode")),
                  shiny::hr(),
                  shiny::h5("UTC date"),
                  shiny::radioButtons(
                    inputId = ns("date_mode"),
                    label = "UTC date type",
                    choices = c("Single day" = "single", "Date range" = "range"),
                    selected = "single",
                    inline = TRUE
                  ),
                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] == 'single'", ns("date_mode")),
                    shiny::dateInput(
                      inputId = ns("single_date"),
                      label = "UTC day",
                      value = goesdl_v04_today_utc()
                    )
                  ),
                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] == 'range'", ns("date_mode")),
                    shiny::dateRangeInput(
                      inputId = ns("date_range"),
                      label = "UTC date range",
                      start = goesdl_v04_today_utc(),
                      end = goesdl_v04_today_utc()
                    )
                  ),
                  shiny::hr(),
                  shiny::h5("UTC time"),
                  shiny::radioButtons(
                    inputId = ns("time_mode"),
                    label = "Time type",
                    choices = c("Single time" = "single", "Time range" = "range"),
                    selected = "single",
                    inline = TRUE
                  ),
                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] == 'single'", ns("time_mode")),
                    time_selectors_single()
                  ),
                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] == 'range'", ns("time_mode")),
                    time_selectors_range()
                  )
                ),
                shiny::div(class = "goes-muted", "S3 query: fixed internal maximum of 20 pages per product/day.")
              )
            )
          )
        ),

        shiny::tabPanel(
          title = "4. Catalog",
          value = "tab_catalog",
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::div(
                class = "card-box",
                shiny::h4("Selected inputs"),
                shiny::verbatimTextOutput(ns("locked_inputs_summary")),
                shiny::hr(),
                shiny::h5("Project folders"),
                shiny::div(
                  class = "goes-path",
                  shiny::textOutput(ns("download_dir_preview_catalog"))
                ),
                shiny::br(),
                shiny::actionButton(
                  inputId = ns("generate_catalog"),
                  label = "Build / update catalog",
                  class = "btn btn-primary"
                ),
                shiny::actionButton(
                  inputId = ns("clear_plan"),
                  label = "Clear catalog and plan",
                  class = "btn btn-outline-warning"
                )
              ),
              shiny::div(
                class = "card-box",
                shiny::h4("Catalog summary"),
                shiny::verbatimTextOutput(ns("catalog_summary"))
              )
            ),
            shiny::column(
              width = 8,
              shiny::div(
                class = "card-box",
                shiny::h4("catalog_raw files updated"),
                shiny::div(
                  class = "goes-muted",
                  "The catalog is written by bucket/product/year/julian day. It records the full product/day grid, online availability and local presence."
                ),
                shiny::br(),
                shiny::tableOutput(ns("catalog_manifest_table"))
              ),
              shiny::div(
                class = "card-box",
                shiny::h4("Catalog completeness"),
                shiny::div(
                  class = "goes-muted",
                  "Summary of the current catalog rows created or updated in this run."
                ),
                shiny::br(),
                shiny::tableOutput(ns("catalog_completeness_table"))
              )
            )
          )
        ),

        shiny::tabPanel(
          title = "5. Download Plan",
          value = "tab_download_plan",
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::div(
                class = "card-box goes-input-lock-box",
                shiny::div(
                  class = "goes-input-lock-row",
                  shiny::uiOutput(ns("local_online_lock_button")),
                  shiny::div(
                    class = "goes-muted",
                    "Close the lock to freeze the download plan before downloading."
                  )
                ),
                shiny::verbatimTextOutput(ns("local_online_lock_summary"))
              ),
              shiny::div(
                class = "card-box",
                shiny::h4("Download plan"),
                shiny::div(
                  class = "goes-muted",
                  "Create a download plan from the catalog and the locked time selection. The worker will only download files in this frozen plan."
                ),
                shiny::hr(),
                shiny::h5("Fixed download folder"),
                shiny::div(
                  class = "goes-path",
                  shiny::textOutput(ns("download_dir_preview_plan"))
                ),
                shiny::br(),
                shiny::actionButton(
                  inputId = ns("generate_plan"),
                  label = "Create download plan",
                  class = "btn btn-primary"
                )
              ),
              shiny::div(
                class = "card-box",
                shiny::h4("General summary"),
                shiny::verbatimTextOutput(ns("selection_summary"))
              )
            ),
            shiny::column(
              width = 8,
              shiny::div(
                class = "card-box",
                shiny::h4("Summary by product"),
                shiny::div(
                  class = "goes-muted",
                  "Summary computed from the frozen download plan: online selected, local files found, and pending downloads."
                ),
                shiny::br(),
                shiny::tableOutput(ns("product_plan_summary_table"))
              ),
              shiny::div(
                class = "card-box",
                shiny::h4("First and last file by product"),
                shiny::div(
                  class = "goes-muted",
                  "For each product, the first and last selected UTC time are shown, together with the first and last .nc file."
                ),
                shiny::br(),
                shiny::tableOutput(ns("product_first_last_table"))
              ),
              shiny::div(
                class = "card-box",
                shiny::h4("Selected files and local verification"),
                DT::DTOutput(ns("verification_table"))
              )
            )
          )
        ),

        shiny::tabPanel(
          title = "6. Download",
          value = "tab_download",
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::div(
                class = "card-box",
                shiny::h4("Download"),
                shiny::verbatimTextOutput(ns("download_summary")),
                shiny::actionButton(
                  inputId = ns("start_download"),
                  label = "Download",
                  class = "btn btn-success"
                ),
                shiny::actionButton(
                  inputId = ns("clear_download"),
                  label = "Clear log",
                  class = "btn btn-outline-secondary"
                )
              )
            ),
            shiny::column(
              width = 8,
              shiny::div(
                class = "card-box",
                shiny::h4("Download log"),
                shiny::div(
                  class = "download-log-box",
                  shiny::textOutput(ns("download_log"), container = shiny::span)
                )
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::div(
                class = "card-box",
                shiny::h4("Download status"),
                DT::DTOutput(ns("download_status_table"))
              )
            )
          )
        )
      )
    )
  )
}


# ==============================================================================
# SERVER MODULE
# ==============================================================================

mod_goes_downloader_04_server <- function(id, str_folder_path_data_raw) {
  goesdl_v04_check_core()

  shiny::moduleServer(id, function(input, output, session) {
    product_specs <- goes_product_specs()
    product_choices <- product_specs$product
    fixed_max_pages <- 20L

    default_download_dir <- normalizePath(
      str_folder_path_data_raw,
      winslash = "/",
      mustWork = FALSE
    )

    project_root_dir <- goesdl_v04_project_root_from_data_raw(default_download_dir)
    catalog_raw_base_dir <- goesdl_v04_catalog_raw_base_dir(default_download_dir)
    plan_download_base_dir <- goesdl_v04_plan_download_base_dir(default_download_dir)

    dir.create(default_download_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(catalog_raw_base_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(plan_download_base_dir, recursive = TRUE, showWarnings = FALSE)

    rv <- shiny::reactiveValues(
      selected_online = NULL,
      verification = NULL,
      plan = NULL,
      download_status = NULL,
      download_log = character(),
      download_job = NULL,
      download_job_running = FALSE,
      latest_fixed_utc = NULL,
      satellite_locked = FALSE,
      products_locked = FALSE,
      time_locked = FALSE,
      local_online_locked = FALSE,
      satellite_config = NULL,
      products_config = NULL,
      time_config = NULL,
      input_config = NULL,
      catalog_raw_manifest = NULL,
      catalog_updated_after_download = FALSE,
      plan_download_dir = NULL,
      plan_download_manifest = NULL
    )

    add_log <- function(...) {
      txt <- paste0(...)
      line <- sprintf(
        "[%s] %s",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
        txt
      )
      rv$download_log <- c(rv$download_log, line)
    }


    make_download_job_dir <- function() {
      file.path(
        tempdir(),
        paste0(
          "legiongoes_download_job_",
          format(Sys.time(), "%Y%m%d_%H%M%S", tz = "UTC"),
          "_",
          sample.int(999999, 1)
        )
      )
    }

    get_rscript_path <- function() {
      exe <- if (.Platform$OS.type == "windows") {
        file.path(R.home("bin"), "Rscript.exe")
      } else {
        file.path(R.home("bin"), "Rscript")
      }

      if (!file.exists(exe)) {
        stop("Rscript was not found at: ", exe)
      }

      normalizePath(exe, winslash = "/", mustWork = TRUE)
    }

    write_download_worker_script <- function(worker_file) {
      worker_code <- c(
        "args <- commandArgs(trailingOnly = TRUE)",
        "plan_file <- args[[1]]",
        "status_file <- args[[2]]",
        "log_file <- args[[3]]",
        "status_json_file <- args[[4]]",
        "write_status_json <- function(meta) {",
        "  if (!requireNamespace('jsonlite', quietly = TRUE)) return(invisible(FALSE))",
        "  payload <- list(",
        "    metadata = list(",
        "      type = 'download_status',",
        "      updated_at_utc = format(Sys.time(), '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC'),",
        "      running = meta$running, done = meta$done, current = meta$current, total = meta$total,",
        "      n_ok = meta$n_ok, n_error = meta$n_error, started_at = meta$started_at, finished_at = meta$finished_at",
        "    ),",
        "    files = meta$status",
        "  )",
        "  tmp <- paste0(status_json_file, '.tmp')",
        "  jsonlite::write_json(payload, tmp, dataframe = 'rows', pretty = TRUE, auto_unbox = TRUE, na = 'null')",
        "  if (file.exists(status_json_file)) unlink(status_json_file)",
        "  file.rename(tmp, status_json_file)",
        "  invisible(TRUE)",
        "}",
        "format_bytes <- function(x) {",
        "  if (is.null(x) || length(x) == 0 || is.na(x)) return(NA_character_)",
        "  x <- as.numeric(x)",
        "  if (x < 1024) return(paste0(x, ' B'))",
        "  if (x < 1024^2) return(sprintf('%.1f KB', x / 1024))",
        "  if (x < 1024^3) return(sprintf('%.1f MB', x / 1024^2))",
        "  sprintf('%.2f GB', x / 1024^3)",
        "}",
        "append_log <- function(...) {",
        "  txt <- paste0(...)",
        "  line <- sprintf('[%s] %s', format(Sys.time(), '%Y-%m-%d %H:%M:%S UTC', tz = 'UTC'), txt)",
        "  cat(line, '\\n', file = log_file, append = TRUE)",
        "}",
        "save_status <- function(meta) {",
        "  tmp <- paste0(status_file, '.tmp')",
        "  saveRDS(meta, tmp)",
        "  if (file.exists(status_file)) unlink(status_file)",
        "  file.rename(tmp, status_file)",
        "  try(write_status_json(meta), silent = TRUE)",
        "}",
        "safe_download <- function(url, destfile, overwrite = TRUE) {",
        "  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)",
        "  tmp <- paste0(destfile, '.partial')",
        "  if (file.exists(tmp)) unlink(tmp)",
        "  ok <- tryCatch({",
        "    utils::download.file(url = url, destfile = tmp, mode = 'wb', quiet = TRUE)",
        "    TRUE",
        "  }, error = function(e) {",
        "    append_log('ERROR download.file: ', conditionMessage(e))",
        "    FALSE",
        "  })",
        "  if (!ok || !file.exists(tmp)) {",
        "    if (file.exists(tmp)) unlink(tmp)",
        "    return(FALSE)",
        "  }",
        "  if (file.exists(destfile)) {",
        "    if (isTRUE(overwrite)) unlink(destfile) else { unlink(tmp); return(TRUE) }",
        "  }",
        "  file.rename(tmp, destfile) && file.exists(destfile)",
        "}",
        "plan <- readRDS(plan_file)",
        "status <- data.frame(",
        "  product = plan$product,",
        "  file = plan$online_file,",
        "  destination = plan$destination,",
        "  status = rep('pending', nrow(plan)),",
        "  detail = rep('', nrow(plan)),",
        "  stringsAsFactors = FALSE",
        ")",
        "meta <- list(",
        "  running = TRUE, done = FALSE, started_at = format(Sys.time(), '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC'),",
        "  finished_at = NA_character_, current = 0L, total = nrow(plan),",
        "  n_ok = 0L, n_error = 0L, status = status",
        ")",
        "save_status(meta)",
        "append_log('Download started using the frozen plan.')",
        "append_log('No new S3 query will be made during download.')",
        "if (nrow(plan) == 0) {",
        "  meta$running <- FALSE; meta$done <- TRUE; meta$finished_at <- format(Sys.time(), '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC')",
        "  save_status(meta); append_log('There are no files to download.'); quit(save = 'no')",
        "}",
        "for (i in seq_len(nrow(plan))) {",
        "  file_name <- plan$online_file[i]",
        "  destfile <- plan$destination[i]",
        "  url <- plan$online_url[i]",
        "  action <- plan$action[i]",
        "  meta$current <- i",
        "  meta$status$status[i] <- 'processing'",
        "  meta$status$detail[i] <- 'Downloading...'",
        "  save_status(meta)",
        "  append_log('--------------------------------------------------')",
        "  append_log('File [', i, '/', nrow(plan), ']: ', file_name)",
        "  append_log('Action: ', action)",
        "  append_log('Destination: ', destfile)",
        "  if (identical(action, 'Delete and Download') && file.exists(destfile)) {",
        "    append_log('Deleting previous local file because size differs...')",
        "    unlink(destfile)",
        "  }",
        "  ok <- safe_download(url = url, destfile = destfile, overwrite = TRUE)",
        "  if (ok) {",
        "    local_size <- file.info(destfile)$size",
        "    expected_size <- plan$online_size[i]",
        "    if (!is.na(expected_size) && !is.na(local_size) && local_size != expected_size) {",
        "      meta$status$status[i] <- 'error'",
        "      meta$status$detail[i] <- paste0('Downloaded, but size differs. Local=', local_size, ' Online=', expected_size)",
        "      append_log('ERROR: local size differs from expected online size.')",
        "    } else {",
        "      meta$status$status[i] <- 'ok'",
        "      meta$status$detail[i] <- paste0('Downloaded. Size: ', format_bytes(local_size))",
        "      append_log('OK. Downloaded size: ', format_bytes(local_size))",
        "    }",
        "  } else {",
        "    meta$status$status[i] <- 'error'",
        "    meta$status$detail[i] <- 'Could not download from URL.'",
        "    append_log('ERROR: could not download from URL.')",
        "  }",
        "  meta$n_ok <- sum(meta$status$status == 'ok', na.rm = TRUE)",
        "  meta$n_error <- sum(meta$status$status == 'error', na.rm = TRUE)",
        "  save_status(meta)",
        "}",
        "meta$running <- FALSE",
        "meta$done <- TRUE",
        "meta$finished_at <- format(Sys.time(), '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC')",
        "meta$n_ok <- sum(meta$status$status == 'ok', na.rm = TRUE)",
        "meta$n_error <- sum(meta$status$status == 'error', na.rm = TRUE)",
        "save_status(meta)",
        "append_log('--------------------------------------------------')",
        "append_log('Download finished.')",
        "append_log('OK: ', meta$n_ok)",
        "append_log('Errors: ', meta$n_error)"
      )

      writeLines(worker_code, worker_file, useBytes = TRUE)
      invisible(worker_file)
    }

    start_download_worker <- function(plan, job_dir = NULL) {
      if (is.null(job_dir) || !nzchar(job_dir)) {
        job_dir <- make_download_job_dir()
      }

      dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)

      plan_file <- file.path(job_dir, "plan.rds")
      status_file <- file.path(job_dir, "status.rds")
      status_json_file <- file.path(job_dir, "status.json")
      log_file <- file.path(job_dir, "download.log")
      worker_file <- file.path(job_dir, "worker.R")

      saveRDS(plan, plan_file)
      writeLines(character(), log_file, useBytes = TRUE)
      write_download_worker_script(worker_file)

      system2(
        command = get_rscript_path(),
        args = c(
          shQuote(worker_file),
          shQuote(plan_file),
          shQuote(status_file),
          shQuote(log_file),
          shQuote(status_json_file)
        ),
        wait = FALSE,
        stdout = FALSE,
        stderr = FALSE
      )

      list(
        job_dir = job_dir,
        plan_file = plan_file,
        status_file = status_file,
        status_json_file = status_json_file,
        log_file = log_file,
        worker_file = worker_file,
        started_at = Sys.time()
      )
    }

    read_download_job_status <- function(job) {
      if (is.null(job) || is.null(job$status_file) || !file.exists(job$status_file)) {
        return(NULL)
      }

      tryCatch(
        readRDS(job$status_file),
        error = function(e) NULL
      )
    }

    read_download_job_log <- function(job) {
      if (is.null(job) || is.null(job$log_file) || !file.exists(job$log_file)) {
        return(character())
      }

      tryCatch(
        readLines(job$log_file, warn = FALSE, encoding = "UTF-8"),
        error = function(e) character()
      )
    }

    selected_products <- shiny::reactive({
      product_choices[
        vapply(
          product_choices,
          function(p) {
            isTRUE(input[[paste0("use_", p)]])
          },
          logical(1)
        )
      ]
    })


    satellite_input_ids <- c("position")
    product_input_ids <- paste0("use_", product_choices)
    time_input_ids_to_lock <- c(
      "search_mode",
      "date_mode",
      "single_date",
      "date_range",
      "time_mode",
      "target_hour",
      "target_minute",
      "target_second",
      "point_rule",
      "from_hour",
      "from_minute",
      "from_second",
      "to_hour",
      "to_minute",
      "to_second",
      "range_rule"
    )

    set_ids_enabled <- function(ids, enabled = TRUE) {
      for (id_input in ids) {
        if (isTRUE(enabled)) {
          shinyjs::enable(id_input)
        } else {
          shinyjs::disable(id_input)
        }
      }
    }

    set_local_online_enabled <- function(enabled = TRUE) {
      if (isTRUE(enabled)) {
        shinyjs::enable("generate_catalog")
        shinyjs::enable("generate_plan")
        shinyjs::enable("clear_plan")
      } else {
        shinyjs::disable("generate_catalog")
        shinyjs::disable("generate_plan")
        shinyjs::disable("clear_plan")
      }
    }

    clear_generated_plan <- function(reset_local_online_lock = TRUE) {
      rv$selected_online <- NULL
      rv$verification <- NULL
      rv$plan <- NULL
      rv$download_status <- NULL
      rv$download_log <- character()
      rv$download_job <- NULL
      rv$download_job_running <- FALSE
      rv$plan_download_dir <- NULL
      rv$plan_download_manifest <- NULL
      rv$catalog_raw_manifest <- NULL
      rv$input_config <- NULL
      rv$catalog_updated_after_download <- FALSE

      if (isTRUE(reset_local_online_lock)) {
        rv$local_online_locked <- FALSE
        set_local_online_enabled(TRUE)
      }

      invisible(TRUE)
    }

    clear_downstream_from_satellite <- function() {
      rv$products_locked <- FALSE
      rv$time_locked <- FALSE
      rv$local_online_locked <- FALSE
      rv$products_config <- NULL
      rv$time_config <- NULL
      clear_generated_plan()
      set_ids_enabled(product_input_ids, TRUE)
      set_ids_enabled(time_input_ids_to_lock, TRUE)
      set_local_online_enabled(TRUE)
    }

    clear_downstream_from_products <- function() {
      rv$time_locked <- FALSE
      rv$local_online_locked <- FALSE
      rv$time_config <- NULL
      clear_generated_plan()
      set_ids_enabled(time_input_ids_to_lock, TRUE)
      set_local_online_enabled(TRUE)
    }

    clear_downstream_from_time <- function() {
      rv$local_online_locked <- FALSE
      clear_generated_plan()
      set_local_online_enabled(TRUE)
    }

    collect_satellite_config <- function() {
      list(
        position = input$position,
        created_at = Sys.time()
      )
    }

    collect_products_config <- function() {
      products <- selected_products()

      if (length(products) == 0) {
        stop("Select at least one product before locking products.")
      }

      list(
        products = products,
        created_at = Sys.time()
      )
    }

    collect_time_config <- function() {
      if (identical(input$search_mode, "latest") &&
          (is.null(rv$latest_fixed_utc) || is.na(rv$latest_fixed_utc))) {
        rv$latest_fixed_utc <- goesdl_v04_now_utc()
      }

      list(
        search_mode = input$search_mode,
        date_mode = input$date_mode,
        single_date = input$single_date,
        date_range = input$date_range,
        time_mode = input$time_mode,
        target_hour = input$target_hour,
        target_minute = input$target_minute,
        target_second = input$target_second,
        point_rule = input$point_rule,
        from_hour = input$from_hour,
        from_minute = input$from_minute,
        from_second = input$from_second,
        to_hour = input$to_hour,
        to_minute = input$to_minute,
        to_second = input$to_second,
        range_rule = input$range_rule,
        latest_fixed_utc = rv$latest_fixed_utc,
        created_at = Sys.time()
      )
    }

    collect_combined_config <- function() {
      if (!isTRUE(rv$satellite_locked) || is.null(rv$satellite_config)) {
        stop("First close the lock in the Satellite tab.")
      }

      if (!isTRUE(rv$products_locked) || is.null(rv$products_config)) {
        stop("First close the lock in the Products tab.")
      }

      if (!isTRUE(rv$time_locked) || is.null(rv$time_config)) {
        stop("First close the lock in the Time tab.")
      }

      list(
        position = rv$satellite_config$position,
        products = rv$products_config$products,
        search_mode = rv$time_config$search_mode,
        date_mode = rv$time_config$date_mode,
        single_date = rv$time_config$single_date,
        date_range = rv$time_config$date_range,
        time_mode = rv$time_config$time_mode,
        target_hour = rv$time_config$target_hour,
        target_minute = rv$time_config$target_minute,
        target_second = rv$time_config$target_second,
        point_rule = rv$time_config$point_rule,
        from_hour = rv$time_config$from_hour,
        from_minute = rv$time_config$from_minute,
        from_second = rv$time_config$from_second,
        to_hour = rv$time_config$to_hour,
        to_minute = rv$time_config$to_minute,
        to_second = rv$time_config$to_second,
        range_rule = rv$time_config$range_rule,
        latest_fixed_utc = rv$time_config$latest_fixed_utc,
        created_at = rv$time_config$created_at
      )
    }

    format_input_config <- function(cfg) {
      if (is.null(cfg)) {
        return("Satellite, Products, and Time still need to be locked.")
      }

      mode_txt <- if (identical(cfg$search_mode, "latest")) {
        "Latest online file available"
      } else {
        "Define search time"
      }

      date_txt <- if (identical(cfg$search_mode, "latest")) {
        paste0(
          "Fixed UTC day:    ",
          format(cfg$latest_fixed_utc, "%Y-%m-%d", tz = "UTC"),
          "\nFixed UTC:       ",
          format(cfg$latest_fixed_utc, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
        )
      } else if (identical(cfg$date_mode, "single")) {
        paste0("UTC date:        ", as.character(as.Date(cfg$single_date)))
      } else {
        dr <- as.Date(cfg$date_range)
        paste0("UTC date range:  ", as.character(dr[[1]]), " to ", as.character(dr[[2]]))
      }

      time_txt <- if (identical(cfg$search_mode, "latest")) {
        "Time:             Latest available inside the fixed UTC day"
      } else if (identical(cfg$time_mode, "single")) {
        paste0(
          "Single time UTC:   ", cfg$target_hour, ":", cfg$target_minute, ":", cfg$target_second,
          "\nRule:            ", cfg$point_rule
        )
      } else {
        paste0(
          "UTC range:        ", cfg$from_hour, ":", cfg$from_minute, ":", cfg$from_second,
          " to ", cfg$to_hour, ":", cfg$to_minute, ":", cfg$to_second,
          "\nRange rule:      ", cfg$range_rule
        )
      }

      paste(
        paste0("Position:         ", cfg$position),
        paste0("Products:        ", paste(cfg$products, collapse = ", ")),
        paste0("Mode:             ", mode_txt),
        date_txt,
        time_txt,
        paste0("Locked at:        ", format(cfg$created_at, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")),
        sep = "\n"
      )
    }

    shiny::observeEvent(input$search_mode, {
      if (identical(input$search_mode, "latest")) {
        rv$latest_fixed_utc <- goesdl_v04_now_utc()
      }
    }, ignoreInit = FALSE)

    output$satellite_lock_button <- shiny::renderUI({
      if (isTRUE(rv$satellite_locked)) {
        shiny::actionButton(
          inputId = session$ns("toggle_satellite_lock"),
          label = "🔒 Satellite locked",
          class = "btn btn-success"
        )
      } else {
        shiny::actionButton(
          inputId = session$ns("toggle_satellite_lock"),
          label = "🔓 Lock satellite",
          class = "btn btn-primary"
        )
      }
    })

    output$products_lock_button <- shiny::renderUI({
      if (isTRUE(rv$products_locked)) {
        shiny::actionButton(
          inputId = session$ns("toggle_products_lock"),
          label = "🔒 Products locked",
          class = "btn btn-success"
        )
      } else {
        shiny::actionButton(
          inputId = session$ns("toggle_products_lock"),
          label = "🔓 Lock products",
          class = "btn btn-primary"
        )
      }
    })

    output$time_lock_button <- shiny::renderUI({
      if (isTRUE(rv$time_locked)) {
        shiny::actionButton(
          inputId = session$ns("toggle_time_lock"),
          label = "🔒 Time locked",
          class = "btn btn-success"
        )
      } else {
        shiny::actionButton(
          inputId = session$ns("toggle_time_lock"),
          label = "🔓 Lock time",
          class = "btn btn-primary"
        )
      }
    })

    output$local_online_lock_button <- shiny::renderUI({
      if (isTRUE(rv$local_online_locked)) {
        shiny::actionButton(
          inputId = session$ns("toggle_local_online_lock"),
          label = "🔒 Download plan locked",
          class = "btn btn-success"
        )
      } else {
        shiny::actionButton(
          inputId = session$ns("toggle_local_online_lock"),
          label = "🔓 Lock download plan",
          class = "btn btn-primary"
        )
      }
    })

    shiny::observeEvent(input$toggle_satellite_lock, {
      if (isTRUE(rv$satellite_locked)) {
        rv$satellite_locked <- FALSE
        rv$satellite_config <- NULL
        clear_downstream_from_satellite()
        set_ids_enabled(satellite_input_ids, TRUE)
        shiny::showNotification("Satellite unlocked. Downstream products/time/plan were cleared.", type = "warning")
        return()
      }

      rv$satellite_config <- collect_satellite_config()
      rv$satellite_locked <- TRUE
      clear_downstream_from_satellite()
      set_ids_enabled(satellite_input_ids, FALSE)
      shiny::showNotification("Satellite locked. Continue with Products.", type = "message")
    })

    shiny::observeEvent(input$toggle_products_lock, {
      if (!isTRUE(rv$satellite_locked)) {
        shiny::showNotification("First close the Satellite lock.", type = "warning")
        return()
      }

      if (isTRUE(rv$products_locked)) {
        rv$products_locked <- FALSE
        rv$products_config <- NULL
        clear_downstream_from_products()
        set_ids_enabled(product_input_ids, TRUE)
        shiny::showNotification("Products unlocked. Downstream time/plan was cleared.", type = "warning")
        return()
      }

      cfg <- tryCatch(
        collect_products_config(),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error", duration = 8)
          NULL
        }
      )

      if (is.null(cfg)) {
        return()
      }

      rv$products_config <- cfg
      rv$products_locked <- TRUE
      clear_downstream_from_products()
      set_ids_enabled(product_input_ids, FALSE)
      shiny::showNotification("Products locked. Continue with Time.", type = "message")
    })

    shiny::observeEvent(input$toggle_time_lock, {
      if (!isTRUE(rv$satellite_locked)) {
        shiny::showNotification("First close the Satellite lock.", type = "warning")
        return()
      }

      if (!isTRUE(rv$products_locked)) {
        shiny::showNotification("First close the Products lock.", type = "warning")
        return()
      }

      if (isTRUE(rv$time_locked)) {
        rv$time_locked <- FALSE
        rv$time_config <- NULL
        clear_downstream_from_time()
        set_ids_enabled(time_input_ids_to_lock, TRUE)
        shiny::showNotification("Time unlocked. Downstream plan was cleared.", type = "warning")
        return()
      }

      cfg <- collect_time_config()
      rv$time_config <- cfg
      rv$time_locked <- TRUE
      clear_downstream_from_time()
      set_ids_enabled(time_input_ids_to_lock, FALSE)
      shiny::showNotification("Time locked. You can now generate the plan.", type = "message")
    })

    shiny::observeEvent(input$toggle_local_online_lock, {
      if (isTRUE(rv$local_online_locked)) {
        rv$local_online_locked <- FALSE
        set_local_online_enabled(TRUE)
        shiny::showNotification("Download plan unlocked. You can create or clear it again.", type = "warning")
        return()
      }

      if (is.null(rv$verification) || nrow(rv$verification) == 0) {
        shiny::showNotification("Create a download plan first.", type = "warning")
        return()
      }

      rv$local_online_locked <- TRUE
      set_local_online_enabled(FALSE)
      shiny::showNotification("Download plan locked. You can now go to Download.", type = "message")
    })

    output$satellite_summary <- shiny::renderText({
      if (isTRUE(rv$satellite_locked) && !is.null(rv$satellite_config)) {
        return(paste(
          paste0("Position: ", rv$satellite_config$position),
          paste0("Locked at: ", format(rv$satellite_config$created_at, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")),
          sep = "\n"
        ))
      }

      paste0("Satellite unlocked. Current position: ", input$position)
    })

    output$products_summary <- shiny::renderText({
      if (isTRUE(rv$products_locked) && !is.null(rv$products_config)) {
        return(paste(
          paste0("Products: ", paste(rv$products_config$products, collapse = ", ")),
          paste0("Count:     ", length(rv$products_config$products)),
          paste0("Locked at:  ", format(rv$products_config$created_at, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")),
          sep = "\n"
        ))
      }

      products <- selected_products()
      if (length(products) == 0) {
        return("Products unlocked. No products selected.")
      }

      paste0("Products unlocked. Current selection: ", paste(products, collapse = ", "))
    })

    output$time_summary <- shiny::renderText({
      if (isTRUE(rv$time_locked) && !is.null(rv$time_config)) {
        cfg <- c(
          list(position = if (!is.null(rv$satellite_config)) rv$satellite_config$position else input$position),
          list(products = if (!is.null(rv$products_config)) rv$products_config$products else selected_products()),
          rv$time_config
        )
        return(format_input_config(cfg))
      }

      cfg <- tryCatch(
        c(
          list(position = if (!is.null(rv$satellite_config)) rv$satellite_config$position else input$position),
          list(products = if (!is.null(rv$products_config)) rv$products_config$products else selected_products()),
          collect_time_config()
        ),
        error = function(e) NULL
      )

      if (is.null(cfg)) {
        return("Time unlocked. Configure the time criteria and close the lock.")
      }

      paste("Current time preview:\n", format_input_config(cfg), sep = "")
    })

    output$locked_inputs_summary <- shiny::renderText({
      cfg <- tryCatch(collect_combined_config(), error = function(e) NULL)

      if (is.null(cfg)) {
        return("Lock tabs 1, 2, and 3 before generating the plan.")
      }

      format_input_config(cfg)
    })

    output$local_online_lock_summary <- shiny::renderText({
      if (isTRUE(rv$local_online_locked)) {
        return(paste(
          "Download plan locked.",
          paste0("Selected files: ", goesdl_v04_nrow_or_zero(rv$verification)),
          paste0("Pending downloads: ", goesdl_v04_nrow_or_zero(rv$plan)),
          sep = "\n"
        ))
      }

      if (is.null(rv$verification)) {
        return("Download plan unlocked. Build/update the catalog first, then create the download plan.")
      }

      paste(
        "Download plan unlocked.",
        paste0("Selected files: ", goesdl_v04_nrow_or_zero(rv$verification)),
        paste0("Pending downloads: ", goesdl_v04_nrow_or_zero(rv$plan)),
        sep = "\n"
      )
    })

    output$latest_fixed_utc_box <- shiny::renderUI({
      fixed_time <- rv$latest_fixed_utc

      if (is.null(fixed_time) || is.na(fixed_time)) {
        fixed_time <- goesdl_v04_now_utc()
      }

      fixed_date <- as.Date(fixed_time, tz = "UTC")
      fixed_hour <- format(fixed_time, "%H", tz = "UTC")
      fixed_minute <- format(fixed_time, "%M", tz = "UTC")
      fixed_second <- format(fixed_time, "%S", tz = "UTC")
      fixed_txt <- format(fixed_time, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")

      shiny::tagList(
        shiny::div(
          class = "goes-frozen-time-note",
          paste0("Fixed UTC date/time for the search:\n", fixed_txt)
        ),
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shinyjs::disabled(
              shiny::dateInput(
                inputId = session$ns("latest_fixed_date_display"),
                label = "Date UTC",
                value = fixed_date
              )
            )
          ),
          shiny::column(
            width = 3,
            shinyjs::disabled(
              shiny::selectInput(
                inputId = session$ns("latest_fixed_hour_display"),
                label = "UTC hour",
                choices = goesdl_v04_fmt_hh(0:23),
                selected = fixed_hour
              )
            )
          ),
          shiny::column(
            width = 3,
            shinyjs::disabled(
              shiny::selectInput(
                inputId = session$ns("latest_fixed_minute_display"),
                label = "UTC minute",
                choices = goesdl_v04_fmt_mm(0:59),
                selected = fixed_minute
              )
            )
          ),
          shiny::column(
            width = 3,
            shinyjs::disabled(
              shiny::selectInput(
                inputId = session$ns("latest_fixed_second_display"),
                label = "UTC second",
                choices = goesdl_v04_fmt_ss(0:59),
                selected = fixed_second
              )
            )
          )
        )
      )
    })

    output$download_dir_preview <- shiny::renderText({
      paste0(
        "data_raw:\n",
        normalizePath(default_download_dir, winslash = "/", mustWork = FALSE),
        "\n\ncatalog_raw:\n",
        normalizePath(catalog_raw_base_dir, winslash = "/", mustWork = FALSE),
        "\n\nplan_download:\n",
        normalizePath(plan_download_base_dir, winslash = "/", mustWork = FALSE)
      )
    })


    output$download_dir_preview_catalog <- shiny::renderText({
      catalog_txt <- if (is.null(rv$catalog_raw_manifest) || nrow(rv$catalog_raw_manifest) == 0) {
        "catalog_raw has not been updated yet."
      } else {
        paste(rv$catalog_raw_manifest$catalog_dir, collapse = "\n")
      }

      paste0(
        "data_raw:\n",
        normalizePath(default_download_dir, winslash = "/", mustWork = FALSE),
        "\n\ncatalog_raw base:\n",
        normalizePath(catalog_raw_base_dir, winslash = "/", mustWork = FALSE),
        "\n\nupdated catalog_raw folders:\n",
        catalog_txt
      )
    })

    output$download_dir_preview_plan <- shiny::renderText({
      plan_dir_txt <- if (is.null(rv$plan_download_dir)) {
        "No plan_download has been created yet."
      } else {
        rv$plan_download_dir
      }

      catalog_txt <- if (is.null(rv$catalog_raw_manifest) || nrow(rv$catalog_raw_manifest) == 0) {
        "catalog_raw has not been updated yet."
      } else {
        paste(rv$catalog_raw_manifest$catalog_dir, collapse = "\n")
      }

      paste0(
        "data_raw:\n",
        normalizePath(default_download_dir, winslash = "/", mustWork = FALSE),
        "\n\nupdated catalog_raw:\n",
        catalog_txt,
        "\n\ncurrent plan_download:\n",
        plan_dir_txt
      )
    })

    output$catalog_summary <- shiny::renderText({
      manifest <- rv$catalog_raw_manifest
      selected <- rv$selected_online

      if (is.null(manifest) || !is.data.frame(manifest) || nrow(manifest) == 0) {
        return("No catalog has been built yet. Lock Satellite, Products and Time, then click Build / update catalog.")
      }

      paste(
        paste0("catalog_raw folders: ", nrow(manifest)),
        paste0("catalog rows:        ", sum(manifest$n_files, na.rm = TRUE)),
        paste0("selected files:      ", goesdl_v04_nrow_or_zero(selected)),
        paste0("catalog_raw base:    ", normalizePath(catalog_raw_base_dir, winslash = "/", mustWork = FALSE)),
        sep = "\n"
      )
    })

    output$catalog_manifest_table <- shiny::renderTable({
      manifest <- rv$catalog_raw_manifest

      if (is.null(manifest) || !is.data.frame(manifest) || nrow(manifest) == 0) {
        return(data.frame(
          Message = "No catalog has been built yet.",
          stringsAsFactors = FALSE
        ))
      }

      manifest[, c("bucket", "product_code", "year", "julian_day", "n_files", "catalog_dir"), drop = FALSE]
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    output$catalog_completeness_table <- shiny::renderTable({
      manifest <- rv$catalog_raw_manifest

      if (is.null(manifest) || !is.data.frame(manifest) || nrow(manifest) == 0) {
        return(data.frame(
          Product = "No catalog",
          Year = "",
          Julian_day = "",
          Expected = 0L,
          Online = 0L,
          Local = 0L,
          Missing_online = 0L,
          Future_time = 0L,
          Completeness_pct = 0,
          stringsAsFactors = FALSE
        ))
      }

      rows <- lapply(seq_len(nrow(manifest)), function(i) {
        csv_file <- manifest$catalog_csv[[i]]

        if (!file.exists(csv_file)) {
          return(data.frame(
            Product = manifest$product_code[[i]],
            Year = manifest$year[[i]],
            Julian_day = manifest$julian_day[[i]],
            Expected = manifest$n_files[[i]],
            Online = NA_integer_,
            Local = NA_integer_,
            Missing_online = NA_integer_,
            Future_time = NA_integer_,
            Completeness_pct = NA_real_,
            stringsAsFactors = FALSE
          ))
        }

        x <- utils::read.csv(csv_file, stringsAsFactors = FALSE)
        expected <- nrow(x)
        online <- sum(tolower(as.character(x$online_found)) %in% c("true", "1"), na.rm = TRUE)
        local <- sum(tolower(as.character(x$local_found)) %in% c("true", "1"), na.rm = TRUE)
        future <- if ("catalog_status" %in% names(x)) sum(x$catalog_status == "expected_future_time", na.rm = TRUE) else 0L
        missing <- expected - online - future

        data.frame(
          Product = manifest$product_code[[i]],
          Year = manifest$year[[i]],
          Julian_day = manifest$julian_day[[i]],
          Expected = expected,
          Online = online,
          Local = local,
          Missing_online = max(missing, 0L),
          Future_time = future,
          Completeness_pct = if (expected > 0) round(100 * online / expected, 2) else NA_real_,
          stringsAsFactors = FALSE
        )
      })

      dplyr::bind_rows(rows)
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    shiny::observeEvent(input$clear_plan, {
      clear_generated_plan()
    })

    shiny::observeEvent(input$clear_download, {
      if (isTRUE(rv$download_job_running)) {
        shiny::showNotification(
          "A download is running. The log will not be cleared until it finishes.",
          type = "warning",
          duration = 6
        )
        return()
      }

      rv$download_status <- NULL
      rv$download_log <- character()
      rv$download_job <- NULL
    })

    shiny::observeEvent(input$generate_catalog, {
      if (isTRUE(rv$local_online_locked)) {
        shiny::showNotification(
          "The download plan is locked. Open the Download Plan lock before rebuilding the catalog.",
          type = "warning",
          duration = 8
        )
        return()
      }

      cfg <- tryCatch(
        collect_combined_config(),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "warning", duration = 8)
          NULL
        }
      )

      if (is.null(cfg)) {
        return()
      }

      rv$input_config <- cfg
      products <- cfg$products
      download_dir <- default_download_dir
      dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

      selected <- NULL

      shiny::withProgress(message = "Building catalog", value = 0, {
        status_fun <- function(txt) {
          shiny::incProgress(0.02, detail = txt)
          message(txt)
        }

        selected <- tryCatch(
          goesdl_v04_build_selection(
            products = products,
            position = cfg$position,
            search_mode = cfg$search_mode,
            date_mode = cfg$date_mode,
            single_date = cfg$single_date,
            date_range = cfg$date_range,
            time_mode = cfg$time_mode,
            target_hour = cfg$target_hour,
            target_minute = cfg$target_minute,
            target_second = cfg$target_second,
            point_rule = cfg$point_rule,
            from_hour = cfg$from_hour,
            from_minute = cfg$from_minute,
            from_second = cfg$from_second,
            to_hour = cfg$to_hour,
            to_minute = cfg$to_minute,
            to_second = cfg$to_second,
            range_rule = cfg$range_rule,
            max_pages = fixed_max_pages,
            status_fun = status_fun,
            latest_reference_time_utc = cfg$latest_fixed_utc
          ),
          error = function(e) {
            shiny::showNotification(conditionMessage(e), type = "error", duration = 10)
            NULL
          }
        )

        shiny::incProgress(0.90, detail = "Writing catalog_raw.")
      })

      if (is.null(selected)) {
        return()
      }

      online_all <- attr(selected, "online_all", exact = TRUE)

      rv$catalog_raw_manifest <- tryCatch(
        goesdl_v04_write_catalog_raw(
          online_all = online_all,
          download_dir = download_dir,
          catalog_base_dir = catalog_raw_base_dir,
          input_config = cfg,
          event = "catalog_updated"
        ),
        error = function(e) {
          shiny::showNotification(
            paste("Could not update catalog_raw:", conditionMessage(e)),
            type = "warning",
            duration = 10
          )
          data.frame()
        }
      )

      rv$selected_online <- selected
      rv$verification <- NULL
      rv$plan <- NULL
      rv$plan_download_dir <- NULL
      rv$plan_download_manifest <- NULL
      rv$download_status <- NULL
      rv$download_log <- character()
      rv$catalog_updated_after_download <- FALSE
      rv$local_online_locked <- FALSE
      set_local_online_enabled(TRUE)

      if (is.null(rv$catalog_raw_manifest) || nrow(rv$catalog_raw_manifest) == 0) {
        shiny::showNotification(
          "No catalog rows were written. Check the locked Satellite, Products, and Time configuration.",
          type = "warning",
          duration = 8
        )
      } else {
        shiny::showNotification(
          "catalog_raw was updated. Continue with Download Plan.",
          type = "message"
        )
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$generate_plan, {
      if (isTRUE(rv$local_online_locked)) {
        shiny::showNotification(
          "The download plan is locked. Open the Download Plan lock to create it again.",
          type = "warning",
          duration = 8
        )
        return()
      }

      cfg <- rv$input_config
      selected <- rv$selected_online

      if (is.null(cfg) || is.null(selected)) {
        shiny::showNotification(
          "Build / update the catalog before creating a download plan.",
          type = "warning",
          duration = 8
        )
        return()
      }

      if (is.null(rv$catalog_raw_manifest) || !is.data.frame(rv$catalog_raw_manifest) || nrow(rv$catalog_raw_manifest) == 0) {
        shiny::showNotification(
          "catalog_raw is empty. Build / update the catalog before creating a download plan.",
          type = "warning",
          duration = 8
        )
        return()
      }

      download_dir <- default_download_dir
      dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

      if (nrow(selected) == 0) {
        rv$verification <- data.frame()
        rv$plan <- data.frame()
        rv$plan_download_dir <- NULL
        rv$plan_download_manifest <- NULL
        shiny::showNotification(
          "The catalog was updated, but no online files matched the locked time selection.",
          type = "warning",
          duration = 8
        )
        return()
      }

      tables <- goesdl_v04_make_download_tables(
        selected = selected,
        download_dir = download_dir
      )

      plan_manifest <- tryCatch(
        goesdl_v04_write_plan_download(
          verification = tables$verification,
          plan = tables$plan,
          input_config = cfg,
          data_raw_dir = download_dir,
          catalog_manifest = rv$catalog_raw_manifest,
          plan_base_dir = plan_download_base_dir
        ),
        error = function(e) {
          shiny::showNotification(
            paste("Could not create plan_download:", conditionMessage(e)),
            type = "error",
            duration = 10
          )
          NULL
        }
      )

      if (is.null(plan_manifest)) {
        return()
      }

      rv$verification <- tables$verification
      rv$plan <- tables$plan
      rv$plan_download_manifest <- plan_manifest
      rv$plan_download_dir <- plan_manifest$plan_dir
      rv$download_status <- NULL
      rv$download_log <- character()
      rv$catalog_updated_after_download <- FALSE
      rv$local_online_locked <- FALSE
      set_local_online_enabled(TRUE)

      shiny::showNotification(
        "Download plan created successfully.",
        type = "message"
      )
    }, ignoreInit = TRUE)

    output$selection_summary <- shiny::renderText({
      cfg <- rv$input_config
      if (is.null(cfg)) {
        cfg <- tryCatch(collect_combined_config(), error = function(e) NULL)
      }
      products <- if (is.null(cfg)) character() else cfg$products
      verification <- rv$verification
      plan <- rv$plan
      download_dir <- default_download_dir

      if (is.null(verification)) {
        return("No download plan has been created yet. Build/update the catalog first, then create the download plan.")
      }

      n_total <- nrow(verification)
      n_ok <- if (n_total > 0) {
        sum(verification$action == "OK", na.rm = TRUE)
      } else {
        0L
      }
      n_download <- if (n_total > 0) {
        sum(verification$action == "Download", na.rm = TRUE)
      } else {
        0L
      }
      n_delete_download <- if (n_total > 0) {
        sum(verification$action == "Delete and Download", na.rm = TRUE)
      } else {
        0L
      }
      size_total <- if (n_total > 0) {
        sum(verification$online_size, na.rm = TRUE)
      } else {
        0
      }

      paste(
        paste0("Products:           ", paste(products, collapse = ", ")),
        paste0("Main mode:      ", if (is.null(cfg)) "" else cfg$search_mode),
        if (!is.null(cfg) && identical(cfg$search_mode, "latest") && !is.null(cfg$latest_fixed_utc)) {
          paste0("Fixed UTC:         ", format(cfg$latest_fixed_utc, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"), " (same UTC day)")
        } else {
          paste0("Fixed UTC:         ", "Not applicable")
        },
        paste0("Selected files:     ", n_total),
        paste0("OK local:            ", n_ok),
        paste0("To download:        ", goesdl_v04_nrow_or_zero(plan)),
        paste0("Download:            ", n_download),
        paste0("Delete and Download: ", n_delete_download),
        paste0("Total online size: ", goes_format_bytes(size_total)),
        paste0("Destination folder: ", normalizePath(download_dir, winslash = "/", mustWork = FALSE)),
        sep = "\n"
      )
    })


    output$product_plan_summary_table <- shiny::renderTable({
      x <- rv$verification

      if (is.null(x) || nrow(x) == 0) {
        return(data.frame(
          Product = "No download plan",
          Online_selected = 0L,
          Local_found = 0L,
          OK_local = 0L,
          To_download = 0L,
          Redownload = 0L,
          MB_online = 0,
          stringsAsFactors = FALSE
        ))
      }

      products <- sort(unique(x$product))

      out <- lapply(products, function(product_now) {
        y <- x[x$product == product_now, , drop = FALSE]
        data.frame(
          Product = product_now,
          Online_selected = nrow(y),
          Local_found = sum(y$local_found, na.rm = TRUE),
          OK_local = sum(y$action == "OK", na.rm = TRUE),
          To_download = sum(y$action %in% c("Download", "Delete and Download"), na.rm = TRUE),
          Redownload = sum(y$action == "Delete and Download", na.rm = TRUE),
          MB_online = round(sum(y$online_size, na.rm = TRUE) / 1024^2, 3),
          stringsAsFactors = FALSE
        )
      })

      dplyr::bind_rows(out)
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    output$product_first_last_table <- shiny::renderTable({
      x <- rv$verification

      if (is.null(x) || nrow(x) == 0) {
        return(data.frame(
          Product = "No download plan",
          First_UTC_time = "",
          Last_UTC_time = "",
          First_nc_file = "",
          Last_nc_file = "",
          stringsAsFactors = FALSE
        ))
      }

      products <- sort(unique(x$product))

      out <- lapply(products, function(product_now) {
        y <- x[x$product == product_now, , drop = FALSE]

        time_utc <- as.POSIXct(
          y$start_time_utc,
          tz = "UTC",
          format = "%Y-%m-%d %H:%M:%S"
        )

        ord <- order(time_utc, y$online_file, na.last = TRUE)
        y <- y[ord, , drop = FALSE]
        time_utc <- time_utc[ord]

        first_idx <- 1L
        last_idx <- nrow(y)

        data.frame(
          Product = product_now,
          First_UTC_time = format(time_utc[first_idx], "%Y-%m-%d %H:%M:%S", tz = "UTC"),
          Last_UTC_time = format(time_utc[last_idx], "%Y-%m-%d %H:%M:%S", tz = "UTC"),
          First_nc_file = y$online_file[first_idx],
          Last_nc_file = y$online_file[last_idx],
          stringsAsFactors = FALSE
        )
      })

      dplyr::bind_rows(out)
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    output$verification_table <- DT::renderDT({
      x <- rv$verification

      if (is.null(x) || nrow(x) == 0) {
        return(DT::datatable(
          data.frame(Message = "There are no selected files yet."),
          rownames = FALSE,
          options = list(dom = "t")
        ))
      }

      tab <- x[, c(
        "product",
        "date",
        "start_time_utc",
        "selection_mode",
        "time_rule",
        "online_file",
        "online_size_mb",
        "local_found",
        "local_size_mb",
        "size_match",
        "action",
        "destination"
      ), drop = FALSE]

      DT::datatable(
        tab,
        rownames = FALSE,
        filter = "top",
        extensions = c("Buttons"),
        options = list(
          pageLength = 25,
          lengthMenu = c(10, 25, 50, 100, 500),
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel")
        )
      )
    })

    output$download_summary <- shiny::renderText({
      plan <- rv$plan
      download_dir <- default_download_dir

      if (is.null(plan) || nrow(plan) == 0) {
        return("There are no pending files to download. Generate a plan first or check whether everything is already OK locally.")
      }

      if (!isTRUE(rv$local_online_locked)) {
        return(paste(
          "There is a plan with pending files, but it is not locked yet.",
          "Close the Download Plan lock before downloading.",
          paste0("Pending files: ", nrow(plan)),
          sep = "\n"
        ))
      }

      size_total <- sum(plan$online_size, na.rm = TRUE)

      paste(
        paste0("Files to download: ", nrow(plan)),
        paste0("Total online size:  ", goes_format_bytes(size_total)),
        paste0("Destination folder:  ", normalizePath(download_dir, winslash = "/", mustWork = FALSE)),
        "",
        "The download uses only the URLs selected in the current plan.",
        sep = "\n"
      )
    })

    shiny::observeEvent(input$start_download, {
      plan <- rv$plan

      if (isTRUE(rv$download_job_running)) {
        shiny::showNotification(
          "A download is already running.",
          type = "warning",
          duration = 6
        )
        return()
      }

      if (!isTRUE(rv$local_online_locked)) {
        shiny::showNotification(
          "First close the Download Plan lock to freeze the plan.",
          type = "warning",
          duration = 8
        )
        return()
      }

      if (is.null(plan) || nrow(plan) == 0) {
        shiny::showNotification(
          "There are no pending files to download.",
          type = "warning"
        )
        return()
      }

      rv$download_log <- character()
      rv$download_status <- data.frame(
        product = plan$product,
        file = plan$online_file,
        destination = plan$destination,
        status = rep("pending", nrow(plan)),
        detail = rep("", nrow(plan)),
        stringsAsFactors = FALSE
      )
      rv$download_job <- NULL
      rv$download_job_running <- FALSE
      rv$catalog_updated_after_download <- FALSE

      add_log("Preparing download in external Rscript process...")

      job <- tryCatch(
        start_download_worker(plan, job_dir = rv$plan_download_dir),
        error = function(e) {
          add_log("ERROR starting download process: ", conditionMessage(e))
          shiny::showNotification(
            conditionMessage(e),
            type = "error",
            duration = 10
          )
          NULL
        }
      )

      if (is.null(job)) {
        return()
      }

      rv$download_job <- job
      rv$download_job_running <- TRUE
      add_log("External process started. The log will update automatically.")

      shiny::showNotification(
        "Download started in a background process.",
        type = "message",
        duration = 5
      )
    })

    download_poll <- shiny::reactiveTimer(1000, session = session)

    shiny::observe({
      download_poll()

      job <- rv$download_job

      if (is.null(job)) {
        return()
      }

      log_now <- read_download_job_log(job)

      if (length(log_now) > 0) {
        rv$download_log <- log_now
      }

      meta <- read_download_job_status(job)

      if (!is.null(meta) && !is.null(meta$status)) {
        rv$download_status <- meta$status

        if (!isTRUE(meta$running) && isTRUE(rv$download_job_running)) {
          rv$download_job_running <- FALSE

          if (!isTRUE(rv$catalog_updated_after_download)) {
            online_all <- attr(rv$selected_online, "online_all", exact = TRUE)
            rv$catalog_raw_manifest <- tryCatch(
              goesdl_v04_write_catalog_raw(
                online_all = online_all,
                download_dir = default_download_dir,
                catalog_base_dir = catalog_raw_base_dir,
                input_config = rv$input_config,
                event = "download_finished"
              ),
              error = function(e) {
                add_log("WARNING: could not refresh catalog_raw after download: ", conditionMessage(e))
                rv$catalog_raw_manifest
              }
            )
            rv$catalog_updated_after_download <- TRUE
          }

          if (isTRUE(meta$n_error > 0)) {
            shiny::showNotification(
              paste0("Download finished with errors: ", meta$n_error),
              type = "warning",
              duration = 8
            )
          } else {
            shiny::showNotification(
              "Download finished successfully.",
              type = "message",
              duration = 6
            )
          }
        }
      }
    })

    output$download_log <- shiny::renderText({
      if (length(rv$download_log) == 0) {
        return("No download has been started yet.")
      }

      paste(rv$download_log, collapse = "\n")
    })

    output$download_status_table <- DT::renderDT({
      x <- rv$download_status

      if (is.null(x) || nrow(x) == 0) {
        return(DT::datatable(
          data.frame(Message = "There is no download status yet."),
          rownames = FALSE,
          options = list(dom = "t")
        ))
      }

      DT::datatable(
        x,
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 25,
          scrollX = TRUE
        )
      )
    })
  })
}

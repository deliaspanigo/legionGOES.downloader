# ==============================================================================
# GOES DOWNLOADER CORE
# Funciones puras de R para buscar, verificar y descargar archivos GOES desde S3.
# No depende de Shiny.
#
# Paquetes requeridos instalados:
# httr, xml2, stringr, dplyr
#
# Uso:
# source("R/goes_downloader_core.R")
# ==============================================================================


# ------------------------------------------------------------------------------
# 1) Configuración de productos
# ------------------------------------------------------------------------------

goes_product_specs <- function() {
  data.frame(
    product = c("LSTF", "MCMIPF", "FDCF", "GLM"),
    product_code = c(
      "ABI-L2-LSTF",
      "ABI-L2-MCMIPF",
      "ABI-L2-FDCF",
      "GLM-L2-LCFA"
    ),
    frequency_label = c(
      "1 archivo por hora",
      "1 archivo cada 10 minutos",
      "1 archivo cada 10 minutos",
      "1 archivo cada 20 segundos"
    ),
    minute_mode = c(
      "locked_first",
      "by_10",
      "by_10",
      "by_1"
    ),
    second_mode = c(
      "locked_first",
      "locked_first",
      "locked_first",
      "by_20"
    ),
    stringsAsFactors = FALSE
  )
}


goes_product_spec <- function(product) {
  specs <- goes_product_specs()

  x <- specs[specs$product == product, , drop = FALSE]

  if (nrow(x) == 0) {
    stop("Producto no reconocido: ", product)
  }

  x
}


goes_product_code <- function(product) {
  goes_product_spec(product)$product_code[[1]]
}


# ------------------------------------------------------------------------------
# 2) Utilidades generales
# ------------------------------------------------------------------------------

goes_format_bytes <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(NA_character_)
  }

  x <- as.numeric(x)

  if (x < 1024) {
    return(paste0(x, " B"))
  }

  if (x < 1024^2) {
    return(sprintf("%.1f KB", x / 1024))
  }

  if (x < 1024^3) {
    return(sprintf("%.1f MB", x / 1024^2))
  }

  sprintf("%.2f GB", x / 1024^3)
}


goes_julian_to_gregorian <- function(year, julian_day) {
  as.Date(
    as.integer(julian_day) - 1,
    origin = sprintf("%04d-01-01", as.integer(year))
  )
}


goes_validate_date <- function(
    year,
    date_format = c("gregorian", "julian"),
    month = NULL,
    day = NULL,
    julian_day = NULL
) {
  date_format <- match.arg(date_format)

  year <- as.integer(year)

  if (is.na(year) || year < 2017 || year > 2100) {
    stop("Año inválido.")
  }

  if (identical(date_format, "gregorian")) {
    date_txt <- sprintf(
      "%04d-%02d-%02d",
      year,
      as.integer(month),
      as.integer(day)
    )

    date <- as.Date(date_txt)

    if (is.na(date)) {
      stop("Fecha gregoriana inválida.")
    }

    if (!identical(format(date, "%Y"), sprintf("%04d", year))) {
      stop("La fecha gregoriana no pertenece al año indicado.")
    }

    return(list(
      year = year,
      gregorian_date = date,
      julian_day = as.integer(format(date, "%j"))
    ))
  }

  jd <- as.integer(julian_day)

  if (is.na(jd) || jd < 1 || jd > 366) {
    stop("Día juliano inválido.")
  }

  date <- goes_julian_to_gregorian(year, jd)

  if (!identical(format(date, "%Y"), sprintf("%04d", year))) {
    stop("Día juliano inválido para ese año.")
  }

  list(
    year = year,
    gregorian_date = date,
    julian_day = jd
  )
}


# ------------------------------------------------------------------------------
# 3) Satélite y bucket
# ------------------------------------------------------------------------------

goes_resolve_satellite <- function(position, date) {
  date <- as.Date(date)

  if (identical(position, "EAST")) {
    if (date >= as.Date("2025-04-07")) {
      return(list(
        position = "EAST",
        satellite = "GOES-19",
        bucket = "noaa-goes19",
        rule = "GOES-19 como GOES-East desde 2025-04-07"
      ))
    }

    if (date >= as.Date("2017-12-18")) {
      return(list(
        position = "EAST",
        satellite = "GOES-16",
        bucket = "noaa-goes16",
        rule = "GOES-16 como GOES-East antes de GOES-19"
      ))
    }

    stop("No hay regla definida para GOES-East antes de 2017-12-18.")
  }

  if (identical(position, "WEST")) {
    if (date >= as.Date("2023-01-04")) {
      return(list(
        position = "WEST",
        satellite = "GOES-18",
        bucket = "noaa-goes18",
        rule = "GOES-18 como GOES-West desde 2023-01-04"
      ))
    }

    if (date >= as.Date("2019-02-12")) {
      return(list(
        position = "WEST",
        satellite = "GOES-17",
        bucket = "noaa-goes17",
        rule = "GOES-17 como GOES-West antes de GOES-18"
      ))
    }

    stop("No hay regla definida para GOES-West antes de 2019-02-12.")
  }

  stop("Posición no reconocida: ", position)
}


goes_satellite_code <- function(satellite) {
  paste0("G", stringr::str_extract(satellite, "[0-9]+"))
}


# ------------------------------------------------------------------------------
# 4) Normalización de hora/minuto/segundo por producto
# ------------------------------------------------------------------------------

goes_normalize_product_time <- function(product, hour, minute, second) {
  spec <- goes_product_spec(product)

  if (identical(spec$minute_mode[[1]], "locked_first")) {
    minute <- "FIRST"
  }

  if (identical(spec$second_mode[[1]], "locked_first")) {
    second <- "FIRST"
  }

  if (identical(hour, "FIRST")) {
    minute <- "FIRST"
    second <- "FIRST"
  }

  if (identical(hour, "ALL")) {
    minute <- if (identical(spec$minute_mode[[1]], "locked_first")) {
      "FIRST"
    } else {
      "ALL"
    }

    second <- if (identical(spec$second_mode[[1]], "locked_first")) {
      "FIRST"
    } else {
      "ALL"
    }
  }

  if (identical(minute, "FIRST")) {
    second <- "FIRST"
  }

  if (identical(minute, "ALL")) {
    second <- if (identical(spec$second_mode[[1]], "locked_first")) {
      "FIRST"
    } else {
      "ALL"
    }
  }

  list(
    hour = hour,
    minute = minute,
    second = second
  )
}


goes_estimate_files_for_product_time <- function(product, hour, minute, second) {
  normalized <- goes_normalize_product_time(
    product = product,
    hour = hour,
    minute = minute,
    second = second
  )

  hour <- normalized$hour
  minute <- normalized$minute
  second <- normalized$second

  if (identical(hour, "FIRST")) {
    return(1L)
  }

  if (identical(hour, "ALL")) {
    if (identical(product, "LSTF")) {
      return(24L)
    }

    if (product %in% c("MCMIPF", "FDCF")) {
      return(24L * 6L)
    }

    if (identical(product, "GLM")) {
      return(24L * 60L * 3L)
    }
  }

  if (identical(product, "LSTF")) {
    return(1L)
  }

  if (product %in% c("MCMIPF", "FDCF")) {
    if (identical(minute, "FIRST")) {
      return(1L)
    }

    if (identical(minute, "ALL")) {
      return(6L)
    }

    return(1L)
  }

  if (identical(product, "GLM")) {
    if (identical(minute, "FIRST")) {
      return(1L)
    }

    if (identical(minute, "ALL")) {
      return(60L * 3L)
    }

    if (identical(second, "FIRST")) {
      return(1L)
    }

    if (identical(second, "ALL")) {
      return(3L)
    }

    return(1L)
  }

  NA_integer_
}


# ------------------------------------------------------------------------------
# 5) Prefijos y stamps esperados
# ------------------------------------------------------------------------------

goes_make_day_prefix <- function(product_code, year, julian_day) {
  sprintf(
    "%s/%04d/%03d/",
    product_code,
    as.integer(year),
    as.integer(julian_day)
  )
}


goes_expected_stamps_for_product <- function(
    product,
    year,
    julian_day,
    hour,
    minute,
    second
) {
  year <- as.integer(year)
  julian_day <- as.integer(julian_day)

  base <- sprintf("%04d%03d", year, julian_day)

  norm <- goes_normalize_product_time(
    product = product,
    hour = hour,
    minute = minute,
    second = second
  )

  hour <- norm$hour
  minute <- norm$minute
  second <- norm$second

  out <- list()

  add_row <- function(hh, mm, ss) {
    expected_stamp <- paste0(base, hh, mm, ss)
    known_stamp <- gsub("X", "", expected_stamp)

    out[[length(out) + 1]] <<- data.frame(
      product = product,
      expected_stamp = expected_stamp,
      known_stamp = known_stamp,
      stringsAsFactors = FALSE
    )
  }

  if (identical(hour, "FIRST")) {
    add_row("XX", "XX", "XX")
    return(dplyr::bind_rows(out))
  }

  if (identical(product, "LSTF")) {
    hours <- if (identical(hour, "ALL")) {
      sprintf("%02d", 0:23)
    } else {
      hour
    }

    for (hh in hours) {
      add_row(hh, "XX", "XX")
    }

    return(dplyr::bind_rows(out))
  }

  if (product %in% c("MCMIPF", "FDCF")) {
    hours <- if (identical(hour, "ALL")) {
      sprintf("%02d", 0:23)
    } else {
      hour
    }

    for (hh in hours) {
      if (identical(minute, "FIRST")) {
        add_row(hh, "XX", "XX")
      } else {
        minutes <- if (identical(minute, "ALL")) {
          sprintf("%02d", seq(0, 50, 10))
        } else {
          minute
        }

        for (mm in minutes) {
          add_row(hh, mm, "XX")
        }
      }
    }

    return(dplyr::bind_rows(out))
  }

  if (identical(product, "GLM")) {
    hours <- if (identical(hour, "ALL")) {
      sprintf("%02d", 0:23)
    } else {
      hour
    }

    for (hh in hours) {
      if (identical(minute, "FIRST")) {
        add_row(hh, "XX", "XX")
      } else {
        minutes <- if (identical(minute, "ALL")) {
          sprintf("%02d", 0:59)
        } else {
          minute
        }

        for (mm in minutes) {
          if (identical(second, "FIRST")) {
            add_row(hh, mm, "XX")
          } else {
            seconds <- if (identical(second, "ALL")) {
              c("00", "20", "40")
            } else {
              second
            }

            for (ss in seconds) {
              add_row(hh, mm, ss)
            }
          }
        }
      }
    }

    return(dplyr::bind_rows(out))
  }

  dplyr::bind_rows(out)
}


# ------------------------------------------------------------------------------
# 6) Crear inventario esperado
# ------------------------------------------------------------------------------

goes_make_expected_inventory <- function(
    position,
    year,
    date_format = c("gregorian", "julian"),
    month = NULL,
    day = NULL,
    julian_day = NULL,
    product_times
) {
  date_format <- match.arg(date_format)

  date_info <- goes_validate_date(
    year = year,
    date_format = date_format,
    month = month,
    day = day,
    julian_day = julian_day
  )

  sat_info <- goes_resolve_satellite(
    position = position,
    date = date_info$gregorian_date
  )

  g_code <- goes_satellite_code(sat_info$satellite)

  rows <- lapply(product_times, function(x) {
    product <- x$product
    product_code <- goes_product_code(product)

    stamps <- goes_expected_stamps_for_product(
      product = product,
      year = date_info$year,
      julian_day = date_info$julian_day,
      hour = x$hour,
      minute = x$minute,
      second = x$second
    )

    stamps$product_code <- product_code
    stamps$satellite <- sat_info$satellite
    stamps$goes_code <- g_code
    stamps$bucket <- sat_info$bucket
    stamps$year <- date_info$year
    stamps$julian_day <- sprintf("%03d", date_info$julian_day)
    stamps$gregorian_date <- date_info$gregorian_date

    stamps$day_prefix <- goes_make_day_prefix(
      product_code = product_code,
      year = date_info$year,
      julian_day = date_info$julian_day
    )

    stamps$file_minimum_pattern <- paste0(
      "OR_",
      product_code,
      "..._",
      g_code,
      "_s",
      stamps$known_stamp
    )

    stamps
  })

  dplyr::bind_rows(rows)
}


# ------------------------------------------------------------------------------
# 7) Listar archivos online en S3
# ------------------------------------------------------------------------------

goes_list_s3_prefix_paginated <- function(bucket, prefix, max_pages = 50) {
  all_keys <- character()
  all_sizes <- numeric()

  continuation_token <- NULL
  page <- 1

  repeat {
    url <- paste0(
      "https://",
      bucket,
      ".s3.amazonaws.com/?list-type=2&prefix=",
      utils::URLencode(prefix, reserved = TRUE)
    )

    if (!is.null(continuation_token)) {
      url <- paste0(
        url,
        "&continuation-token=",
        utils::URLencode(continuation_token, reserved = TRUE)
      )
    }

    res <- httr::GET(url)

    if (httr::status_code(res) != 200) {
      stop("Error consultando S3. HTTP status: ", httr::status_code(res))
    }

    txt <- httr::content(
      res,
      as = "text",
      encoding = "UTF-8"
    )

    doc <- xml2::read_xml(txt)
    doc <- xml2::xml_ns_strip(doc)

    keys <- xml2::xml_text(
      xml2::xml_find_all(doc, ".//Contents/Key")
    )

    sizes <- xml2::xml_text(
      xml2::xml_find_all(doc, ".//Contents/Size")
    )

    if (length(keys) > 0) {
      all_keys <- c(all_keys, keys)
      all_sizes <- c(all_sizes, suppressWarnings(as.numeric(sizes)))
    }

    is_truncated <- xml2::xml_text(
      xml2::xml_find_first(doc, ".//IsTruncated")
    )

    if (!identical(tolower(is_truncated), "true")) {
      break
    }

    continuation_token <- xml2::xml_text(
      xml2::xml_find_first(doc, ".//NextContinuationToken")
    )

    if (is.na(continuation_token) || !nzchar(continuation_token)) {
      break
    }

    page <- page + 1

    if (page > max_pages) {
      warning("Se alcanzó max_pages al consultar S3 para prefix: ", prefix)
      break
    }
  }

  if (length(all_keys) == 0) {
    return(data.frame(
      key = character(),
      file = character(),
      size_online = numeric(),
      url = character(),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    key = all_keys,
    file = basename(all_keys),
    size_online = all_sizes,
    url = paste0(
      "https://",
      bucket,
      ".s3.amazonaws.com/",
      all_keys
    ),
    stringsAsFactors = FALSE
  )
}


goes_list_online_files_for_inventory <- function(expected_inventory) {
  if (is.null(expected_inventory) || nrow(expected_inventory) == 0) {
    return(data.frame(
      key = character(),
      file = character(),
      size_online = numeric(),
      url = character(),
      stringsAsFactors = FALSE
    ))
  }

  prefixes <- unique(expected_inventory[, c("bucket", "day_prefix")])

  out <- lapply(seq_len(nrow(prefixes)), function(i) {
    goes_list_s3_prefix_paginated(
      bucket = prefixes$bucket[i],
      prefix = prefixes$day_prefix[i]
    )
  })

  dplyr::bind_rows(out) |>
    dplyr::distinct(.data$key, .keep_all = TRUE)
}


# ------------------------------------------------------------------------------
# 8) Listar archivos locales
# ------------------------------------------------------------------------------

goes_list_local_nc_files <- function(download_dir) {
  download_dir <- normalizePath(
    download_dir,
    winslash = "/",
    mustWork = FALSE
  )

  if (!dir.exists(download_dir)) {
    return(data.frame(
      path = character(),
      file = character(),
      size_local = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  paths <- list.files(
    download_dir,
    pattern = "\\.nc$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(paths) == 0) {
    return(data.frame(
      path = character(),
      file = character(),
      size_local = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    path = normalizePath(
      paths,
      winslash = "/",
      mustWork = FALSE
    ),
    file = basename(paths),
    size_local = file.info(paths)$size,
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------------------------
# 9) Verificar inventario esperado vs local/online
# ------------------------------------------------------------------------------

goes_make_filename_regex <- function(product_code, goes_code, known_stamp) {
  paste0(
    "^OR_",
    stringr::str_replace_all(product_code, "-", "\\\\-"),
    ".*_",
    goes_code,
    "_s",
    known_stamp
  )
}


goes_verify_expected_inventory <- function(
    expected_inventory,
    local_files,
    online_files
) {
  if (is.null(expected_inventory) || nrow(expected_inventory) == 0) {
    return(data.frame())
  }

  rows <- lapply(seq_len(nrow(expected_inventory)), function(i) {
    x <- expected_inventory[i, ]

    rx <- goes_make_filename_regex(
      product_code = x$product_code,
      goes_code = x$goes_code,
      known_stamp = x$known_stamp
    )

    local_idx <- which(grepl(rx, local_files$file))
    online_idx <- which(grepl(rx, online_files$file))

    local_found <- length(local_idx) > 0
    online_found <- length(online_idx) > 0

    local_file <- NA_character_
    local_path <- NA_character_
    local_size <- NA_real_

    online_file <- NA_character_
    online_key <- NA_character_
    online_url <- NA_character_
    online_size <- NA_real_

    if (local_found) {
      j <- local_idx[order(local_files$file[local_idx])][1]

      local_file <- local_files$file[j]
      local_path <- local_files$path[j]
      local_size <- local_files$size_local[j]
    }

    if (online_found) {
      j <- online_idx[order(online_files$file[online_idx])][1]

      online_file <- online_files$file[j]
      online_key <- online_files$key[j]
      online_url <- online_files$url[j]
      online_size <- online_files$size_online[j]
    }

    size_match <- !is.na(local_size) &&
      !is.na(online_size) &&
      isTRUE(local_size == online_size)

    action <- dplyr::case_when(
      local_found && online_found && size_match ~ "OK",
      local_found && online_found && !size_match ~ "Delete and Download",
      !local_found && online_found ~ "Download",
      local_found && !online_found ~ "Local only",
      TRUE ~ "No online"
    )

    data.frame(
      product = x$product,
      expected_stamp = x$expected_stamp,
      known_stamp = x$known_stamp,
      file_minimum_pattern = x$file_minimum_pattern,

      local_found = local_found,
      local_file = local_file,
      local_size = local_size,
      local_size_mb = ifelse(
        is.na(local_size),
        NA,
        round(local_size / 1024^2, 3)
      ),

      online_found = online_found,
      online_file = online_file,
      online_size = online_size,
      online_size_mb = ifelse(
        is.na(online_size),
        NA,
        round(online_size / 1024^2, 3)
      ),

      size_match = ifelse(
        local_found && online_found,
        size_match,
        NA
      ),

      action = action,
      n_local_matches = length(local_idx),
      n_online_matches = length(online_idx),

      local_path = local_path,
      online_key = online_key,
      online_url = online_url,

      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(rows)
}


# ------------------------------------------------------------------------------
# 10) Crear plan de descarga
# ------------------------------------------------------------------------------

goes_make_download_plan <- function(verification_table, download_dir) {
  if (is.null(verification_table) || nrow(verification_table) == 0) {
    return(data.frame())
  }

  plan <- verification_table |>
    dplyr::filter(.data$action %in% c("Download", "Delete and Download")) |>
    dplyr::filter(!is.na(.data$online_url), nzchar(.data$online_url))

  if (nrow(plan) == 0) {
    return(data.frame())
  }

  plan$bucket <- sub(
    pattern = "^https://([^/]+)\\.s3\\.amazonaws\\.com/.*$",
    replacement = "\\1",
    x = plan$online_url
  )

  plan$destination <- file.path(
    normalizePath(
      download_dir,
      winslash = "/",
      mustWork = FALSE
    ),
    plan$bucket,
    plan$online_key
  )

  plan
}


# ------------------------------------------------------------------------------
# 11) Descargar archivos del plan
# ------------------------------------------------------------------------------

goes_safe_download_from_url <- function(url, destfile, overwrite = TRUE) {
  dir.create(
    dirname(destfile),
    recursive = TRUE,
    showWarnings = FALSE
  )

  tmp <- paste0(destfile, ".partial")

  if (file.exists(tmp)) {
    unlink(tmp)
  }

  ok <- tryCatch({
    utils::download.file(
      url = url,
      destfile = tmp,
      mode = "wb",
      quiet = TRUE
    )

    TRUE
  }, error = function(e) {
    warning("Error descargando archivo: ", conditionMessage(e))
    FALSE
  })

  if (!ok || !file.exists(tmp)) {
    if (file.exists(tmp)) {
      unlink(tmp)
    }

    return(FALSE)
  }

  if (file.exists(destfile)) {
    if (isTRUE(overwrite)) {
      unlink(destfile)
    } else {
      unlink(tmp)
      return(TRUE)
    }
  }

  file.rename(tmp, destfile) && file.exists(destfile)
}


goes_download_plan <- function(
    plan,
    overwrite = TRUE,
    status_fun = message
) {
  if (is.null(plan) || nrow(plan) == 0) {
    status_fun("No hay archivos para descargar.")

    return(data.frame(
      product = character(),
      file = character(),
      destination = character(),
      status = character(),
      detail = character(),
      stringsAsFactors = FALSE
    ))
  }

  results <- vector("list", nrow(plan))

  status_fun("Inicio de descarga usando plan generado.")
  status_fun("No se hará nueva consulta S3 durante la descarga.")

  for (i in seq_len(nrow(plan))) {
    file_name <- plan$online_file[i]
    destfile <- plan$destination[i]
    url <- plan$online_url[i]
    action <- plan$action[i]

    status_fun("--------------------------------------------------")
    status_fun(sprintf("Archivo [%s/%s]: %s", i, nrow(plan), file_name))
    status_fun(paste("Acción:", action))
    status_fun(paste("Destino:", destfile))

    if (identical(action, "Delete and Download") && file.exists(destfile)) {
      status_fun("Borrando archivo local previo por diferencia de tamaño...")
      unlink(destfile)
    }

    ok <- goes_safe_download_from_url(
      url = url,
      destfile = destfile,
      overwrite = overwrite
    )

    if (ok) {
      local_size <- file.info(destfile)$size
      expected_size <- plan$online_size[i]

      if (!is.na(expected_size) &&
          !is.na(local_size) &&
          local_size != expected_size) {
        status <- "error"
        detail <- paste0(
          "Descargado, pero tamaño diferente. Local=",
          local_size,
          " Online=",
          expected_size
        )

        status_fun(paste("ERROR:", detail))
      } else {
        status <- "ok"
        detail <- paste0(
          "Descargado. Peso: ",
          goes_format_bytes(local_size)
        )

        status_fun(paste("OK.", detail))
      }
    } else {
      status <- "error"
      detail <- "No se pudo descargar desde la URL."

      status_fun(paste("ERROR:", detail))
    }

    results[[i]] <- data.frame(
      product = plan$product[i],
      file = file_name,
      destination = destfile,
      status = status,
      detail = detail,
      stringsAsFactors = FALSE
    )
  }

  out <- dplyr::bind_rows(results)

  status_fun("--------------------------------------------------")
  status_fun("Descarga finalizada.")
  status_fun(paste("OK:", sum(out$status == "ok", na.rm = TRUE)))
  status_fun(paste("Errores:", sum(out$status == "error", na.rm = TRUE)))

  out
}


# ------------------------------------------------------------------------------
# 12) Pipeline completo: inventario -> online/local -> verificación -> plan
# ------------------------------------------------------------------------------

goes_prepare_download_plan <- function(
    position,
    year,
    date_format = c("gregorian", "julian"),
    month = NULL,
    day = NULL,
    julian_day = NULL,
    product_times,
    download_dir
) {
  date_format <- match.arg(date_format)

  expected <- goes_make_expected_inventory(
    position = position,
    year = year,
    date_format = date_format,
    month = month,
    day = day,
    julian_day = julian_day,
    product_times = product_times
  )

  online <- goes_list_online_files_for_inventory(expected)

  local <- goes_list_local_nc_files(download_dir)

  verification <- goes_verify_expected_inventory(
    expected_inventory = expected,
    local_files = local,
    online_files = online
  )

  plan <- goes_make_download_plan(
    verification_table = verification,
    download_dir = download_dir
  )

  list(
    expected_inventory = expected,
    online_files = online,
    local_files = local,
    verification_table = verification,
    download_plan = plan
  )
}


goes_prepare_and_download <- function(
    position,
    year,
    date_format = c("gregorian", "julian"),
    month = NULL,
    day = NULL,
    julian_day = NULL,
    product_times,
    download_dir,
    overwrite = TRUE,
    status_fun = message
) {
  date_format <- match.arg(date_format)

  prepared <- goes_prepare_download_plan(
    position = position,
    year = year,
    date_format = date_format,
    month = month,
    day = day,
    julian_day = julian_day,
    product_times = product_times,
    download_dir = download_dir
  )

  download_result <- goes_download_plan(
    plan = prepared$download_plan,
    overwrite = overwrite,
    status_fun = status_fun
  )

  prepared$download_result <- download_result

  prepared
}

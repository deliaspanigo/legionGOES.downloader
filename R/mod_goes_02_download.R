# ==============================================================================
# MODULE: GOES 02 DOWNLOAD - BACKGROUND PROCESS VERSION
# File: R/mod_goes_02_download.R
# ==============================================================================

library(shiny)
library(DT)
library(bslib)

# ==============================================================================
# BACKGROUND WORKER
# ==============================================================================

goes_02_download_worker <- function(
    pending_rds,
    status_rds,
    log_file
) {

  append_log <- function(...) {
    line <- sprintf(
      "[%s] %s",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
      paste0(..., collapse = "")
    )

    cat(line, "\n", file = log_file, append = TRUE)
  }

  safe_file_size <- function(path) {
    if (!file.exists(path)) {
      return(NA_real_)
    }

    as.numeric(file.info(path)$size)
  }

  pending <- readRDS(pending_rds)

  status <- data.frame(
    row_id = pending$.row_id,
    file = pending$file,
    url = pending$url,
    destination = pending$local_path,
    status = rep("pending", nrow(pending)),
    detail = rep("", nrow(pending)),
    local_found = rep(FALSE, nrow(pending)),
    local_size_bytes = rep(NA_real_, nrow(pending)),
    size_match = rep(NA, nrow(pending)),
    downloaded_at_utc = rep(NA_character_, nrow(pending)),
    stringsAsFactors = FALSE
  )

  saveRDS(status, status_rds)

  append_log("Background R process started.")
  append_log("Pending files: ", nrow(pending))

  for (i in seq_len(nrow(pending))) {

    append_log("[", i, "/", nrow(pending), "] Downloading ", pending$file[[i]])

    destination <- pending$local_path[[i]]
    url <- pending$url[[i]]
    action <- pending$action[[i]]

    expected_size <- if ("size_online" %in% names(pending)) {
      pending$size_online[[i]]
    } else {
      NA_real_
    }

    result <- tryCatch(
      {
        dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)

        if (file.exists(destination) && action == "Delete and Download") {
          unlink(destination)
        }

        utils::download.file(
          url = url,
          destfile = destination,
          mode = "wb",
          quiet = TRUE
        )

        local_found <- file.exists(destination)
        local_size <- safe_file_size(destination)

        if (is.na(expected_size)) {
          size_match <- local_found
        } else {
          size_match <- local_found &&
            !is.na(local_size) &&
            local_size == expected_size
        }

        list(
          ok = isTRUE(size_match),
          local_found = local_found,
          local_size = local_size,
          size_match = size_match,
          detail = ifelse(
            isTRUE(size_match),
            "Downloaded and size matched.",
            "Downloaded but size did not match."
          )
        )
      },
      error = function(e) {
        list(
          ok = FALSE,
          local_found = file.exists(destination),
          local_size = safe_file_size(destination),
          size_match = FALSE,
          detail = conditionMessage(e)
        )
      }
    )

    status$local_found[[i]] <- result$local_found
    status$local_size_bytes[[i]] <- result$local_size
    status$size_match[[i]] <- result$size_match
    status$downloaded_at_utc[[i]] <- format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S UTC",
      tz = "UTC"
    )

    if (isTRUE(result$ok)) {
      status$status[[i]] <- "ok"
      status$detail[[i]] <- result$detail
      append_log("OK: ", pending$file[[i]])
    } else {
      status$status[[i]] <- "error"
      status$detail[[i]] <- result$detail
      append_log("ERROR: ", pending$file[[i]], " | ", result$detail)
    }

    saveRDS(status, status_rds)
  }

  append_log("Background R process finished.")
  append_log("OK: ", sum(status$status == "ok", na.rm = TRUE))
  append_log("Errors: ", sum(status$status == "error", na.rm = TRUE))

  invisible(TRUE)
}

# ==============================================================================
# UI
# ==============================================================================

mod_goes_02_download_ui <- function(id) {

  ns <- NS(id)

  tagList(

    tags$style(
      HTML(
        sprintf(
          "
          #%s {
            background: #071018 !important;
            color: #b8f7c1 !important;
          }

          #%s pre {
            background: transparent !important;
            color: #b8f7c1 !important;
            border: none !important;
            box-shadow: none !important;
            margin: 0 !important;
            padding: 0 !important;
            white-space: pre-wrap !important;
            font-family: Consolas, monospace !important;
          }

          #%s .dataTables_wrapper {
            width: 100%% !important;
            overflow-x: auto !important;
          }

          #%s .dataTables_wrapper {
            width: 100%% !important;
            overflow-x: auto !important;
          }
          ",
          ns("log_console"),
          ns("log_console"),
          ns("files_table"),
          ns("download_status_table")
        )
      )
    ),

    tags$script(
      HTML(
        sprintf(
          "
          (function() {

            var followLog = true;
            var lastScrollHeight = 0;
            var consoleId = '%s';

            function getConsole() {
              return document.getElementById(consoleId);
            }

            function isNearBottom(el) {
              var distanceFromBottom = el.scrollHeight - el.scrollTop - el.clientHeight;
              return distanceFromBottom < 40;
            }

            function scrollToBottomIfFollowing() {
              var el = getConsole();

              if (!el) {
                return;
              }

              if (followLog) {
                el.scrollTop = el.scrollHeight;
              }

              lastScrollHeight = el.scrollHeight;
            }

            document.addEventListener('DOMContentLoaded', function() {

              var el = getConsole();

              if (!el) {
                return;
              }

              el.addEventListener('scroll', function() {
                followLog = isNearBottom(el);
              });
            });

            setInterval(function() {

              var el = getConsole();

              if (!el) {
                return;
              }

              if (el.scrollHeight !== lastScrollHeight) {
                scrollToBottomIfFollowing();
              }

            }, 500);

          })();
          ",
          ns("log_console")
        )
      )
    ),

    fluidRow(

      column(
        width = 3,

        wellPanel(
          h4("Actions"),

          actionButton(
            inputId = ns("clear_state"),
            label = "Reset",
            class = "btn btn-outline-secondary",
            width = "100%"
          ),

          br(),
          br(),

          actionButton(
            inputId = ns("build_table"),
            label = "Build URL table",
            class = "btn btn-primary",
            width = "100%"
          ),

          br(),
          br(),

          actionButton(
            inputId = ns("download_pending"),
            label = "Start download",
            class = "btn btn-success",
            width = "100%"
          ),

          br(),
          br(),

          actionButton(
            inputId = ns("stop_download"),
            label = "Stop download",
            class = "btn btn-warning",
            width = "100%"
          )
        )
      ),

      column(
        width = 9,

        div(
          style = "height: 42vh; overflow-y: auto;",

          bslib::navset_tab(

            bslib::nav_panel(
              title = "Summary",
              br(),
              verbatimTextOutput(ns("summary"))
            ),

            bslib::nav_panel(
              title = "Files",
              br(),
              DTOutput(ns("files_table"), width = "100%")
            ),

            bslib::nav_panel(
              title = "Download status",
              br(),
              DTOutput(ns("download_status_table"), width = "100%")
            )
          )
        ),

        h4("Console"),

        div(
          id = ns("log_console"),
          style = paste(
            "background: #071018;",
            "color: #b8f7c1;",
            "font-family: Consolas, monospace;",
            "white-space: pre-wrap;",
            "height: 36vh;",
            "overflow-y: auto;",
            "border-radius: 4px;",
            "padding: 12px;",
            "border: 1px solid #ddd;"
          ),
          verbatimTextOutput(ns("log"))
        )
      )
    )
  )
}

# ==============================================================================
# SERVER
# ==============================================================================

mod_goes_02_download_server <- function(
    id,
    params_r,
    data_raw_dir,
    max_pages = 20
) {

  moduleServer(id, function(input, output, session) {

    rv <- reactiveValues(
      files = NULL,
      status = NULL,
      log = character(),
      running = FALSE,
      job = NULL,
      job_dir = NULL,
      log_file = NULL,
      status_rds = NULL,
      pending_rds = NULL
    )

    get_data_raw_dir <- function() {
      if (is.function(data_raw_dir)) {
        return(data_raw_dir())
      }

      data_raw_dir
    }

    add_log <- function(...) {
      line <- sprintf(
        "[%s] %s",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
        paste0(..., collapse = "")
      )

      rv$log <- c(rv$log, line)

      if (!is.null(rv$log_file)) {
        cat(line, "\n", file = rv$log_file, append = TRUE)
      }
    }

    clear_state <- function() {

      if (isTRUE(rv$running) && !is.null(rv$job)) {
        try(rv$job$kill(), silent = TRUE)
      }

      rv$files <- NULL
      rv$status <- NULL
      rv$log <- character()
      rv$running <- FALSE
      rv$job <- NULL
      rv$job_dir <- NULL
      rv$log_file <- NULL
      rv$status_rds <- NULL
      rv$pending_rds <- NULL
    }

    empty_dt <- function(msg) {
      datatable(
        data.frame(Message = msg),
        rownames = FALSE,
        options = list(dom = "t")
      )
    }

    map_single_hour_rule <- function(x) {

      if (is.null(x) || is.na(x)) {
        return("less_equal")
      }

      switch(
        x,
        "Less than or equal" = "less_equal",
        "Greater than or equal" = "greater_equal",
        "Less than" = "less",
        "Greater than" = "greater",
        "Exact" = "exact",
        "less_equal"
      )
    }

    add_local_status <- function(files) {

      if (is.null(files) || nrow(files) == 0) {
        return(files)
      }

      data_raw <- get_data_raw_dir()

      files$local_path <- vapply(
        seq_len(nrow(files)),
        function(i) {
          fn_goes_make_local_path(
            data_raw_dir = data_raw,
            bucket = files$bucket[[i]],
            product_code = files$product_code[[i]],
            start_time_utc = files$start_time_utc[[i]],
            filename = files$file[[i]]
          )
        },
        character(1)
      )

      files$local_found <- file.exists(files$local_path)

      files$local_size_bytes <- vapply(
        files$local_path,
        function(path) {
          if (!file.exists(path)) {
            return(NA_real_)
          }

          as.numeric(file.info(path)$size)
        },
        numeric(1)
      )

      if (!"size_online" %in% names(files)) {
        files$size_online <- NA_real_
      }

      files$size_match <- files$local_found &
        !is.na(files$local_size_bytes) &
        !is.na(files$size_online) &
        files$local_size_bytes == files$size_online

      files$action <- ifelse(
        files$local_found & files$size_match,
        "OK",
        ifelse(
          files$local_found & !files$size_match,
          "Delete and Download",
          "Download"
        )
      )

      files$checked_at_utc <- format(
        Sys.time(),
        "%Y-%m-%d %H:%M:%S UTC",
        tz = "UTC"
      )

      files
    }

    keep_first_file_each_hour <- function(files) {

      if (is.null(files) || nrow(files) == 0) {
        return(files)
      }

      files$hour_key_tmp <- format(
        files$start_time_utc,
        "%Y-%m-%d %H",
        tz = "UTC"
      )

      split_key <- paste(
        files$product,
        files$bucket,
        files$hour_key_tmp,
        sep = "|"
      )

      out <- lapply(split(files, split_key), function(x) {
        x <- x[order(x$start_time_utc, x$file), , drop = FALSE]
        x[1, , drop = FALSE]
      })

      out <- do.call(rbind, out)
      out$hour_key_tmp <- NULL
      rownames(out) <- NULL

      out
    }

    get_single_hour_times <- function(params) {

      dates <- seq.Date(
        from = as.Date(params$date_from),
        to = as.Date(params$date_to),
        by = "day"
      )

      hms <- format(
        as.POSIXct(params$from_utc, tz = "UTC"),
        "%H:%M:%S",
        tz = "UTC"
      )

      as.POSIXct(
        paste(dates, hms),
        tz = "UTC"
      )
    }

    get_files_from_params <- function(params) {

      if (is.null(params)) {
        stop("No parameters received from module 01.")
      }

      position <- params$position
      product <- params$product

      if (params$date_mode == "Now (Last file online)") {

        add_log("Getting latest available file online...")
        add_log("Searching backwards from UTC time: ", as.character(params$from_utc))

        x <- fn_goes_get_latest_file_online(
          position = position,
          product = product,
          time_utc = params$from_utc,
          lookback_hours = 72,
          max_pages = max_pages
        )

        return(x)
      }

      if (params$time_mode == "Single hour") {

        rule <- map_single_hour_rule(params$single_hour_search)
        times <- get_single_hour_times(params)

        add_log("Getting one file per selected day using single-hour rule: ", rule)

        out <- lapply(times, function(tt) {
          fn_goes_get_file_candidate(
            position = position,
            product = product,
            time_utc = tt,
            rule = rule,
            max_pages = max_pages
          )
        })

        out <- out[
          vapply(out, function(x) !is.null(x) && nrow(x) > 0, logical(1))
        ]

        if (length(out) == 0) {
          return(data.frame())
        }

        return(do.call(rbind, out))
      }

      add_log("Getting all URLs between selected UTC times...")

      x <- fn_goes_get_urls_between(
        position = position,
        product = product,
        from_utc = params$from_utc,
        to_utc = params$to_utc,
        max_pages = max_pages
      )

      if (
        params$time_mode == "Time range" &&
        !is.null(params$time_range_files) &&
        params$time_range_files == "Only the first file of each hour of each day"
      ) {
        add_log("Keeping only the first file of each hour.")
        x <- keep_first_file_each_hour(x)
      }

      x
    }

    sync_files_from_status <- function(status) {

      if (is.null(rv$files) || is.null(status) || nrow(status) == 0) {
        return()
      }

      for (j in seq_len(nrow(status))) {

        i <- status$row_id[[j]]

        if (is.na(i) || i < 1 || i > nrow(rv$files)) {
          next
        }

        rv$files$local_found[[i]] <- status$local_found[[j]]
        rv$files$local_size_bytes[[i]] <- status$local_size_bytes[[j]]
        rv$files$size_match[[i]] <- status$size_match[[j]]
        rv$files$download_status[[i]] <- status$status[[j]]
        rv$files$download_detail[[i]] <- status$detail[[j]]
        rv$files$downloaded_at_utc[[i]] <- status$downloaded_at_utc[[j]]

        if (identical(status$status[[j]], "ok")) {
          rv$files$action[[i]] <- "OK"
        }
      }
    }

    observe({
      invalidateLater(1000, session)

      if (!is.null(rv$log_file) && file.exists(rv$log_file)) {
        rv$log <- readLines(rv$log_file, warn = FALSE)
      }

      if (!is.null(rv$status_rds) && file.exists(rv$status_rds)) {
        st <- readRDS(rv$status_rds)
        rv$status <- st
        sync_files_from_status(st)
      }

      if (isTRUE(rv$running) && !is.null(rv$job)) {

        alive <- tryCatch(
          rv$job$is_alive(),
          error = function(e) FALSE
        )

        if (!alive) {

          exit_status <- tryCatch(
            rv$job$get_exit_status(),
            error = function(e) NA_integer_
          )

          rv$running <- FALSE

          if (!is.null(rv$files) && nrow(rv$files) > 0) {
            rv$files <- add_local_status(rv$files)
          }

          if (!is.na(exit_status) && exit_status == 0) {
            add_log("Background process ended successfully.")
            showNotification("Download process finished.", type = "message")
          } else {
            add_log("Background process ended with exit status: ", exit_status)
            showNotification("Download process ended with errors.", type = "warning")
          }

          rv$job <- NULL
        }
      }
    })

    observeEvent(input$build_table, {

      tryCatch(
        {
          if (isTRUE(rv$running)) {
            showNotification(
              "A download is running. Stop it before rebuilding the table.",
              type = "warning"
            )
            return()
          }

          clear_state()

          params <- params_r()
          data_raw <- get_data_raw_dir()

          dir.create(data_raw, recursive = TRUE, showWarnings = FALSE)

          add_log("Building URL table...")
          add_log("Data raw directory: ", normalizePath(data_raw, winslash = "/", mustWork = FALSE))

          files <- get_files_from_params(params)

          if (is.null(files) || nrow(files) == 0) {
            add_log("No online files found for the selected parameters.")
            rv$files <- data.frame()
            return()
          }

          files <- files[!duplicated(files$key), , drop = FALSE]
          files <- files[order(files$start_time_utc, files$file), , drop = FALSE]
          rownames(files) <- NULL

          files <- add_local_status(files)

          files$download_status <- NA_character_
          files$download_detail <- NA_character_
          files$downloaded_at_utc <- NA_character_

          rv$files <- files

          add_log("Files found: ", nrow(files))
          add_log("Already OK locally: ", sum(files$action == "OK", na.rm = TRUE))
          add_log("Pending download: ", sum(files$action == "Download", na.rm = TRUE))
          add_log("Pending delete and download: ", sum(files$action == "Delete and Download", na.rm = TRUE))

          showNotification("URL table created.", type = "message")
        },
        error = function(e) {
          add_log("ERROR: ", conditionMessage(e))
          showNotification(paste("Error:", conditionMessage(e)), type = "error")
        }
      )
    })

    observeEvent(input$download_pending, {

      if (!requireNamespace("callr", quietly = TRUE)) {
        showNotification(
          "Package 'callr' is required. Run install.packages('callr').",
          type = "error"
        )
        return()
      }

      if (isTRUE(rv$running)) {
        showNotification("A download is already running.", type = "warning")
        return()
      }

      if (is.null(rv$files) || nrow(rv$files) == 0) {
        showNotification("No files table available. Build it first.", type = "warning")
        return()
      }

      pending_idx <- which(rv$files$action %in% c("Download", "Delete and Download"))

      if (length(pending_idx) == 0) {
        showNotification("There are no pending files to download.", type = "message")
        return()
      }

      job_dir <- file.path(
        tempdir(),
        paste0("goes_download_", format(Sys.time(), "%Y%m%d_%H%M%S"))
      )

      dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)

      rv$job_dir <- job_dir
      rv$log_file <- file.path(job_dir, "download.log")
      rv$status_rds <- file.path(job_dir, "status.rds")
      rv$pending_rds <- file.path(job_dir, "pending.rds")

      writeLines(rv$log, rv$log_file)

      pending <- rv$files[pending_idx, , drop = FALSE]
      pending$.row_id <- pending_idx

      saveRDS(pending, rv$pending_rds)

      add_log("Launching background R process...")
      add_log("Pending files: ", nrow(pending))
      add_log("Job directory: ", normalizePath(job_dir, winslash = "/", mustWork = FALSE))

      rv$job <- callr::r_bg(
        func = goes_02_download_worker,
        args = list(
          pending_rds = rv$pending_rds,
          status_rds = rv$status_rds,
          log_file = rv$log_file
        ),
        supervise = TRUE
      )

      rv$running <- TRUE

      showNotification("Background download process started.", type = "message")
    })

    observeEvent(input$stop_download, {

      if (!isTRUE(rv$running) || is.null(rv$job)) {
        showNotification("No background download process is running.", type = "message")
        return()
      }

      try(rv$job$kill(), silent = TRUE)
      rv$running <- FALSE
      add_log("Background download process was stopped by the user.")

      showNotification("Download process stopped.", type = "warning")
    })

    observeEvent(input$clear_state, {
      clear_state()
      showNotification("State cleared.", type = "message")
    })

    output$summary <- renderText({

      data_raw <- get_data_raw_dir()

      if (is.null(rv$files)) {
        return(paste(
          "No URL table has been built yet.",
          paste0("Data raw directory: ", normalizePath(data_raw, winslash = "/", mustWork = FALSE)),
          sep = "\n"
        ))
      }

      if (nrow(rv$files) == 0) {
        return(paste(
          "No files found for the selected parameters.",
          paste0("Data raw directory: ", normalizePath(data_raw, winslash = "/", mustWork = FALSE)),
          sep = "\n"
        ))
      }

      paste(
        paste0("Data raw directory: ", normalizePath(data_raw, winslash = "/", mustWork = FALSE)),
        "",
        paste0("Files found: ", nrow(rv$files)),
        paste0("Already OK locally: ", sum(rv$files$action == "OK", na.rm = TRUE)),
        paste0("Pending Download: ", sum(rv$files$action == "Download", na.rm = TRUE)),
        paste0("Pending Delete and Download: ", sum(rv$files$action == "Delete and Download", na.rm = TRUE)),
        paste0("Running background process: ", rv$running),
        sep = "\n"
      )
    })

    output$files_table <- renderDT({

      if (is.null(rv$files) || nrow(rv$files) == 0) {
        return(empty_dt("No files yet."))
      }

      datatable(
        rv$files,
        rownames = FALSE,
        filter = "top",
        class = "compact nowrap",
        width = "100%",
        options = list(
          pageLength = 6,
          scrollY = "24vh",
          scrollX = TRUE,
          autoWidth = TRUE
        )
      )
    })

    output$download_status_table <- renderDT({

      if (is.null(rv$status) || nrow(rv$status) == 0) {
        return(empty_dt("No download status yet."))
      }

      datatable(
        rv$status,
        rownames = FALSE,
        filter = "top",
        class = "compact nowrap",
        width = "100%",
        options = list(
          pageLength = 6,
          scrollY = "24vh",
          scrollX = TRUE,
          autoWidth = TRUE
        )
      )
    })

    output$log <- renderText({
      paste(rv$log, collapse = "\n")
    })

    return(
      reactive({
        list(
          files = rv$files,
          status = rv$status,
          log = rv$log,
          running = rv$running,
          job_dir = rv$job_dir,
          data_raw_dir = get_data_raw_dir()
        )
      })
    )
  })
}

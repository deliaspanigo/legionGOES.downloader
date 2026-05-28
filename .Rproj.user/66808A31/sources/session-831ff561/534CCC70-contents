# ==============================================================================
# MODULE: GOES 01 PARAMS
# File: R/mod_goes_01_params.R
# ------------------------------------------------------------------------------
# Purpose:
#   Collect download parameters for GOES files.
#
# Returns a reactive list with:
#   position
#   product
#   date_mode
#   time_mode
#   date_from
#   date_to
#   from_utc
#   to_utc
#   single_hour_search
#   time_range_files
#   goes_detail
#   goes_segments
# ==============================================================================

library(shiny)
library(shinyjs)

# ==============================================================================
# Helper objects
# ==============================================================================

goes_01_hours <- sprintf("%02d", 0:23)
goes_01_min_sec <- sprintf("%02d", 0:59)

goes_01_table <- data.frame(
  position = c("EAST", "EAST", "WEST", "WEST"),
  goes = c("GOES-16", "GOES-19", "GOES-17", "GOES-18"),
  start_utc = as.POSIXct(
    c(
      "2017-12-18 00:00:00",  # GOES-16 as GOES-East
      "2025-04-07 15:00:00",  # GOES-19 as GOES-East
      "2019-02-12 00:00:00",  # GOES-17 as GOES-West
      "2023-01-04 18:00:00"   # GOES-18 as GOES-West
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
  )
)

goes_01_now_utc <- function() {
  as.POSIXct(
    format(Sys.time(), tz = "UTC", usetz = FALSE),
    tz = "UTC"
  )
}

goes_01_today_utc <- function() {
  as.Date(goes_01_now_utc(), tz = "UTC")
}

goes_01_format_utc <- function(x) {
  format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
}

goes_01_make_datetime_utc <- function(date, hour, minute, second) {
  as.POSIXct(
    paste(
      as.Date(date),
      paste(hour, minute, second, sep = ":")
    ),
    tz = "UTC"
  )
}

goes_01_goes_segments <- function(position, from_utc, to_utc) {

  x <- goes_01_table[goes_01_table$position == position, ]

  end_aux <- x$end_utc
  end_aux[is.na(end_aux)] <- as.POSIXct("9999-12-31 00:00:00", tz = "UTC")

  overlaps <- from_utc < end_aux & to_utc >= x$start_utc

  x <- x[overlaps, , drop = FALSE]
  end_aux <- end_aux[overlaps]

  if (nrow(x) == 0) {
    return(data.frame(
      position = character(),
      goes = character(),
      from_utc = as.POSIXct(character(), tz = "UTC"),
      to_utc = as.POSIXct(character(), tz = "UTC"),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    position = x$position,
    goes = x$goes,
    from_utc = pmax(x$start_utc, from_utc),
    to_utc = pmin(end_aux, to_utc),
    stringsAsFactors = FALSE
  )
}

goes_01_goes_detail <- function(position, from_utc, to_utc) {

  segments <- goes_01_goes_segments(
    position = position,
    from_utc = from_utc,
    to_utc = to_utc
  )

  if (nrow(segments) == 0) {
    return(
      "The GOES satellite could not be determined for this date/time range."
    )
  }

  parts <- paste0(
    segments$goes,
    " from ",
    goes_01_format_utc(segments$from_utc),
    " to ",
    goes_01_format_utc(segments$to_utc)
  )

  if (nrow(segments) == 1) {
    paste0("Corresponding satellite: ", parts, ".")
  } else {
    paste0(
      "Warning: the selected range crosses a GOES satellite transition. ",
      "The following satellites apply: ",
      paste(parts, collapse = "; "),
      "."
    )
  }
}

# ==============================================================================
# UI
# ==============================================================================

mod_goes_01_params_ui <- function(id) {

  ns <- NS(id)

  tagList(

    shinyjs::useShinyjs(),

    h3("GOES Parameters"),

    # --------------------------------------------------------------------------
    # Clocks
    # --------------------------------------------------------------------------
    fluidRow(
      column(
        width = 6,
        wellPanel(
          strong("UTC time"),
          br(),
          textOutput(ns("clock_utc"))
        )
      ),
      column(
        width = 6,
        wellPanel(
          strong("System time"),
          br(),
          textOutput(ns("clock_system"))
        )
      )
    ),

    # --------------------------------------------------------------------------
    # Position
    # --------------------------------------------------------------------------
    fluidRow(
      column(
        width = 4,
        selectInput(
          inputId = ns("position"),
          label = "Position",
          choices = c("WEST", "EAST"),
          selected = "WEST"
        )
      )
    ),

    # --------------------------------------------------------------------------
    # Dates
    # --------------------------------------------------------------------------
    fluidRow(
      column(
        width = 4,
        selectInput(
          inputId = ns("date_mode"),
          label = "Dates",
          choices = c(
            "Now (Last file online)",
            "Single day",
            "Date range"
          ),
          selected = "Now (Last file online)"
        )
      ),
      column(
        width = 4,
        dateInput(
          inputId = ns("date_from"),
          label = "From",
          value = goes_01_today_utc(),
          format = "yyyy-mm-dd",
          language = "en"
        ),
        div(
          style = "font-size: 12px; color: #666;",
          textOutput(ns("julian_from"))
        )
      ),
      column(
        width = 4,
        dateInput(
          inputId = ns("date_to"),
          label = "To",
          value = goes_01_today_utc(),
          format = "yyyy-mm-dd",
          language = "en"
        ),
        div(
          style = "font-size: 12px; color: #666;",
          textOutput(ns("julian_to"))
        )
      )
    ),

    # --------------------------------------------------------------------------
    # Times
    # --------------------------------------------------------------------------
    fluidRow(
      column(
        width = 4,
        selectInput(
          inputId = ns("time_mode"),
          label = "Time",
          choices = c("Full day", "Single hour", "Time range"),
          selected = "Full day"
        )
      ),

      column(
        width = 4,
        h4("From"),
        fluidRow(
          column(
            width = 4,
            selectInput(
              inputId = ns("hour_from"),
              label = "Hour",
              choices = goes_01_hours,
              selected = "00",
              selectize = FALSE
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = ns("min_from"),
              label = "Min",
              choices = goes_01_min_sec,
              selected = "00",
              selectize = FALSE
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = ns("sec_from"),
              label = "Sec",
              choices = goes_01_min_sec,
              selected = "00",
              selectize = FALSE
            )
          )
        )
      ),

      column(
        width = 4,
        h4("To"),
        fluidRow(
          column(
            width = 4,
            selectInput(
              inputId = ns("hour_to"),
              label = "Hour",
              choices = c("", goes_01_hours),
              selected = "23",
              selectize = FALSE
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = ns("min_to"),
              label = "Min",
              choices = c("", goes_01_min_sec),
              selected = "59",
              selectize = FALSE
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = ns("sec_to"),
              label = "Sec",
              choices = c("", goes_01_min_sec),
              selected = "59",
              selectize = FALSE
            )
          )
        )
      )
    ),

    # --------------------------------------------------------------------------
    # Search detail for single hour
    # --------------------------------------------------------------------------
    hidden(
      div(
        id = ns("single_hour_search_panel"),
        fluidRow(
          column(
            width = 6,
            selectInput(
              inputId = ns("single_hour_search"),
              label = "Single hour search detail",
              choices = c(
                "Greater than or equal",
                "Less than or equal",
                "Greater than",
                "Less than",
                "Exact"
              ),
              selected = "Exact"
            )
          )
        )
      )
    ),

    # --------------------------------------------------------------------------
    # File detail for time range
    # --------------------------------------------------------------------------
    hidden(
      div(
        id = ns("time_range_files_panel"),
        fluidRow(
          column(
            width = 6,
            selectInput(
              inputId = ns("time_range_files"),
              label = "Files for time range",
              choices = c(
                "All files in the selected time range",
                "Only the first file of each hour of each day"
              ),
              selected = "All files in the selected time range"
            )
          )
        )
      )
    ),

    # --------------------------------------------------------------------------
    # Product
    # --------------------------------------------------------------------------
    fluidRow(
      column(
        width = 4,
        radioButtons(
          inputId = ns("product"),
          label = "Product",
          choices = c("LSTF", "MCMIPF", "FDCF", "GLM"),
          selected = "LSTF"
        )
      )
    ),

    hr(),

    # --------------------------------------------------------------------------
    # Summary
    # --------------------------------------------------------------------------
    fluidRow(
      column(
        width = 12,
        h4("Parameter summary"),
        wellPanel(
          textOutput(ns("params_summary"))
        )
      )
    )
  )
}

# ==============================================================================
# SERVER
# ==============================================================================

mod_goes_01_params_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # --------------------------------------------------------------------------
    # Clocks
    # --------------------------------------------------------------------------
    output$clock_utc <- renderText({
      invalidateLater(1000, session)
      format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
    })

    output$clock_system <- renderText({
      invalidateLater(1000, session)
      format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    })

    # --------------------------------------------------------------------------
    # Julian dates
    # --------------------------------------------------------------------------
    output$julian_from <- renderText({
      req(input$date_from)

      date <- as.Date(input$date_from)
      paste0(
        "Day: ",
        format(date, "%j"),
        " | Year: ",
        format(date, "%Y")
      )
    })

    output$julian_to <- renderText({
      req(input$date_to)

      date <- as.Date(input$date_to)
      paste0(
        "Day: ",
        format(date, "%j"),
        " | Year: ",
        format(date, "%Y")
      )
    })

    # --------------------------------------------------------------------------
    # Helpers for UI state
    # --------------------------------------------------------------------------
    disable_time_controls <- function() {
      disable(ns("time_mode"))
      disable(ns("hour_from"))
      disable(ns("min_from"))
      disable(ns("sec_from"))
      disable(ns("hour_to"))
      disable(ns("min_to"))
      disable(ns("sec_to"))
      disable(ns("single_hour_search"))
    }

    apply_now <- function() {

      now_utc <- goes_01_now_utc()

      date_utc <- as.Date(now_utc, tz = "UTC")
      hour_utc <- format(now_utc, "%H", tz = "UTC")
      min_utc <- format(now_utc, "%M", tz = "UTC")
      sec_utc <- format(now_utc, "%S", tz = "UTC")

      updateDateInput(session, "date_from", value = date_utc)
      updateDateInput(session, "date_to", value = date_utc)

      updateSelectInput(session, "time_mode", selected = "Single hour")

      updateSelectInput(
        session,
        "single_hour_search",
        selected = "Less than or equal"
      )

      updateSelectInput(session, "hour_from", selected = hour_utc)
      updateSelectInput(session, "min_from", selected = min_utc)
      updateSelectInput(session, "sec_from", selected = sec_utc)

      updateSelectInput(session, "hour_to", selected = "")
      updateSelectInput(session, "min_to", selected = "")
      updateSelectInput(session, "sec_to", selected = "")

      show(ns("single_hour_search_panel"))
      hide(ns("time_range_files_panel"))

      disable(ns("date_from"))
      disable(ns("date_to"))
      disable_time_controls()
    }

    # --------------------------------------------------------------------------
    # Show / hide auxiliary panels
    # --------------------------------------------------------------------------
    observe({
      req(input$time_mode, input$date_mode)

      if (input$time_mode == "Single hour") {
        show(ns("single_hour_search_panel"))
      } else {
        hide(ns("single_hour_search_panel"))
      }

      if (
        input$time_mode == "Time range" &&
        input$date_mode != "Now (Last file online)"
      ) {
        show(ns("time_range_files_panel"))
      } else {
        hide(ns("time_range_files_panel"))
      }
    })

    # --------------------------------------------------------------------------
    # Date mode logic
    # --------------------------------------------------------------------------
    observeEvent(input$date_mode, {

      if (input$date_mode == "Now (Last file online)") {
        apply_now()
      }

      if (input$date_mode == "Single day") {

        enable(ns("date_from"))
        disable(ns("date_to"))
        enable(ns("time_mode"))
        enable(ns("single_hour_search"))

        updateDateInput(session, "date_to", value = input$date_from)
      }

      if (input$date_mode == "Date range") {

        enable(ns("date_from"))
        enable(ns("date_to"))
        enable(ns("time_mode"))
        enable(ns("single_hour_search"))
      }

    }, ignoreInit = FALSE)

    observeEvent(input$date_from, {
      if (input$date_mode == "Single day") {
        updateDateInput(session, "date_to", value = input$date_from)
      }
    })

    # --------------------------------------------------------------------------
    # Time mode logic
    # --------------------------------------------------------------------------
    observe({

      req(input$time_mode)

      if (input$date_mode == "Now (Last file online)") {
        return()
      }

      if (input$time_mode == "Full day") {

        updateSelectInput(session, "hour_from", selected = "00")
        updateSelectInput(session, "min_from", selected = "00")
        updateSelectInput(session, "sec_from", selected = "00")

        updateSelectInput(session, "hour_to", selected = "23")
        updateSelectInput(session, "min_to", selected = "59")
        updateSelectInput(session, "sec_to", selected = "59")

        disable(ns("hour_from"))
        disable(ns("min_from"))
        disable(ns("sec_from"))

        disable(ns("hour_to"))
        disable(ns("min_to"))
        disable(ns("sec_to"))
      }

      if (input$time_mode == "Single hour") {

        enable(ns("hour_from"))
        enable(ns("min_from"))
        enable(ns("sec_from"))
        enable(ns("single_hour_search"))

        updateSelectInput(session, "hour_to", selected = "")
        updateSelectInput(session, "min_to", selected = "")
        updateSelectInput(session, "sec_to", selected = "")

        disable(ns("hour_to"))
        disable(ns("min_to"))
        disable(ns("sec_to"))
      }

      if (input$time_mode == "Time range") {

        enable(ns("hour_from"))
        enable(ns("min_from"))
        enable(ns("sec_from"))

        enable(ns("hour_to"))
        enable(ns("min_to"))
        enable(ns("sec_to"))

        if (is.null(input$hour_to) || input$hour_to == "") {
          updateSelectInput(session, "hour_to", selected = "23")
        }

        if (is.null(input$min_to) || input$min_to == "") {
          updateSelectInput(session, "min_to", selected = "59")
        }

        if (is.null(input$sec_to) || input$sec_to == "") {
          updateSelectInput(session, "sec_to", selected = "59")
        }
      }
    })

    # --------------------------------------------------------------------------
    # Main reactive parameter object
    # --------------------------------------------------------------------------
    params <- reactive({

      req(
        input$position,
        input$product,
        input$date_mode,
        input$time_mode,
        input$date_from,
        input$date_to
      )

      date_from <- as.Date(input$date_from)
      date_to <- as.Date(input$date_to)

      if (input$date_mode == "Now (Last file online)") {

        from_utc <- goes_01_make_datetime_utc(
          date = date_from,
          hour = input$hour_from,
          minute = input$min_from,
          second = input$sec_from
        )

        to_utc <- from_utc
      }

      if (input$date_mode != "Now (Last file online)" &&
          input$time_mode == "Full day") {

        from_utc <- goes_01_make_datetime_utc(
          date = date_from,
          hour = "00",
          minute = "00",
          second = "00"
        )

        to_utc <- goes_01_make_datetime_utc(
          date = date_to,
          hour = "23",
          minute = "59",
          second = "59"
        )
      }

      if (input$date_mode != "Now (Last file online)" &&
          input$time_mode == "Single hour") {

        from_utc <- goes_01_make_datetime_utc(
          date = date_from,
          hour = input$hour_from,
          minute = input$min_from,
          second = input$sec_from
        )

        to_utc <- goes_01_make_datetime_utc(
          date = date_to,
          hour = input$hour_from,
          minute = input$min_from,
          second = input$sec_from
        )
      }

      if (input$date_mode != "Now (Last file online)" &&
          input$time_mode == "Time range") {

        from_utc <- goes_01_make_datetime_utc(
          date = date_from,
          hour = input$hour_from,
          minute = input$min_from,
          second = input$sec_from
        )

        to_utc <- goes_01_make_datetime_utc(
          date = date_to,
          hour = input$hour_to,
          minute = input$min_to,
          second = input$sec_to
        )
      }

      segments <- goes_01_goes_segments(
        position = input$position,
        from_utc = from_utc,
        to_utc = to_utc
      )

      detail <- goes_01_goes_detail(
        position = input$position,
        from_utc = from_utc,
        to_utc = to_utc
      )

      list(
        position = input$position,
        product = input$product,
        products = input$product,

        date_mode = input$date_mode,
        time_mode = input$time_mode,

        date_from = date_from,
        date_to = date_to,

        hour_from = input$hour_from,
        min_from = input$min_from,
        sec_from = input$sec_from,

        hour_to = input$hour_to,
        min_to = input$min_to,
        sec_to = input$sec_to,

        from_utc = from_utc,
        to_utc = to_utc,

        single_hour_search = input$single_hour_search,
        time_range_files = input$time_range_files,

        julian_day_from = format(date_from, "%j"),
        julian_year_from = format(date_from, "%Y"),
        julian_day_to = format(date_to, "%j"),
        julian_year_to = format(date_to, "%Y"),

        goes_detail = detail,
        goes_segments = segments
      )
    })

    # --------------------------------------------------------------------------
    # User-facing summary
    # --------------------------------------------------------------------------
    output$params_summary <- renderText({

      x <- params()

      if (x$date_mode == "Now (Last file online)") {

        paste0(
          "You are selecting the latest available online file for position ",
          x$position,
          ", product ",
          x$product,
          ". This search uses the current UTC date and time: ",
          goes_01_format_utc(x$from_utc),
          ". This mode implies a single hour search with the criterion ",
          "'Less than or equal' to that moment. ",
          "Julian date: day ",
          x$julian_day_from,
          ", year ",
          x$julian_year_from,
          ". ",
          x$goes_detail
        )

      } else if (x$time_mode == "Full day") {

        paste0(
          "You are selecting files for position ",
          x$position,
          ", product ",
          x$product,
          ", using ",
          tolower(x$date_mode),
          ", from ",
          goes_01_format_utc(x$from_utc),
          " to ",
          goes_01_format_utc(x$to_utc),
          ". Initial Julian date: day ",
          x$julian_day_from,
          ", year ",
          x$julian_year_from,
          ". Final Julian date: day ",
          x$julian_day_to,
          ", year ",
          x$julian_year_to,
          ". ",
          x$goes_detail
        )

      } else if (x$time_mode == "Single hour") {

        paste0(
          "You are selecting files for position ",
          x$position,
          ", product ",
          x$product,
          ", using ",
          tolower(x$date_mode),
          ", from ",
          goes_01_format_utc(x$from_utc),
          " to ",
          goes_01_format_utc(x$to_utc),
          ", with search criterion: ",
          x$single_hour_search,
          ". Initial Julian date: day ",
          x$julian_day_from,
          ", year ",
          x$julian_year_from,
          ". Final Julian date: day ",
          x$julian_day_to,
          ", year ",
          x$julian_year_to,
          ". ",
          x$goes_detail
        )

      } else if (x$time_mode == "Time range") {

        paste0(
          "You are selecting files for position ",
          x$position,
          ", product ",
          x$product,
          ", using ",
          tolower(x$date_mode),
          ", from ",
          goes_01_format_utc(x$from_utc),
          " to ",
          goes_01_format_utc(x$to_utc),
          ". File mode: ",
          x$time_range_files,
          ". Initial Julian date: day ",
          x$julian_day_from,
          ", year ",
          x$julian_year_from,
          ". Final Julian date: day ",
          x$julian_day_to,
          ", year ",
          x$julian_year_to,
          ". ",
          x$goes_detail
        )
      }
    })

    return(params)
  })
}

# ==============================================================================
# Minimal usage example
# ==============================================================================

# ui <- fluidPage(
#   shinyjs::useShinyjs(),
#   mod_goes_01_params_ui("goes_params")
# )
#
# server <- function(input, output, session) {
#   goes_params <- mod_goes_01_params_server("goes_params")
#
#   observe({
#     print(goes_params())
#   })
# }
#
# shinyApp(ui, server)

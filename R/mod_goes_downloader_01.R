# ===============================================================================
# MODULO SHINY - GOES DOWNLOADER
#
# Uso en una app principal:
#
# ui <- fluidPage(
#   shinyjs::useShinyjs(),
#   mod_goes_downloader_ui("goesdl")
# )
#
# server <- function(input, output, session) {
#   mod_goes_downloader_server("goesdl")
# }
#
# shinyApp(ui, server)
#
# Paquetes requeridos:
# shiny, shinyjs, dplyr, httr, xml2, stringr, DT
# ===============================================================================

# ===============================================================================
# UI MODULE
# ===============================================================================

mod_goes_downloader_01_ui <- function(id) {

  ns <- shiny::NS(id)

  PRODUCT_SPECS <- data.frame(
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
    minute_mode = c("locked_first", "by_10", "by_10", "by_1"),
    second_mode = c("locked_first", "locked_first", "locked_first", "by_20"),
    stringsAsFactors = FALSE
  )

  PRODUCT_CHOICES <- PRODUCT_SPECS$product

  product_spec <- function(product) {
    PRODUCT_SPECS[PRODUCT_SPECS$product == product, , drop = FALSE]
  }

  time_input_ids <- function(product) {
    list(
      hour = paste0("time_", product, "_hour"),
      minute = paste0("time_", product, "_minute"),
      second = paste0("time_", product, "_second")
    )
  }

  product_use_id <- function(product) {
    paste0("use_", product)
  }

  hour_choices <- function() {
    c("ALL", "FIRST", sprintf("%02d", 0:23))
  }

  minute_choices_for_product <- function(product) {
    spec <- product_spec(product)

    if (identical(spec$minute_mode[[1]], "locked_first")) {
      return("FIRST")
    }

    if (identical(spec$minute_mode[[1]], "by_10")) {
      return(c("ALL", "FIRST", sprintf("%02d", seq(0, 50, 10))))
    }

    if (identical(spec$minute_mode[[1]], "by_1")) {
      return(c("ALL", "FIRST", sprintf("%02d", 0:59)))
    }

    "FIRST"
  }

  second_choices_for_product <- function(product) {
    spec <- product_spec(product)

    if (identical(spec$second_mode[[1]], "locked_first")) {
      return("FIRST")
    }

    if (identical(spec$second_mode[[1]], "by_20")) {
      return(c("ALL", "FIRST", "00", "20", "40"))
    }

    "FIRST"
  }

  default_minute_for_product <- function(product) {
    choices <- minute_choices_for_product(product)

    if ("00" %in% choices) {
      return("00")
    }

    "FIRST"
  }

  default_second_for_product <- function(product) {
    choices <- second_choices_for_product(product)

    if ("00" %in% choices) {
      return("00")
    }

    "FIRST"
  }

  lock_switch_ui <- function(input_id, locked = FALSE) {
    initial_class <- if (isTRUE(locked)) {
      "custom-lock-switch locked"
    } else {
      "custom-lock-switch unlocked"
    }

    initial_icon <- if (isTRUE(locked)) {
      "\U0001F512"
    } else {
      "\U0001F513"
    }

    shiny::tags$button(
      id = ns(input_id),
      type = "button",
      class = initial_class,
      `data-locked` = ifelse(isTRUE(locked), "true", "false"),
      shiny::tags$span(class = "custom-lock-label closed-label", "LOCK"),
      shiny::tags$span(class = "custom-lock-label open-label", "OPEN"),
      shiny::tags$span(class = "custom-lock-knob", initial_icon)
    )
  }

  product_time_box <- function(product) {
    ids <- time_input_ids(product)
    spec <- product_spec(product)

    shiny::div(
      id = ns(paste0("box_time_", product)),
      class = "product-time-box compact-product-time-box",

      shiny::div(
        class = "product-time-header",
        shiny::div(
          class = "product-title-line",
          shiny::tags$strong(product),
          shiny::tags$span(
            class = "product-code-line",
            paste0(" | ", spec$product_code, " | ", spec$frequency_label)
          )
        )
      ),

      shiny::div(
        class = "product-time-controls-row",

        shiny::div(
          class = "compact-time-field",
          shiny::tags$label("Hora UTC", `for` = ns(ids$hour)),
          shiny::selectInput(
            inputId = ns(ids$hour),
            label = NULL,
            choices = hour_choices(),
            selected = "00",
            width = "100%"
          )
        ),

        shiny::div(
          class = "compact-time-field",
          shiny::tags$label("Minuto UTC", `for` = ns(ids$minute)),
          shiny::selectInput(
            inputId = ns(ids$minute),
            label = NULL,
            choices = minute_choices_for_product(product),
            selected = default_minute_for_product(product),
            width = "100%"
          )
        ),

        shiny::div(
          class = "compact-time-field",
          shiny::tags$label("Segundo UTC", `for` = ns(ids$second)),
          shiny::selectInput(
            inputId = ns(ids$second),
            label = NULL,
            choices = second_choices_for_product(product),
            selected = default_second_for_product(product),
            width = "100%"
          )
        ),

        shiny::div(
          class = "estimated-files-inline",
          shiny::uiOutput(ns(paste0("estimated_files_", product)))
        )
      ),

      shiny::div(
        class = "product-time-note",
        if (identical(product, "LSTF")) {
          "LSTF es horario: minuto y segundo quedan en FIRST."
        } else if (product %in% c("MCMIPF", "FDCF")) {
          "Producto cada 10 minutos: segundo queda en FIRST."
        } else {
          "GLM permite hora, minuto y segundo."
        }
      )
    )
  }

  product_rules_table <- function() {
    shiny::tags$table(
      class = "table table-sm table-bordered product-reference-table",
      shiny::tags$thead(
        shiny::tags$tr(
          shiny::tags$th("Usar"),
          shiny::tags$th("Producto"),
          shiny::tags$th("C\u00f3digo"),
          shiny::tags$th("Frecuencia"),
          shiny::tags$th("Minuto"),
          shiny::tags$th("Segundo")
        )
      ),
      shiny::tags$tbody(
        lapply(seq_len(nrow(PRODUCT_SPECS)), function(i) {
          product <- PRODUCT_SPECS$product[i]
          use_id <- product_use_id(product)

          minute_txt <- switch(
            PRODUCT_SPECS$minute_mode[i],
            "locked_first" = "FIRST",
            "by_10" = "00,10,...,50",
            "by_1" = "00 a 59",
            ""
          )

          second_txt <- switch(
            PRODUCT_SPECS$second_mode[i],
            "locked_first" = "FIRST",
            "by_20" = "00,20,40",
            ""
          )

          shiny::tags$tr(
            shiny::tags$td(
              shiny::checkboxInput(
                inputId = ns(use_id),
                label = NULL,
                value = identical(product, "FDCF"),
                width = "24px"
              )
            ),
            shiny::tags$td(product),
            shiny::tags$td(PRODUCT_SPECS$product_code[i]),
            shiny::tags$td(PRODUCT_SPECS$frequency_label[i]),
            shiny::tags$td(minute_txt),
            shiny::tags$td(second_txt)
          )
        })
      )
    )
  }

  shiny::tagList(
    shinyjs::useShinyjs(),

    shiny::tags$head(
      shiny::tags$style(shiny::HTML("\n        .goes-downloader-root {\n          position: relative !important;\n          width: 100% !important;\n          min-height: 100vh !important;\n          height: 100% !important;\n          margin: 0 !important;\n          padding: 0 !important;\n          background: #f2f4f7 !important;\n          overflow-x: hidden !important;\n          box-sizing: border-box !important;\n        }\n\n        .goes-downloader-root > .app-title {\n          font-size: 28px !important;\n          font-weight: 800 !important;\n          margin-top: 18px !important;\n          margin-bottom: 4px !important;\n          margin-left: 16px !important;\n          margin-right: 16px !important;\n        }\n\n        .goes-downloader-root > .app-subtitle {\n          color: #666 !important;\n          margin-bottom: 18px !important;\n          margin-left: 16px !important;\n          margin-right: 16px !important;\n        }\n\n        .goes-downloader-root > .tabbable {\n          width: 100% !important;\n          min-height: calc(100vh - 82px) !important;\n          background: #f2f4f7 !important;\n          box-sizing: border-box !important;\n        }\n\n        .goes-downloader-root > .tabbable > .nav-tabs {\n          margin-left: 0 !important;\n          margin-right: 0 !important;\n          padding-left: 16px !important;\n          padding-right: 16px !important;\n          background: #f2f4f7 !important;\n          box-sizing: border-box !important;\n        }\n\n        .goes-downloader-root > .tabbable > .tab-content {\n          width: 100% !important;\n          min-height: calc(100vh - 130px) !important;\n          background: #f2f4f7 !important;\n          padding: 16px !important;\n          box-sizing: border-box !important;\n        }\n\n        .goes-downloader-root > .tabbable > .tab-content > .tab-pane {\n          min-height: calc(100vh - 162px) !important;\n          background: #f2f4f7 !important;\n          box-sizing: border-box !important;\n        }\n\n        .goes-downloader-root .row {\n          margin-left: 0 !important;\n          margin-right: 0 !important;\n        }\n\n        .goes-downloader-root [class^='col-'],\n        .goes-downloader-root [class*=' col-'] {\n          padding-left: 8px !important;\n          padding-right: 8px !important;\n          box-sizing: border-box !important;\n        }\n\n        .goes-downloader-root .card-box {\n          background: white !important;\n          border-radius: 14px !important;\n          padding: 16px !important;\n          margin-bottom: 16px !important;\n          box-shadow: 0 2px 12px rgba(0,0,0,0.08) !important;\n          box-sizing: border-box !important;\n        }\n\n        .goes-downloader-root .control-bar {\n          background: #fff !important;\n          border-radius: 14px !important;\n          padding: 12px 16px !important;\n          margin-bottom: 16px !important;\n          box-shadow: 0 2px 12px rgba(0,0,0,0.08) !important;\n          display: flex !important;\n          align-items: center !important;\n          gap: 12px !important;\n          box-sizing: border-box !important;\n        }\n\n        .goes-downloader-root .custom-lock-switch {\n          width: 92px !important;\n          min-width: 92px !important;\n          height: 44px !important;\n          border-radius: 999px !important;\n          border: none !important;\n          padding: 4px !important;\n          cursor: pointer !important;\n          position: relative !important;\n          transition: background 0.25s ease !important;\n          box-shadow: inset 0 0 0 1px rgba(0,0,0,0.08) !important;\n          flex: 0 0 auto !important;\n        }\n\n        .goes-downloader-root .custom-lock-switch.unlocked {\n          background: #74c0fc !important;\n        }\n\n        .goes-downloader-root .custom-lock-switch.locked {\n          background: #2f9e44 !important;\n        }\n\n        .goes-downloader-root .custom-lock-knob {\n          width: 36px !important;\n          height: 36px !important;\n          border-radius: 50% !important;\n          background: white !important;\n          position: absolute !important;\n          top: 4px !important;\n          left: 4px !important;\n          display: flex !important;\n          align-items: center !important;\n          justify-content: center !important;\n          font-size: 20px !important;\n          transition: left 0.25s ease !important;\n          box-shadow: 0 2px 8px rgba(0,0,0,0.25) !important;\n        }\n\n        .goes-downloader-root .custom-lock-switch.locked .custom-lock-knob {\n          left: 52px !important;\n        }\n\n        .goes-downloader-root .custom-lock-label {\n          font-size: 11px !important;\n          font-weight: 700 !important;\n          color: white !important;\n          position: absolute !important;\n          top: 50% !important;\n          transform: translateY(-50%) !important;\n          pointer-events: none !important;\n          letter-spacing: 0.3px !important;\n        }\n\n        .goes-downloader-root .custom-lock-label.open-label {\n          right: 8px !important;\n        }\n\n        .goes-downloader-root .custom-lock-label.closed-label {\n          left: 8px !important;\n        }\n\n        .goes-downloader-root button.reset-button,\n        .goes-downloader-root .reset-button,\n        .goes-downloader-root .btn.reset-button,\n        .goes-downloader-root .action-button.reset-button {\n          display: inline-flex !important;\n          align-items: center !important;\n          justify-content: center !important;\n          width: 58px !important;\n          min-width: 58px !important;\n          max-width: 58px !important;\n          height: 44px !important;\n          min-height: 44px !important;\n          padding: 0 !important;\n          margin: 0 !important;\n          border-radius: 12px !important;\n          border: 1px solid #ffc078 !important;\n          background: #fff4e6 !important;\n          color: #e67700 !important;\n          font-size: 22px !important;\n          font-weight: 700 !important;\n          line-height: 1 !important;\n          box-shadow: none !important;\n          text-align: center !important;\n          vertical-align: middle !important;\n          flex: 0 0 auto !important;\n        }\n\n        .goes-downloader-root button.reset-button:hover,\n        .goes-downloader-root .reset-button:hover,\n        .goes-downloader-root .btn.reset-button:hover,\n        .goes-downloader-root .action-button.reset-button:hover {\n          background: #ffe8cc !important;\n          border-color: #ffa94d !important;\n          color: #d9480f !important;\n        }\n\n        .goes-downloader-root .control-text {\n          font-size: 13px !important;\n          color: #555 !important;\n        }\n\n        .goes-downloader-root .product-time-box {\n          background: #fff !important;\n          border: 1px solid #dee2e6 !important;\n          border-left: 6px solid #2c7be5 !important;\n          border-radius: 12px !important;\n          padding: 8px 10px !important;\n          margin-bottom: 8px !important;\n          box-shadow: 0 2px 8px rgba(0,0,0,0.05) !important;\n          box-sizing: border-box !important;\n        }\n\n        .goes-downloader-root .product-time-header {\n          margin-bottom: 4px !important;\n        }\n\n        .goes-downloader-root .product-title-line {\n          display: flex !important;\n          align-items: center !important;\n          gap: 6px !important;\n          font-size: 15px !important;\n          line-height: 1.15 !important;\n        }\n\n        .goes-downloader-root .product-code-line {\n          font-size: 12px !important;\n          color: #555 !important;\n          font-weight: normal !important;\n        }\n\n        .goes-downloader-root .product-time-controls-row {\n          display: grid !important;\n          grid-template-columns: 92px 92px 92px 1fr !important;\n          gap: 8px !important;\n          align-items: end !important;\n        }\n\n        .goes-downloader-root .compact-time-field {\n          margin: 0 !important;\n          padding: 0 !important;\n        }\n\n        .goes-downloader-root .compact-time-field label {\n          font-size: 10px !important;\n          margin-bottom: 1px !important;\n          color: #555 !important;\n          font-weight: 600 !important;\n        }\n\n        .goes-downloader-root .compact-time-field .form-group {\n          margin-bottom: 0 !important;\n        }\n\n        .goes-downloader-root .compact-time-field select,\n        .goes-downloader-root .compact-time-field .selectize-input {\n          min-height: 28px !important;\n          height: 28px !important;\n          padding: 2px 6px !important;\n          font-size: 12px !important;\n          line-height: 1.2 !important;\n        }\n\n        .goes-downloader-root .compact-time-field .selectize-control {\n          margin-bottom: 0 !important;\n        }\n\n        .goes-downloader-root .estimated-files-inline {\n          min-height: 28px !important;\n          display: flex !important;\n          align-items: end !important;\n        }\n\n        .goes-downloader-root .estimated-files-inline .summary-box {\n          width: 100% !important;\n          padding: 5px 8px !important;\n          border-radius: 7px !important;\n          font-size: 12px !important;\n          line-height: 1.2 !important;\n          margin: 0 !important;\n          white-space: normal !important;\n        }\n\n        .goes-downloader-root .product-time-note {\n          margin-top: 4px !important;\n          font-size: 11px !important;\n          color: #5c3d00 !important;\n          background: #fff4e6 !important;\n          border-left: 3px solid #f08c00 !important;\n          border-radius: 5px !important;\n          padding: 4px 7px !important;\n        }\n\n        .goes-downloader-root .summary-box {\n          background: #eef6ff !important;\n          border-left: 5px solid #2c7be5 !important;\n          padding: 12px !important;\n          border-radius: 8px !important;\n          font-family: Consolas, monospace !important;\n          font-size: 13px !important;\n          white-space: pre-wrap !important;\n          box-sizing: border-box !important;\n        }\n\n        .goes-downloader-root .locked-box {\n          background: #f0fff4 !important;\n          border-left: 5px solid #2f9e44 !important;\n          padding: 12px !important;\n          border-radius: 8px !important;\n          font-family: Consolas, monospace !important;\n          font-size: 13px !important;\n          white-space: pre-wrap !important;\n          box-sizing: border-box !important;\n        }\n\n        .goes-downloader-root .warning-box {\n          background: #fff4e6 !important;\n          border-left: 5px solid #f08c00 !important;\n          padding: 12px !important;\n          border-radius: 8px !important;\n          font-family: Consolas, monospace !important;\n          font-size: 13px !important;\n          white-space: pre-wrap !important;\n          box-sizing: border-box !important;\n        }\n\n        .goes-downloader-root .products-config-box {\n          font-family: Consolas, monospace !important;\n          font-size: 12px !important;\n          line-height: 1.35 !important;\n          white-space: pre !important;\n          overflow-x: auto !important;\n        }\n\n        .goes-downloader-root .sat-date-inline-box {\n          white-space: nowrap !important;\n          overflow-x: auto !important;\n          font-size: 12px !important;\n          padding: 7px 10px !important;\n        }\n\n        .goes-downloader-root .small-note {\n          color: #555 !important;\n          font-size: 12px !important;\n        }\n\n        .goes-downloader-root .download-log-box {\n          background: #071018 !important;\n          color: #b8f7c1 !important;\n          border-radius: 10px !important;\n          padding: 12px !important;\n          height: 320px !important;\n          overflow-y: auto !important;\n          font-family: Consolas, monospace !important;\n          font-size: 13px !important;\n          white-space: pre-wrap !important;\n          box-sizing: border-box !important;\n        }\n\n        .goes-downloader-root .rules-table-wrap,\n        .goes-downloader-root .wide-table-wrap {\n          width: 100% !important;\n          overflow-x: auto !important;\n          box-sizing: border-box !important;\n        }\n\n        .goes-downloader-root .product-reference-table {\n          width: 100% !important;\n          font-size: 11px !important;\n          margin-bottom: 0 !important;\n        }\n\n        .goes-downloader-root .product-reference-table th,\n        .goes-downloader-root .product-reference-table td {\n          padding: 4px 5px !important;\n          vertical-align: middle !important;\n        }\n\n        .goes-downloader-root .product-reference-table .form-group,\n        .goes-downloader-root .product-reference-table .checkbox {\n          margin: 0 !important;\n        }\n\n        .goes-downloader-root .product-reference-table input[type='checkbox'] {\n          margin: 0 !important;\n          position: static !important;\n        }\n\n        .goes-downloader-root table {\n          font-size: 12px !important;\n        }\n      ")),

      shiny::tags$script(shiny::HTML(sprintf("\n        (function() {\n          var rootId = '%s';\n          var handlerName = '%s';\n\n          function setLockSwitchState(id, locked) {\n            var el = document.getElementById(id);\n            if (!el) return;\n\n            var root = document.getElementById(rootId);\n            if (!root || !root.contains(el)) return;\n\n            var knob = el.querySelector('.custom-lock-knob');\n            el.dataset.locked = locked ? 'true' : 'false';\n\n            if (locked) {\n              el.classList.remove('unlocked');\n              el.classList.add('locked');\n              if (knob) knob.textContent = '\U0001F512';\n            } else {\n              el.classList.remove('locked');\n              el.classList.add('unlocked');\n              if (knob) knob.textContent = '\U0001F513';\n            }\n          }\n\n          document.addEventListener('click', function(e) {\n            var root = document.getElementById(rootId);\n            if (!root) return;\n\n            var sw = e.target.closest('.custom-lock-switch');\n            if (!sw || !root.contains(sw)) return;\n\n            var switchId = sw.id;\n            var currentlyLocked = sw.dataset.locked === 'true';\n            var nextLocked = !currentlyLocked;\n\n            Shiny.setInputValue(\n              switchId + '_clicked',\n              {locked: nextLocked, nonce: Math.random()},\n              {priority: 'event'}\n            );\n          });\n\n          Shiny.addCustomMessageHandler(handlerName, function(message) {\n            setLockSwitchState(message.id, message.locked);\n          });\n        })();\n      ", ns("goes_downloader_root"), ns("set_lock_switch"))))
    ),

    shiny::div(
      id = ns("goes_downloader_root"),
      class = "goes-downloader-root",

      shiny::div(class = "app-title", "LegionGOES Downloader"),
      shiny::div(
        class = "app-subtitle",
        "Sat\u00e9lite/fecha, productos/tiempo, verificaci\u00f3n congelada y descarga en segundo proceso."
      ),

      shiny::tabsetPanel(
        id = ns("main_tabs"),
        type = "tabs",

        shiny::tabPanel(
          title = "1. Sat\u00e9lite y fecha",
          value = "tab_sat_date",
          shiny::br(),

          shiny::div(
            class = "control-bar",
            lock_switch_ui("toggle_sat_date", locked = FALSE),
            shiny::actionButton(ns("reset_sat_date"), "\U0001F504", class = "reset-button"),
            shiny::div(class = "control-text", shiny::uiOutput(ns("sat_date_control_text")))
          ),

          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::div(
                class = "card-box",
                shiny::h4("Sat\u00e9lite"),
                shiny::selectInput(
                  inputId = ns("position"),
                  label = "Posici\u00f3n",
                  choices = c("EAST", "WEST"),
                  selected = "EAST"
                )
              ),
              shiny::div(
                class = "card-box",
                shiny::h4("Fecha"),
                shiny::numericInput(
                  inputId = ns("year"),
                  label = "A\u00f1o",
                  value = as.integer(format(Sys.Date(), "%Y")),
                  min = 2017,
                  max = 2100,
                  step = 1
                ),
                shiny::radioButtons(
                  inputId = ns("date_format"),
                  label = "Formato",
                  choices = c("Gregoriano" = "gregorian", "Juliano" = "julian"),
                  selected = "gregorian"
                ),
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] == 'gregorian'", ns("date_format")),
                  shiny::numericInput(
                    inputId = ns("month"),
                    label = "Mes",
                    value = as.integer(format(Sys.Date(), "%m")),
                    min = 1,
                    max = 12,
                    step = 1
                  ),
                  shiny::numericInput(
                    inputId = ns("day"),
                    label = "D\u00eda",
                    value = as.integer(format(Sys.Date(), "%d")),
                    min = 1,
                    max = 31,
                    step = 1
                  )
                ),
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] == 'julian'", ns("date_format")),
                  shiny::numericInput(
                    inputId = ns("julian_day"),
                    label = "D\u00eda juliano",
                    value = as.integer(format(Sys.Date(), "%j")),
                    min = 1,
                    max = 366,
                    step = 1
                  )
                )
              )
            ),
            shiny::column(
              width = 4,
              shiny::div(
                class = "card-box",
                shiny::h4("Estado Sat\u00e9lite/fecha"),
                shiny::uiOutput(ns("sat_date_state_box"))
              )
            ),
            shiny::column(
              width = 4,
              shiny::div(
                class = "card-box",
                shiny::h4("Configuraci\u00f3n Sat\u00e9lite/fecha"),
                shiny::uiOutput(ns("sat_date_config_box"))
              )
            )
          )
        ),

        shiny::tabPanel(
          title = "2. Productos y tiempo",
          value = "tab_products_time",
          shiny::br(),

          shiny::div(
            class = "control-bar",
            lock_switch_ui("toggle_products_time", locked = FALSE),
            shiny::actionButton(ns("reset_products_time"), "\U0001F504", class = "reset-button"),
            shiny::div(class = "control-text", shiny::uiOutput(ns("products_time_control_text")))
          ),

          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::div(
                class = "card-box",
                shiny::h4("Referencia de productos"),
                shiny::div(class = "rules-table-wrap", product_rules_table())
              ),
              shiny::div(
                class = "card-box",
                shiny::h4("Configuraci\u00f3n productos/tiempo"),
                shiny::uiOutput(ns("products_time_config_box"))
              )
            ),
            shiny::column(
              width = 6,
              shiny::div(
                class = "card-box",
                shiny::h4("Productos seleccionados"),
                shiny::uiOutput(ns("product_tab_warning")),
                shiny::div(
                  class = "small-note",
                  "Los selectores de tiempo aparecen solo para los productos marcados en la tabla de referencia."
                ),
                shiny::br(),
                product_time_box("LSTF"),
                product_time_box("MCMIPF"),
                product_time_box("FDCF"),
                product_time_box("GLM")
              )
            )
          ),

          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::div(
                class = "card-box",
                shiny::h4("Estado productos/tiempo"),
                shiny::uiOutput(ns("products_time_state_box"))
              )
            )
          )
        ),

        shiny::tabPanel(
          title = "3. Local / Online",
          value = "tab_local",
          shiny::br(),

          shiny::div(
            class = "control-bar",
            lock_switch_ui("toggle_download_plan", locked = FALSE),
            shiny::actionButton(ns("reset_download_plan"), "\U0001F504", class = "reset-button"),
            shiny::div(class = "control-text", shiny::uiOutput(ns("download_plan_control_text")))
          ),

          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::div(
                class = "card-box",
                shiny::h4("Carpeta local de descargas"),
                shiny::div(
                  class = "locked-box products-config-box",
                  shiny::textOutput(ns("text_path"))
                ),
                shiny::br(),
                shiny::actionButton(ns("verify_local_online"), "Verificar local y online", class = "btn btn-primary"),
                shiny::br(),
                shiny::br(),
                shiny::uiOutput(ns("verification_summary"))
              )
            ),
            shiny::column(
              width = 8,
              shiny::div(
                class = "card-box",
                shiny::h4("Detalle m\u00ednimo esperado"),
                shiny::div(
                  class = "small-note",
                  "Se muestra solo un ejemplo por producto. Las X indican posiciones no determinadas por la selecci\u00f3n."
                ),
                shiny::br(),
                shiny::div(class = "wide-table-wrap", shiny::tableOutput(ns("expected_inventory_table")))
              )
            )
          ),

          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::div(
                class = "card-box",
                shiny::h4("Verificaci\u00f3n local / online"),
                shiny::div(
                  class = "small-note",
                  "Esta tabla no descarga ni elimina archivos. Solo decide qu\u00e9 habr\u00eda que hacer despu\u00e9s."
                ),
                shiny::br(),
                shiny::div(class = "wide-table-wrap", DT::DTOutput(ns("verification_table")))
              )
            )
          ),

          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::div(
                class = "card-box",
                shiny::h4("Estado de lista congelada"),
                shiny::uiOutput(ns("download_plan_state_box"))
              )
            ),
            shiny::column(
              width = 8,
              shiny::div(
                class = "card-box",
                shiny::h4("Resumen de lo que se descargar\u00e1"),
                shiny::uiOutput(ns("download_plan_summary"))
              )
            )
          ),

          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::div(
                class = "card-box",
                shiny::h4("Lista congelada para descarga"),
                shiny::div(
                  class = "small-note",
                  "Esta lista sale exclusivamente de la verificaci\u00f3n de esta pesta\u00f1a. No se vuelve a consultar S3 al congelarla ni al descargar."
                ),
                shiny::br(),
                DT::DTOutput(ns("download_plan_table"))
              )
            )
          )
        ),

        shiny::tabPanel(
          title = "4. Descarga",
          value = "tab_download",
          shiny::br(),

          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::div(
                class = "card-box",
                shiny::h4("Descarga efectiva"),
                shiny::uiOutput(ns("download_ready_box")),
                shiny::br(),
                shiny::actionButton(ns("start_download"), "Download", class = "btn btn-success")
              )
            ),
            shiny::column(
              width = 8,
              shiny::div(
                class = "card-box",
                shiny::h4("Log de descarga"),
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
                shiny::h4("Estado de descarga"),
                DT::DTOutput(ns("download_status_table"))
              )
            )
          )
        )
      )
    )
  )
}


# ===============================================================================
# SERVER MODULE
# ===============================================================================

mod_goes_downloader_01_server <- function(id, str_folder_path_data_raw) {
  shiny::moduleServer(id, function(input, output, session) {



    PRODUCT_SPECS <- data.frame(
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
      minute_mode = c("locked_first", "by_10", "by_10", "by_1"),
      second_mode = c("locked_first", "locked_first", "locked_first", "by_20"),
      stringsAsFactors = FALSE
    )

    PRODUCT_CHOICES <- PRODUCT_SPECS$product

    # ---------------------------------------------------------------------------
    # Helpers internos del server
    # ---------------------------------------------------------------------------

    get_default_download_dir <- function() {

      the_folder <-  str_folder_path_data_raw
      normalizePath(the_folder, winslash = "/", mustWork = FALSE)
      # normalizePath(file.path(the_folder, "goes_downloads"), winslash = "/", mustWork = FALSE)
    }


    format_bytes <- function(x) {
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

    julian_to_gregorian <- function(year, julian_day) {
      as.Date(as.integer(julian_day) - 1, origin = sprintf("%04d-01-01", as.integer(year)))
    }

    validate_date_inputs <- function(year, date_format, month, day, julian_day) {
      year <- as.integer(year)

      if (is.na(year) || year < 2017 || year > 2100) {
        stop("A\u00f1o inv\u00e1lido.")
      }

      if (identical(date_format, "gregorian")) {
        date_txt <- sprintf("%04d-%02d-%02d", year, as.integer(month), as.integer(day))
        date <- as.Date(date_txt)

        if (is.na(date)) {
          stop("Fecha gregoriana inv\u00e1lida.")
        }

        if (!identical(format(date, "%Y"), sprintf("%04d", year))) {
          stop("La fecha gregoriana no pertenece al a\u00f1o indicado.")
        }

        return(list(
          year = year,
          gregorian_date = date,
          julian_day = as.integer(format(date, "%j"))
        ))
      }

      jd <- as.integer(julian_day)

      if (is.na(jd) || jd < 1 || jd > 366) {
        stop("D\u00eda juliano inv\u00e1lido.")
      }

      date <- julian_to_gregorian(year, jd)

      if (!identical(format(date, "%Y"), sprintf("%04d", year))) {
        stop("D\u00eda juliano inv\u00e1lido para ese a\u00f1o.")
      }

      list(year = year, gregorian_date = date, julian_day = jd)
    }

    resolve_goes_satellite <- function(position, date) {
      date <- as.Date(date)

      if (identical(position, "EAST")) {
        if (date >= as.Date("2025-04-07")) {
          return(list(position = "EAST", satellite = "GOES-19", bucket = "noaa-goes19", rule = "GOES-19 como GOES-East desde 2025-04-07"))
        }
        if (date >= as.Date("2017-12-18")) {
          return(list(position = "EAST", satellite = "GOES-16", bucket = "noaa-goes16", rule = "GOES-16 como GOES-East antes de GOES-19"))
        }
        stop("No hay regla definida para GOES-East antes de 2017-12-18.")
      }

      if (identical(position, "WEST")) {
        if (date >= as.Date("2023-01-04")) {
          return(list(position = "WEST", satellite = "GOES-18", bucket = "noaa-goes18", rule = "GOES-18 como GOES-West desde 2023-01-04"))
        }
        if (date >= as.Date("2019-02-12")) {
          return(list(position = "WEST", satellite = "GOES-17", bucket = "noaa-goes17", rule = "GOES-17 como GOES-West antes de GOES-18"))
        }
        stop("No hay regla definida para GOES-West antes de 2019-02-12.")
      }

      stop("Posici\u00f3n no reconocida: ", position)
    }

    goes_satellite_code <- function(satellite) {
      paste0("G", stringr::str_extract(satellite, "[0-9]+"))
    }

    product_spec <- function(product) {
      x <- PRODUCT_SPECS[PRODUCT_SPECS$product == product, , drop = FALSE]
      if (nrow(x) == 0) {
        stop("Producto no reconocido: ", product)
      }
      x
    }

    time_input_ids <- function(product) {
      list(
        hour = paste0("time_", product, "_hour"),
        minute = paste0("time_", product, "_minute"),
        second = paste0("time_", product, "_second")
      )
    }

    product_use_id <- function(product) {
      paste0("use_", product)
    }

    hour_choices <- function() {
      c("ALL", "FIRST", sprintf("%02d", 0:23))
    }

    minute_choices_for_product <- function(product) {
      spec <- product_spec(product)
      if (identical(spec$minute_mode[[1]], "locked_first")) return("FIRST")
      if (identical(spec$minute_mode[[1]], "by_10")) return(c("ALL", "FIRST", sprintf("%02d", seq(0, 50, 10))))
      if (identical(spec$minute_mode[[1]], "by_1")) return(c("ALL", "FIRST", sprintf("%02d", 0:59)))
      "FIRST"
    }

    second_choices_for_product <- function(product) {
      spec <- product_spec(product)
      if (identical(spec$second_mode[[1]], "locked_first")) return("FIRST")
      if (identical(spec$second_mode[[1]], "by_20")) return(c("ALL", "FIRST", "00", "20", "40"))
      "FIRST"
    }

    default_minute_for_product <- function(product) {
      choices <- minute_choices_for_product(product)
      if ("00" %in% choices) return("00")
      "FIRST"
    }

    default_second_for_product <- function(product) {
      choices <- second_choices_for_product(product)
      if ("00" %in% choices) return("00")
      "FIRST"
    }

    normalize_product_time <- function(product, hour, minute, second) {
      spec <- product_spec(product)

      if (identical(spec$minute_mode[[1]], "locked_first")) minute <- "FIRST"
      if (identical(spec$second_mode[[1]], "locked_first")) second <- "FIRST"

      if (identical(hour, "FIRST")) {
        minute <- "FIRST"
        second <- "FIRST"
      }

      if (identical(hour, "ALL")) {
        minute <- if (identical(spec$minute_mode[[1]], "locked_first")) "FIRST" else "ALL"
        second <- if (identical(spec$second_mode[[1]], "locked_first")) "FIRST" else "ALL"
      }

      if (identical(minute, "FIRST")) second <- "FIRST"
      if (identical(minute, "ALL")) second <- if (identical(spec$second_mode[[1]], "locked_first")) "FIRST" else "ALL"

      list(hour = hour, minute = minute, second = second)
    }

    estimate_files_for_product_time <- function(product, hour, minute, second) {
      normalized <- normalize_product_time(product, hour, minute, second)
      hour <- normalized$hour
      minute <- normalized$minute
      second <- normalized$second

      if (identical(hour, "FIRST")) return(1L)

      if (identical(hour, "ALL")) {
        if (identical(product, "LSTF")) return(24L)
        if (product %in% c("MCMIPF", "FDCF")) return(24L * 6L)
        if (identical(product, "GLM")) return(24L * 60L * 3L)
      }

      if (identical(product, "LSTF")) return(1L)

      if (product %in% c("MCMIPF", "FDCF")) {
        if (identical(minute, "FIRST")) return(1L)
        if (identical(minute, "ALL")) return(6L)
        return(1L)
      }

      if (identical(product, "GLM")) {
        if (identical(minute, "FIRST")) return(1L)
        if (identical(minute, "ALL")) return(60L * 3L)
        if (identical(second, "FIRST")) return(1L)
        if (identical(second, "ALL")) return(3L)
        return(1L)
      }

      NA_integer_
    }

    make_day_prefix <- function(product_code, year, julian_day) {
      sprintf("%s/%04d/%03d/", product_code, as.integer(year), as.integer(julian_day))
    }

    list_s3_prefix_paginated <- function(bucket, prefix, max_pages = 50) {
      all_keys <- character()
      all_sizes <- numeric()
      continuation_token <- NULL
      page <- 1

      repeat {
        url <- paste0("https://", bucket, ".s3.amazonaws.com/?list-type=2&prefix=", utils::URLencode(prefix, reserved = TRUE))

        if (!is.null(continuation_token)) {
          url <- paste0(url, "&continuation-token=", utils::URLencode(continuation_token, reserved = TRUE))
        }

        res <- httr::GET(url)

        if (httr::status_code(res) != 200) {
          stop("Error consultando S3. HTTP status: ", httr::status_code(res))
        }

        txt <- httr::content(res, as = "text", encoding = "UTF-8")
        doc <- xml2::read_xml(txt)
        xml2::xml_ns_strip(doc)

        keys <- xml2::xml_text(xml2::xml_find_all(doc, ".//Contents/Key"))
        sizes <- xml2::xml_text(xml2::xml_find_all(doc, ".//Contents/Size"))

        if (length(keys) > 0) {
          all_keys <- c(all_keys, keys)
          all_sizes <- c(all_sizes, suppressWarnings(as.numeric(sizes)))
        }

        is_truncated <- xml2::xml_text(xml2::xml_find_first(doc, ".//IsTruncated"))

        if (!identical(tolower(is_truncated), "true")) break

        continuation_token <- xml2::xml_text(xml2::xml_find_first(doc, ".//NextContinuationToken"))

        if (is.na(continuation_token) || !nzchar(continuation_token)) break

        page <- page + 1
        if (page > max_pages) {
          warning("Se alcanz\u00f3 max_pages al consultar S3 para prefix: ", prefix)
          break
        }
      }

      if (length(all_keys) == 0) {
        return(data.frame(key = character(), file = character(), size_online = numeric(), url = character(), stringsAsFactors = FALSE))
      }

      data.frame(
        key = all_keys,
        file = basename(all_keys),
        size_online = all_sizes,
        url = paste0("https://", bucket, ".s3.amazonaws.com/", all_keys),
        stringsAsFactors = FALSE
      )
    }

    expected_stamps_for_product <- function(product, year, julian_day, hour, minute, second) {
      year <- as.integer(year)
      julian_day <- as.integer(julian_day)
      base <- sprintf("%04d%03d", year, julian_day)

      norm <- normalize_product_time(product, hour, minute, second)
      hour <- norm$hour
      minute <- norm$minute
      second <- norm$second

      out <- list()

      add_row <- function(hh, mm, ss) {
        expected_stamp <- paste0(base, hh, mm, ss)
        known_stamp <- gsub("X", "", expected_stamp)
        out[[length(out) + 1]] <<- data.frame(product = product, expected_stamp = expected_stamp, known_stamp = known_stamp, stringsAsFactors = FALSE)
      }

      if (identical(hour, "FIRST")) {
        add_row("XX", "XX", "XX")
        return(dplyr::bind_rows(out))
      }

      if (identical(product, "LSTF")) {
        hours <- if (identical(hour, "ALL")) sprintf("%02d", 0:23) else hour
        for (hh in hours) add_row(hh, "XX", "XX")
        return(dplyr::bind_rows(out))
      }

      if (product %in% c("MCMIPF", "FDCF")) {
        hours <- if (identical(hour, "ALL")) sprintf("%02d", 0:23) else hour
        for (hh in hours) {
          if (identical(minute, "FIRST")) {
            add_row(hh, "XX", "XX")
          } else {
            minutes <- if (identical(minute, "ALL")) sprintf("%02d", seq(0, 50, 10)) else minute
            for (mm in minutes) add_row(hh, mm, "XX")
          }
        }
        return(dplyr::bind_rows(out))
      }

      if (identical(product, "GLM")) {
        hours <- if (identical(hour, "ALL")) sprintf("%02d", 0:23) else hour
        for (hh in hours) {
          if (identical(minute, "FIRST")) {
            add_row(hh, "XX", "XX")
          } else {
            minutes <- if (identical(minute, "ALL")) sprintf("%02d", 0:59) else minute
            for (mm in minutes) {
              if (identical(second, "FIRST")) {
                add_row(hh, mm, "XX")
              } else {
                seconds <- if (identical(second, "ALL")) c("00", "20", "40") else second
                for (ss in seconds) add_row(hh, mm, ss)
              }
            }
          }
        }
        return(dplyr::bind_rows(out))
      }

      dplyr::bind_rows(out)
    }

    make_expected_inventory <- function(sat_date_config, products_time_config) {
      if (is.null(sat_date_config)) stop("No hay Configuraci\u00f3n de Sat\u00e9lite/fecha.")
      if (is.null(products_time_config)) stop("No hay Configuraci\u00f3n de productos/tiempo.")

      g_code <- goes_satellite_code(sat_date_config$satellite)

      rows <- lapply(products_time_config$product_times, function(x) {
        stamps <- expected_stamps_for_product(
          product = x$product,
          year = sat_date_config$year,
          julian_day = sat_date_config$julian_day,
          hour = x$hour,
          minute = x$minute,
          second = x$second
        )

        stamps$product_code <- x$product_code
        stamps$satellite <- sat_date_config$satellite
        stamps$goes_code <- g_code
        stamps$bucket <- sat_date_config$bucket
        stamps$year <- sat_date_config$year
        stamps$julian_day <- sprintf("%03d", sat_date_config$julian_day)
        stamps$gregorian_date <- sat_date_config$gregorian_date
        stamps$day_prefix <- make_day_prefix(x$product_code, sat_date_config$year, sat_date_config$julian_day)
        stamps$file_minimum_pattern <- paste0("OR_", x$product_code, "..._", g_code, "_s", stamps$known_stamp)
        stamps
      })

      dplyr::bind_rows(rows)
    }

    list_local_nc_files <- function(download_dir) {
      download_dir <- normalizePath(download_dir, winslash = "/", mustWork = FALSE)

      if (!dir.exists(download_dir)) {
        return(data.frame(path = character(), file = character(), size_local = numeric(), stringsAsFactors = FALSE))
      }

      paths <- list.files(download_dir, pattern = "\\.nc$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)

      if (length(paths) == 0) {
        return(data.frame(path = character(), file = character(), size_local = numeric(), stringsAsFactors = FALSE))
      }

      data.frame(
        path = normalizePath(paths, winslash = "/", mustWork = FALSE),
        file = basename(paths),
        size_local = file.info(paths)$size,
        stringsAsFactors = FALSE
      )
    }

    make_goes_filename_regex <- function(product_code, goes_code, known_stamp) {
      paste0("^OR_", stringr::str_replace_all(product_code, "-", "\\\\-"), ".*_", goes_code, "_s", known_stamp)
    }

    verify_expected_inventory <- function(expected, local_files, online_files) {
      if (nrow(expected) == 0) return(data.frame())

      rows <- lapply(seq_len(nrow(expected)), function(i) {
        x <- expected[i, ]

        rx <- make_goes_filename_regex(x$product_code, x$goes_code, x$known_stamp)
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

        size_match <- !is.na(local_size) && !is.na(online_size) && isTRUE(local_size == online_size)

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
          local_size_mb = ifelse(is.na(local_size), NA, round(local_size / 1024^2, 3)),
          online_found = online_found,
          online_file = online_file,
          online_size = online_size,
          online_size_mb = ifelse(is.na(online_size), NA, round(online_size / 1024^2, 3)),
          size_match = ifelse(local_found && online_found, size_match, NA),
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

    make_download_plan <- function(verification_table, download_dir) {
      if (is.null(verification_table) || nrow(verification_table) == 0) return(data.frame())

      plan <- verification_table |>
        dplyr::filter(action %in% c("Download", "Delete and Download")) |>
        dplyr::filter(!is.na(online_url), nzchar(online_url))

      if (nrow(plan) == 0) return(data.frame())

      plan$bucket <- sub(
        pattern = "^https://([^/]+)\\.s3\\.amazonaws\\.com/.*$",
        replacement = "\\1",
        x = plan$online_url
      )

      plan$destination <- file.path(
        normalizePath(download_dir, winslash = "/", mustWork = FALSE),
        plan$bucket,
        plan$online_key
      )

      plan
    }

    make_download_job_dir <- function() {
      file.path(
        tempdir(),
        paste0("legiongoes_download_job_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sample.int(999999, 1))
      )
    }

    get_rscript_path <- function() {
      exe <- if (.Platform$OS.type == "windows") {
        file.path(R.home("bin"), "Rscript.exe")
      } else {
        file.path(R.home("bin"), "Rscript")
      }

      normalizePath(exe, winslash = "/", mustWork = TRUE)
    }

    write_download_worker_script <- function(worker_file) {
      worker_code <- c(
        "args <- commandArgs(trailingOnly = TRUE)",
        "plan_file <- args[[1]]",
        "status_file <- args[[2]]",
        "log_file <- args[[3]]",
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
        "  line <- sprintf('[%s] %s', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), txt)",
        "  cat(line, '\\n', file = log_file, append = TRUE)",
        "}",
        "save_status <- function(status) saveRDS(status, status_file)",
        "safe_download_from_verified_url <- function(url, destfile) {",
        "  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)",
        "  tmp <- paste0(destfile, '.partial')",
        "  if (file.exists(tmp)) unlink(tmp)",
        "  ok <- tryCatch(expr = {",
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
        "  if (file.exists(destfile)) unlink(destfile)",
        "  file.rename(tmp, destfile) && file.exists(destfile)",
        "}",
        "plan <- readRDS(plan_file)",
        "status <- data.frame(producto = plan$product, archivo = plan$online_file, accion_original = plan$action, destino = plan$destination, estado = 'pendiente', detalle = '', stringsAsFactors = FALSE)",
        "meta <- list(running = TRUE, done = FALSE, started_at = as.character(Sys.time()), finished_at = NA_character_, current = 0L, total = nrow(plan), n_ok = 0L, n_error = 0L, status = status)",
        "save_status(meta)",
        "append_log('Inicio de descarga usando lista congelada.')",
        "append_log('No se har\u00e1 nueva consulta S3. Se usar\u00e1n solo las URLs verificadas.')",
        "for (i in seq_len(nrow(plan))) {",
        "  file_name <- plan$online_file[i]",
        "  destfile <- plan$destination[i]",
        "  url <- plan$online_url[i]",
        "  action <- plan$action[i]",
        "  meta$current <- i",
        "  meta$status$estado[i] <- 'procesando'",
        "  meta$status$detalle[i] <- 'Descargando...'",
        "  save_status(meta)",
        "  append_log('--------------------------------------------------')",
        "  append_log('Archivo [', i, '/', nrow(plan), ']: ', file_name)",
        "  append_log('Acci\u00f3n congelada: ', action)",
        "  append_log('Destino: ', destfile)",
        "  if (identical(action, 'Delete and Download') && file.exists(destfile)) {",
        "    append_log('Borrando archivo local previo por diferencia de tama\u00f1o...')",
        "    unlink(destfile)",
        "  }",
        "  ok <- safe_download_from_verified_url(url = url, destfile = destfile)",
        "  if (ok) {",
        "    local_size <- file.info(destfile)$size",
        "    expected_size <- plan$online_size[i]",
        "    if (!is.na(expected_size) && !is.na(local_size) && local_size != expected_size) {",
        "      meta$status$estado[i] <- 'error'",
        "      meta$status$detalle[i] <- paste0('Descargado, pero tama\u00f1o diferente. Local=', local_size, ' Online=', expected_size)",
        "      append_log('ERROR: tama\u00f1o descargado diferente al tama\u00f1o congelado online.')",
        "    } else {",
        "      meta$status$estado[i] <- 'ok'",
        "      meta$status$detalle[i] <- paste0('Descargado. Peso: ', format_bytes(local_size))",
        "      append_log('OK. Peso descargado: ', format_bytes(local_size))",
        "    }",
        "  } else {",
        "    meta$status$estado[i] <- 'error'",
        "    meta$status$detalle[i] <- 'No se pudo descargar desde la URL congelada.'",
        "    append_log('ERROR: no se pudo descargar desde la URL congelada.')",
        "  }",
        "  meta$n_ok <- sum(meta$status$estado == 'ok', na.rm = TRUE)",
        "  meta$n_error <- sum(meta$status$estado == 'error', na.rm = TRUE)",
        "  save_status(meta)",
        "}",
        "meta$running <- FALSE",
        "meta$done <- TRUE",
        "meta$finished_at <- as.character(Sys.time())",
        "meta$n_ok <- sum(meta$status$estado == 'ok', na.rm = TRUE)",
        "meta$n_error <- sum(meta$status$estado == 'error', na.rm = TRUE)",
        "save_status(meta)",
        "append_log('--------------------------------------------------')",
        "append_log('Descarga finalizada.')",
        "append_log('OK: ', meta$n_ok)",
        "append_log('Errores: ', meta$n_error)"
      )

      writeLines(worker_code, worker_file, useBytes = TRUE)
      invisible(worker_file)
    }

    start_download_worker <- function(plan) {
      job_dir <- make_download_job_dir()
      dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)

      plan_file <- file.path(job_dir, "plan.rds")
      status_file <- file.path(job_dir, "status.rds")
      log_file <- file.path(job_dir, "download.log")
      worker_file <- file.path(job_dir, "download_worker.R")

      saveRDS(plan, plan_file)
      writeLines(character(), log_file)
      write_download_worker_script(worker_file)

      system2(
        command = get_rscript_path(),
        args = c(shQuote(worker_file), shQuote(plan_file), shQuote(status_file), shQuote(log_file)),
        wait = FALSE,
        stdout = FALSE,
        stderr = FALSE
      )

      list(
        job_dir = job_dir,
        plan_file = plan_file,
        status_file = status_file,
        log_file = log_file,
        worker_file = worker_file,
        started_at = Sys.time()
      )
    }

    read_download_job_status <- function(job) {
      if (is.null(job) || is.null(job$status_file) || !file.exists(job$status_file)) return(NULL)

      out <- tryCatch(
        expr = readRDS(job$status_file),
        error = function(e) NULL
      )

      out
    }

    read_download_job_log <- function(job) {
      if (is.null(job) || is.null(job$log_file) || !file.exists(job$log_file)) return(character())

      out <- tryCatch(
        expr = readLines(job$log_file, warn = FALSE, encoding = "UTF-8"),
        error = function(e) character()
      )

      out
    }

    # ---------------------------------------------------------------------------
    # Reactive values
    # ---------------------------------------------------------------------------

    rv <- shiny::reactiveValues(
      sat_date_locked = FALSE,
      products_time_locked = FALSE,
      sat_date_config = NULL,
      products_time_config = NULL,
      expected_inventory = NULL,
      verification_table = NULL,
      verification_summary = "Todav\u00eda no se realiz\u00f3 la verificaci\u00f3n.",
      download_plan_locked = FALSE,
      download_plan = NULL,
      download_plan_summary = "Todav\u00eda no se congel\u00f3 ninguna lista de descarga.",
      download_log = character(),
      download_status = NULL,
      download_job = NULL,
      download_job_running = FALSE
    )

    add_download_log <- function(...) {
      txt <- paste0(...)
      line <- sprintf("[%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), txt)
      rv$download_log <- c(rv$download_log, line)
    }

    set_button_lock_state <- function(button_id, locked) {
      session$sendCustomMessage(
        type = session$ns("set_lock_switch"),
        message = list(id = session$ns(button_id), locked = isTRUE(locked))
      )
    }

    # ---------------------------------------------------------------------------
    # Outputs estimaci\u00f3n por producto
    # ---------------------------------------------------------------------------

    for (product in PRODUCT_CHOICES) {
      local({
        p <- product
        ids <- time_input_ids(p)
        use_id <- product_use_id(p)
        output_id <- paste0("estimated_files_", p)

        output[[output_id]] <- shiny::renderUI({
          use_product <- isTRUE(input[[use_id]])

          if (!use_product) {
            return(shiny::div(class = "summary-box", shiny::tags$strong("No seleccionado")))
          }

          hour <- input[[ids$hour]]
          minute <- input[[ids$minute]]
          second <- input[[ids$second]]

          if (is.null(hour) || is.null(minute) || is.null(second)) {
            return(NULL)
          }

          n_files <- estimate_files_for_product_time(p, hour, minute, second)
          normalized <- normalize_product_time(p, hour, minute, second)

          shiny::div(
            class = "summary-box",
            shiny::tags$strong(paste0(n_files, " archivo", ifelse(n_files == 1, "", "s"))),
            shiny::tags$span(paste0(" | ", normalized$hour, ":", normalized$minute, ":", normalized$second, " UTC"))
          )
        })
      })
    }

    # ---------------------------------------------------------------------------
    # Outputs pesta\u00f1a 1
    # ---------------------------------------------------------------------------

    output$sat_date_control_text <- shiny::renderUI({
      if (isTRUE(rv$sat_date_locked)) {
        shiny::HTML("<b>\U0001F504 Sat\u00e9lite/fecha cerrados.</b> Mueva el switch para editar. Presione \U0001F504 para resetear.")
      } else {
        shiny::HTML("<b>\U0001F512 Sat\u00e9lite/fecha editables.</b> Mueva el switch para cerrar esta Configuraci\u00f3n.")
      }
    })

    output$sat_date_state_box <- shiny::renderUI({
      if (isTRUE(rv$sat_date_locked)) {
        shiny::div(class = "locked-box", "Sat\u00e9lite/fecha cerrados.\nLa app usar\u00e1 esta Configuraci\u00f3n para las siguientes pesta\u00f1as.")
      } else {
        shiny::div(class = "warning-box", "Sat\u00e9lite/fecha editables.\nCierre esta secci\u00f3n antes de configurar productos.")
      }
    })

    output$sat_date_config_box <- shiny::renderUI({
      if (is.null(rv$sat_date_config)) {
        return(shiny::div(class = "warning-box", "No hay Sat\u00e9lite/fecha cerrados."))
      }

      x <- rv$sat_date_config

      shiny::div(
        class = "locked-box",
        paste(
          paste0("Posici\u00f3n:       ", x$position),
          paste0("Sat\u00e9lite:       ", x$satellite),
          paste0("Bucket:         ", x$bucket),
          paste0("Regla:          ", x$satellite_rule),
          paste0("A\u00f1o:            ", x$year),
          paste0("Fecha:          ", x$gregorian_date),
          paste0("D\u00eda juliano:    ", sprintf("%03d", x$julian_day)),
          sep = "\n"
        )
      )
    })

    # ---------------------------------------------------------------------------
    # Outputs pesta\u00f1a 2
    # ---------------------------------------------------------------------------

    output$products_time_control_text <- shiny::renderUI({
      if (!isTRUE(rv$sat_date_locked)) {
        shiny::HTML("<b>Bloqueado.</b> Primero cierre Sat\u00e9lite/fecha en la pesta\u00f1a 1.")
      } else if (isTRUE(rv$products_time_locked)) {
        shiny::HTML("<b>\U0001F512 Productos/tiempo cerrados.</b> Mueva el switch para editar. Presione \U0001F504 para resetear.")
      } else {
        shiny::HTML("<b>\U0001F513 Productos/tiempo editables.</b> Mueva el switch para cerrar esta Configuraci\u00f3n.")
      }
    })

    output$product_tab_warning <- shiny::renderUI({
      if (!isTRUE(rv$sat_date_locked) || is.null(rv$sat_date_config)) {
        return(shiny::div(class = "warning-box", "Primero cierre Sat\u00e9lite/fecha en la pesta\u00f1a 1."))
      }

      x <- rv$sat_date_config

      shiny::div(
        class = "locked-box sat-date-inline-box",
        paste0(
          "GOES: ", x$satellite,
          " | Posici\u00f3n: ", x$position,
          " | Fecha gregoriana: ", x$gregorian_date,
          " | D\u00eda juliano: ", sprintf("%03d", x$julian_day)
        )
      )
    })

    output$products_time_state_box <- shiny::renderUI({
      if (!isTRUE(rv$sat_date_locked)) {
        return(shiny::div(class = "warning-box", "Bloqueado hasta cerrar Sat\u00e9lite/fecha."))
      }

      if (isTRUE(rv$products_time_locked)) {
        shiny::div(class = "locked-box", "Productos/tiempo cerrados.")
      } else {
        shiny::div(class = "warning-box", "Productos/tiempo editables.")
      }
    })

    output$products_time_config_box <- shiny::renderUI({
      if (is.null(rv$products_time_config)) {
        return(shiny::div(class = "warning-box", "No hay productos/tiempo cerrados."))
      }

      product_time_text <- paste(
        vapply(
          rv$products_time_config$product_times,
          function(x) {
            n_files <- estimate_files_for_product_time(x$product, x$hour, x$minute, x$second)
            sprintf(
              "%-7s | hora: %-5s | minuto: %-5s | segundo: %-5s | archivos: %s",
              x$product,
              x$hour,
              x$minute,
              x$second,
              n_files
            )
          },
          character(1)
        ),
        collapse = "\n"
      )

      total_files <- sum(
        vapply(
          rv$products_time_config$product_times,
          function(x) estimate_files_for_product_time(x$product, x$hour, x$minute, x$second),
          numeric(1)
        ),
        na.rm = TRUE
      )

      shiny::div(
        class = "locked-box products-config-box",
        paste(
          sprintf("%-14s %s", "Productos:", paste(rv$products_time_config$products, collapse = ", ")),
          sprintf("%-14s %s", "Total:", total_files),
          "",
          "Tiempo por producto:",
          product_time_text,
          sep = "\n"
        )
      )
    })

    # ---------------------------------------------------------------------------
    # Outputs pesta\u00f1a 3
    # ---------------------------------------------------------------------------

    output$text_path <- shiny::renderText({
      the_folder_path <- get_default_download_dir()

      paste0("La app revisar\u00e1 autom\u00e1ticamente:\n", the_folder_path)
    })

    output$expected_inventory_table <- shiny::renderTable({
      x <- rv$expected_inventory

      if (is.null(x) || nrow(x) == 0) {
        return(data.frame(Mensaje = "Todav\u00eda no se gener\u00f3 el inventario esperado."))
      }

      x_example <- x |>
        dplyr::group_by(product) |>
        dplyr::slice(1) |>
        dplyr::ungroup()

      data.frame(
        producto = x_example$product,
        fecha = x_example$gregorian_date,
        juliano = x_example$julian_day,
        goes = x_example$goes_code,
        expected_stamp = x_example$expected_stamp,
        known_stamp = x_example$known_stamp,
        patron_minimo = x_example$file_minimum_pattern,
        stringsAsFactors = FALSE
      )
    })

    output$verification_table <- DT::renderDT({
      x <- rv$verification_table

      if (is.null(x) || nrow(x) == 0) {
        return(
          DT::datatable(
            data.frame(Mensaje = "Todav\u00eda no se realiz\u00f3 la verificaci\u00f3n."),
            rownames = FALSE,
            options = list(dom = "t", pageLength = 5)
          )
        )
      }

      tab <- data.frame(
        producto = x$product,
        expected_stamp = x$expected_stamp,
        patron_minimo = x$file_minimum_pattern,
        local = ifelse(x$local_found, "YES", "NO"),
        archivo_local = x$local_file,
        peso_local_MB = x$local_size_mb,
        online = ifelse(x$online_found, "YES", "NO"),
        archivo_online = x$online_file,
        peso_online_MB = x$online_size_mb,
        pesos_coinciden = x$size_match,
        accion = x$action,
        stringsAsFactors = FALSE
      )

      DT::datatable(
        tab,
        rownames = FALSE,
        filter = "top",
        extensions = c("Buttons"),
        options = list(
          pageLength = 25,
          lengthMenu = c(10, 25, 50, 100, 500),
          scrollX = TRUE,
          autoWidth = TRUE,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel")
        )
      )
    })

    output$verification_summary <- shiny::renderUI({
      shiny::div(class = "locked-box products-config-box", rv$verification_summary)
    })

    output$download_plan_control_text <- shiny::renderUI({
      if (isTRUE(rv$download_plan_locked)) {
        shiny::HTML("<b>\U0001F512 Verificaci\u00f3n congelada.</b> La lista de descarga qued\u00f3 cerrada en esta pesta\u00f1a. Ahora puede ir a la pesta\u00f1a 4 para descargar.")
      } else {
        shiny::HTML("<b>\U0001F513 Verificaci\u00f3n editable.</b> Primero ejecute la verificaci\u00f3n local/online. Luego cierre este candado para congelar exactamente esa lista.")
      }
    })

    output$download_plan_state_box <- shiny::renderUI({
      if (isTRUE(rv$download_plan_locked)) {
        shiny::div(
          class = "locked-box",
          "Lista congelada.\nLa verificaci\u00f3n local/online ya no puede repetirse hasta resetear o abrir el candado.\nPuede pasar a la pesta\u00f1a 4 para descargar solo esta lista."
        )
      } else {
        shiny::div(
          class = "warning-box",
          "La lista todav\u00eda no est\u00e1 congelada.\nPrimero haga la verificaci\u00f3n local/online en esta pesta\u00f1a.\nLuego cierre el candado para habilitar la descarga segura en la pesta\u00f1a 4."
        )
      }
    })

    output$download_plan_summary <- shiny::renderUI({
      shiny::div(class = "locked-box products-config-box", rv$download_plan_summary)
    })

    output$download_plan_table <- DT::renderDT({
      x <- rv$download_plan

      if (is.null(x) || nrow(x) == 0) {
        return(DT::datatable(data.frame(Mensaje = "Todav\u00eda no hay lista congelada para descargar."), rownames = FALSE, options = list(dom = "t")))
      }

      tab <- data.frame(
        producto = x$product,
        expected_stamp = x$expected_stamp,
        accion = x$action,
        archivo_online = x$online_file,
        peso_online_MB = x$online_size_mb,
        destino = x$destination,
        url_verificada = x$online_url,
        stringsAsFactors = FALSE
      )

      DT::datatable(
        tab,
        rownames = FALSE,
        filter = "top",
        extensions = c("Buttons"),
        options = list(
          pageLength = 25,
          lengthMenu = c(10, 25, 50, 100, 500),
          scrollX = TRUE,
          autoWidth = TRUE,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel")
        )
      )
    })

    # ---------------------------------------------------------------------------
    # Outputs pesta\u00f1a 4
    # ---------------------------------------------------------------------------

    output$download_ready_box <- shiny::renderUI({
      if (!isTRUE(rv$download_plan_locked) || is.null(rv$download_plan)) {
        return(shiny::div(class = "warning-box", "Primero verifique y congele la lista en la pesta\u00f1a 3."))
      }

      n <- nrow(rv$download_plan)

      shiny::div(
        class = "locked-box products-config-box",
        paste(
          paste0("Lista congelada:  YES"),
          paste0("Archivos a bajar: ", n),
          paste0("Carpeta destino:  ", get_default_download_dir()),
          "",
          "La descarga no har\u00e1 una nueva consulta online.",
          "Solo usar\u00e1 las URLs verificadas y congeladas.",
          sep = "\n"
        )
      )
    })

    output$download_log <- shiny::renderText({
      if (length(rv$download_log) == 0) {
        return("Todav\u00eda no se inici\u00f3 ninguna descarga.")
      }

      paste(rv$download_log, collapse = "\n")
    })

    output$download_status_table <- DT::renderDT({
      x <- rv$download_status

      if (is.null(x) || nrow(x) == 0) {
        return(DT::datatable(data.frame(Mensaje = "Todav\u00eda no hay estado de descarga."), rownames = FALSE, options = list(dom = "t")))
      }

      DT::datatable(
        x,
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 25, lengthMenu = c(10, 25, 50, 100, 500), scrollX = TRUE, autoWidth = TRUE)
      )
    })

    # ---------------------------------------------------------------------------
    # Helpers de bloqueo
    # ---------------------------------------------------------------------------

    sat_date_input_ids <- c("position", "year", "date_format", "month", "day", "julian_day")
    product_check_ids <- vapply(PRODUCT_CHOICES, product_use_id, character(1))
    all_time_input_ids <- unlist(lapply(PRODUCT_CHOICES, function(product) unname(unlist(time_input_ids(product)))), use.names = FALSE)

    get_selected_products <- function() {
      PRODUCT_CHOICES[
        vapply(
          PRODUCT_CHOICES,
          function(product) isTRUE(shiny::isolate(input[[product_use_id(product)]])),
          logical(1)
        )
      ]
    }

    update_visible_product_boxes <- function() {
      for (product in PRODUCT_CHOICES) {
        use_product <- shiny::isolate(input[[product_use_id(product)]])
        box_id <- paste0("box_time_", product)

        if (isTRUE(use_product)) {
          shinyjs::show(box_id)
        } else {
          shinyjs::hide(box_id)
        }
      }
    }

    disable_sat_date_inputs <- function() {
      for (id_input in sat_date_input_ids) {
        shinyjs::disable(id_input)
      }
    }

    enable_sat_date_inputs <- function() {
      for (id_input in sat_date_input_ids) {
        shinyjs::enable(id_input)
      }
    }

    disable_products_time_inputs <- function() {
      for (id_input in c(product_check_ids, all_time_input_ids)) {
        shinyjs::disable(id_input)
      }
    }

    enable_products_time_inputs <- function() {
      for (id_input in c(product_check_ids, all_time_input_ids)) {
        shinyjs::enable(id_input)
      }
    }

    valid_or_default <- function(current, choices, default) {
      if (!is.null(current) && current %in% choices) {
        return(current)
      }
      default
    }

    apply_product_specific_locks <- function(products = PRODUCT_CHOICES) {
      if (is.null(products) || length(products) == 0) {
        return()
      }

      for (product in products) {
        ids <- time_input_ids(product)
        spec <- product_spec(product)
        minute_choices <- minute_choices_for_product(product)
        second_choices <- second_choices_for_product(product)

        if (identical(spec$minute_mode[[1]], "locked_first")) {
          shiny::updateSelectInput(session, ids$minute, choices = "FIRST", selected = "FIRST")
          shinyjs::disable(ids$minute)
        } else {
          shiny::updateSelectInput(
            session,
            ids$minute,
            choices = minute_choices,
            selected = valid_or_default(shiny::isolate(input[[ids$minute]]), minute_choices, default_minute_for_product(product))
          )
          shinyjs::enable(ids$minute)
        }

        if (identical(spec$second_mode[[1]], "locked_first")) {
          shiny::updateSelectInput(session, ids$second, choices = "FIRST", selected = "FIRST")
          shinyjs::disable(ids$second)
        } else {
          shiny::updateSelectInput(
            session,
            ids$second,
            choices = second_choices,
            selected = valid_or_default(shiny::isolate(input[[ids$second]]), second_choices, default_second_for_product(product))
          )
          shinyjs::enable(ids$second)
        }
      }
    }

    enforce_all_first_rule <- function(product) {
      ids <- time_input_ids(product)
      spec <- product_spec(product)
      hour_value <- input[[ids$hour]]
      minute_value <- input[[ids$minute]]

      if (is.null(hour_value)) return()

      if (identical(hour_value, "FIRST")) {
        shiny::updateSelectInput(session, ids$minute, selected = "FIRST")
        shiny::updateSelectInput(session, ids$second, selected = "FIRST")
        return()
      }

      if (identical(hour_value, "ALL")) {
        if (identical(spec$minute_mode[[1]], "locked_first")) {
          shiny::updateSelectInput(session, ids$minute, selected = "FIRST")
        } else {
          shiny::updateSelectInput(session, ids$minute, selected = "ALL")
        }

        if (identical(spec$second_mode[[1]], "locked_first")) {
          shiny::updateSelectInput(session, ids$second, selected = "FIRST")
        } else {
          shiny::updateSelectInput(session, ids$second, selected = "ALL")
        }

        return()
      }

      if (!is.null(minute_value) && identical(minute_value, "FIRST")) {
        shiny::updateSelectInput(session, ids$second, selected = "FIRST")
        return()
      }

      if (!is.null(minute_value) && identical(minute_value, "ALL")) {
        if (identical(spec$second_mode[[1]], "locked_first")) {
          shiny::updateSelectInput(session, ids$second, selected = "FIRST")
        } else {
          shiny::updateSelectInput(session, ids$second, selected = "ALL")
        }
        return()
      }
    }

    collect_product_times <- function(products) {
      out <- lapply(products, function(product) {
        ids <- time_input_ids(product)
        spec <- product_spec(product)

        hour <- input[[ids$hour]]
        minute <- input[[ids$minute]]
        second <- input[[ids$second]]

        if (is.null(hour) || !nzchar(hour)) stop("Falta seleccionar hora para ", product)
        if (is.null(minute) || !nzchar(minute)) stop("Falta seleccionar minuto para ", product)
        if (is.null(second) || !nzchar(second)) stop("Falta seleccionar segundo para ", product)

        normalized <- normalize_product_time(product, hour, minute, second)

        list(
          product = product,
          product_code = spec$product_code[[1]],
          frequency_label = spec$frequency_label[[1]],
          hour = normalized$hour,
          minute = normalized$minute,
          second = normalized$second,
          minute_mode = spec$minute_mode[[1]],
          second_mode = spec$second_mode[[1]]
        )
      })

      names(out) <- products
      out
    }

    reset_product_time_inputs <- function() {
      for (product in PRODUCT_CHOICES) {
        ids <- time_input_ids(product)
        shiny::updateSelectInput(session, ids$hour, choices = hour_choices(), selected = "00")
        shiny::updateSelectInput(session, ids$minute, choices = minute_choices_for_product(product), selected = default_minute_for_product(product))
        shiny::updateSelectInput(session, ids$second, choices = second_choices_for_product(product), selected = default_second_for_product(product))
      }
    }

    reset_product_checks <- function() {
      shiny::updateCheckboxInput(session, "use_LSTF", value = FALSE)
      shiny::updateCheckboxInput(session, "use_MCMIPF", value = FALSE)
      shiny::updateCheckboxInput(session, "use_FDCF", value = TRUE)
      shiny::updateCheckboxInput(session, "use_GLM", value = FALSE)
    }

    clear_download_plan <- function() {
      rv$download_plan_locked <- FALSE
      rv$download_plan <- NULL
      rv$download_plan_summary <- "Todav\u00eda no se congel\u00f3 ninguna lista de descarga."
      rv$download_log <- character()
      rv$download_status <- NULL
      rv$download_job <- NULL
      rv$download_job_running <- FALSE
      set_button_lock_state("toggle_download_plan", FALSE)
      shinyjs::enable("verify_local_online")
      shinyjs::enable("start_download")
    }

    clear_products_time_config <- function() {
      rv$products_time_locked <- FALSE
      rv$products_time_config <- NULL
      rv$expected_inventory <- NULL
      rv$verification_table <- NULL
      rv$verification_summary <- "Todav\u00eda no se realiz\u00f3 la verificaci\u00f3n."
      clear_download_plan()
      set_button_lock_state("toggle_products_time", FALSE)
    }

    # ---------------------------------------------------------------------------
    # Locks principales
    # ---------------------------------------------------------------------------

    lock_sat_date <- function() {
      date_info <- tryCatch(
        expr = validate_date_inputs(input$year, input$date_format, input$month, input$day, input$julian_day),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )

      if (is.null(date_info)) {
        set_button_lock_state("toggle_sat_date", FALSE)
        return(FALSE)
      }

      sat <- tryCatch(
        expr = resolve_goes_satellite(input$position, date_info$gregorian_date),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )

      if (is.null(sat)) {
        set_button_lock_state("toggle_sat_date", FALSE)
        return(FALSE)
      }

      rv$sat_date_config <- list(
        position = input$position,
        year = date_info$year,
        gregorian_date = as.character(date_info$gregorian_date),
        julian_day = date_info$julian_day,
        satellite = sat$satellite,
        bucket = sat$bucket,
        satellite_rule = sat$rule
      )

      rv$sat_date_locked <- TRUE
      clear_products_time_config()
      disable_sat_date_inputs()
      enable_products_time_inputs()
      apply_product_specific_locks(PRODUCT_CHOICES)

      session$onFlushed(function() update_visible_product_boxes(), once = TRUE)
      set_button_lock_state("toggle_sat_date", TRUE)
      shiny::showNotification("Sat\u00e9lite/fecha cerrados. Ya puede configurar productos.", type = "message")

      TRUE
    }

    unlock_sat_date <- function() {
      rv$sat_date_locked <- FALSE
      rv$sat_date_config <- NULL
      clear_products_time_config()
      enable_sat_date_inputs()
      disable_products_time_inputs()
      set_button_lock_state("toggle_sat_date", FALSE)
      shiny::showNotification("Sat\u00e9lite/fecha habilitados. Se borr\u00f3 la Configuraci\u00f3n de productos/tiempo.", type = "warning")
      TRUE
    }

    lock_products_time <- function() {
      if (!isTRUE(rv$sat_date_locked) || is.null(rv$sat_date_config)) {
        shiny::showNotification("Primero debe cerrar Sat\u00e9lite/fecha.", type = "warning")
        set_button_lock_state("toggle_products_time", FALSE)
        return(FALSE)
      }

      selected_products <- get_selected_products()

      if (length(selected_products) == 0) {
        shiny::showNotification("Seleccione al menos un producto.", type = "error")
        set_button_lock_state("toggle_products_time", FALSE)
        return(FALSE)
      }

      product_times <- tryCatch(
        expr = collect_product_times(selected_products),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )

      if (is.null(product_times)) {
        set_button_lock_state("toggle_products_time", FALSE)
        return(FALSE)
      }

      rv$products_time_config <- list(products = selected_products, product_times = product_times)
      rv$products_time_locked <- TRUE
      rv$expected_inventory <- NULL
      rv$verification_table <- NULL
      rv$verification_summary <- "Todav\u00eda no se realiz\u00f3 la verificaci\u00f3n."
      clear_download_plan()

      disable_products_time_inputs()
      set_button_lock_state("toggle_products_time", TRUE)
      shiny::showNotification("Productos/tiempo cerrados correctamente.", type = "message")

      TRUE
    }

    unlock_products_time <- function() {
      if (!isTRUE(rv$sat_date_locked)) {
        shiny::showNotification("Primero debe cerrar Sat\u00e9lite/fecha.", type = "warning")
        set_button_lock_state("toggle_products_time", FALSE)
        return(FALSE)
      }

      rv$products_time_locked <- FALSE
      rv$products_time_config <- NULL
      rv$expected_inventory <- NULL
      rv$verification_table <- NULL
      rv$verification_summary <- "Todav\u00eda no se realiz\u00f3 la verificaci\u00f3n."
      clear_download_plan()

      enable_products_time_inputs()
      apply_product_specific_locks(PRODUCT_CHOICES)
      session$onFlushed(function() update_visible_product_boxes(), once = TRUE)
      set_button_lock_state("toggle_products_time", FALSE)
      shiny::showNotification("Productos/tiempo habilitados.", type = "warning")

      TRUE
    }

    lock_download_plan <- function() {
      if (is.null(rv$verification_table) || nrow(rv$verification_table) == 0) {
        shiny::showNotification("Primero debe verificar local/online en esta pesta\u00f1a.", type = "warning")
        set_button_lock_state("toggle_download_plan", FALSE)
        return(FALSE)
      }

      download_dir <- get_default_download_dir()
      plan <- make_download_plan(rv$verification_table, download_dir)

      rv$download_plan <- plan
      rv$download_plan_locked <- TRUE
      rv$download_log <- character()
      rv$download_status <- NULL
      rv$download_job <- NULL
      rv$download_job_running <- FALSE

      n_download <- sum(plan$action == "Download", na.rm = TRUE)
      n_delete_download <- sum(plan$action == "Delete and Download", na.rm = TRUE)
      n_total <- nrow(plan)
      size_total <- sum(plan$online_size, na.rm = TRUE)

      rv$download_plan_summary <- paste(
        paste0("Carpeta destino:       ", download_dir),
        paste0("Archivos congelados:   ", n_total),
        paste0("Download:              ", n_download),
        paste0("Delete and Download:   ", n_delete_download),
        paste0("Tama\u00f1o online total:   ", format_bytes(size_total)),
        "",
        "IMPORTANTE:",
        "Esta lista queda congelada a partir de la verificaci\u00f3n local/online ya hecha en esta pesta\u00f1a.",
        "La pesta\u00f1a 4 descargar\u00e1 \u00fanicamente esta lista, sin nueva consulta S3 ni archivos nuevos.",
        sep = "\n"
      )

      set_button_lock_state("toggle_download_plan", TRUE)
      shinyjs::disable("verify_local_online")
      shiny::showNotification("Lista de descarga congelada.", type = "message")

      TRUE
    }

    unlock_download_plan <- function() {
      clear_download_plan()
      shiny::showNotification("Lista de descarga descongelada/resetada.", type = "warning")
      TRUE
    }

    # ---------------------------------------------------------------------------
    # Inicializaci\u00f3n visual
    # ---------------------------------------------------------------------------

    session$onFlushed(function() {
      set_button_lock_state("toggle_sat_date", FALSE)
      set_button_lock_state("toggle_products_time", FALSE)
      set_button_lock_state("toggle_download_plan", FALSE)

      apply_product_specific_locks(PRODUCT_CHOICES)

      shinyjs::hide("box_time_LSTF")
      shinyjs::hide("box_time_MCMIPF")
      shinyjs::show("box_time_FDCF")
      shinyjs::hide("box_time_GLM")
      disable_products_time_inputs()
    }, once = TRUE)

    # ---------------------------------------------------------------------------
    # Eventos pesta\u00f1a 1
    # ---------------------------------------------------------------------------

    shiny::observeEvent(input$toggle_sat_date_clicked, {
      if (isTRUE(rv$sat_date_locked)) {
        unlock_sat_date()
      } else {
        lock_sat_date()
      }
    })

    shiny::observeEvent(input$reset_sat_date, {
      rv$sat_date_locked <- FALSE
      rv$sat_date_config <- NULL
      clear_products_time_config()
      enable_sat_date_inputs()
      disable_products_time_inputs()
      set_button_lock_state("toggle_sat_date", FALSE)

      shiny::updateSelectInput(session, "position", selected = "EAST")
      shiny::updateNumericInput(session, "year", value = as.integer(format(Sys.Date(), "%Y")))
      shiny::updateRadioButtons(session, "date_format", selected = "gregorian")
      shiny::updateNumericInput(session, "month", value = as.integer(format(Sys.Date(), "%m")))
      shiny::updateNumericInput(session, "day", value = as.integer(format(Sys.Date(), "%d")))
      shiny::updateNumericInput(session, "julian_day", value = as.integer(format(Sys.Date(), "%j")))

      shiny::showNotification("Sat\u00e9lite/fecha reseteados.", type = "message")
    })

    # ---------------------------------------------------------------------------
    # Eventos pesta\u00f1a 2
    # ---------------------------------------------------------------------------

    shiny::observeEvent(input$toggle_products_time_clicked, {
      if (!isTRUE(rv$sat_date_locked)) {
        shiny::showNotification("Primero debe cerrar Sat\u00e9lite/fecha.", type = "warning")
        set_button_lock_state("toggle_products_time", FALSE)
        return()
      }

      if (isTRUE(rv$products_time_locked)) {
        unlock_products_time()
      } else {
        lock_products_time()
      }
    })

    shiny::observeEvent(input$reset_products_time, {
      if (!isTRUE(rv$sat_date_locked)) {
        shiny::showNotification("Primero debe cerrar Sat\u00e9lite/fecha.", type = "warning")
        set_button_lock_state("toggle_products_time", FALSE)
        return()
      }

      rv$products_time_locked <- FALSE
      rv$products_time_config <- NULL
      rv$expected_inventory <- NULL
      rv$verification_table <- NULL
      rv$verification_summary <- "Todav\u00eda no se realiz\u00f3 la verificaci\u00f3n."
      clear_download_plan()

      enable_products_time_inputs()
      set_button_lock_state("toggle_products_time", FALSE)
      reset_product_checks()
      reset_product_time_inputs()

      session$onFlushed(function() {
        apply_product_specific_locks(PRODUCT_CHOICES)
        update_visible_product_boxes()
      }, once = TRUE)

      shiny::showNotification("Productos/tiempo reseteados.", type = "message")
    })

    for (product in PRODUCT_CHOICES) {
      local({
        p <- product
        use_id <- product_use_id(p)

        shiny::observeEvent(input[[use_id]], {
          if (!isTRUE(rv$sat_date_locked)) return()
          if (isTRUE(rv$products_time_locked)) return()
          update_visible_product_boxes()
        }, ignoreInit = TRUE)
      })
    }

    for (product in PRODUCT_CHOICES) {
      local({
        p <- product
        ids <- time_input_ids(p)

        shiny::observeEvent(input[[ids$hour]], {
          if (!isTRUE(rv$sat_date_locked)) return()
          if (isTRUE(rv$products_time_locked)) return()
          enforce_all_first_rule(p)
        }, ignoreInit = TRUE)

        shiny::observeEvent(input[[ids$minute]], {
          if (!isTRUE(rv$sat_date_locked)) return()
          if (isTRUE(rv$products_time_locked)) return()
          enforce_all_first_rule(p)
        }, ignoreInit = TRUE)
      })
    }

    # ---------------------------------------------------------------------------
    # Eventos pesta\u00f1a 3
    # ---------------------------------------------------------------------------

    shiny::observeEvent(input$verify_local_online, {
      if (isTRUE(rv$download_plan_locked)) {
        shiny::showNotification("La verificaci\u00f3n est\u00e1 congelada. Abra el candado o presione reset para volver a verificar.", type = "warning")
        return()
      }

      if (is.null(rv$sat_date_config)) {
        shiny::showNotification("Primero debe cerrar Sat\u00e9lite/fecha.", type = "warning")
        return()
      }

      if (is.null(rv$products_time_config)) {
        shiny::showNotification("Primero debe cerrar productos/tiempo.", type = "warning")
        return()
      }

      download_dir <- get_default_download_dir()

      expected <- tryCatch(
        expr = make_expected_inventory(rv$sat_date_config, rv$products_time_config),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )

      if (is.null(expected) || nrow(expected) == 0) {
        shiny::showNotification("No se pudo generar el inventario esperado.", type = "error")
        return()
      }

      rv$expected_inventory <- expected
      local_files <- list_local_nc_files(download_dir)

      product_prefixes <- expected |>
        dplyr::select(product, product_code, bucket, day_prefix) |>
        dplyr::distinct()

      online_list <- list()

      shiny::withProgress(message = "Verificando local y online", value = 0, {
        shiny::incProgress(0.15, detail = "Buscando archivos locales...")

        for (i in seq_len(nrow(product_prefixes))) {
          shiny::incProgress(
            amount = 0.75 / max(1, nrow(product_prefixes)),
            detail = paste0("Consultando online: ", product_prefixes$product[i])
          )

          online_list[[i]] <- tryCatch(
            expr = list_s3_prefix_paginated(product_prefixes$bucket[i], product_prefixes$day_prefix[i]),
            error = function(e) {
              shiny::showNotification(
                paste("Error online", product_prefixes$product[i], ":", conditionMessage(e)),
                type = "error",
                duration = 8
              )

              data.frame(key = character(), file = character(), size_online = numeric(), url = character(), stringsAsFactors = FALSE)
            }
          )
        }

        shiny::incProgress(0.10, detail = "Comparando local vs online...")
      })

      online_files <- dplyr::bind_rows(online_list)
      verification <- verify_expected_inventory(expected, local_files, online_files)

      rv$verification_table <- verification
      clear_download_plan()

      n_total <- nrow(verification)
      n_ok <- sum(verification$action == "OK", na.rm = TRUE)
      n_download <- sum(verification$action == "Download", na.rm = TRUE)
      n_delete_download <- sum(verification$action == "Delete and Download", na.rm = TRUE)
      n_no_online <- sum(verification$action == "No online", na.rm = TRUE)
      n_local_only <- sum(verification$action == "Local only", na.rm = TRUE)

      rv$verification_summary <- paste(
        paste0("Carpeta local:        ", download_dir),
        paste0("Archivos esperados:   ", n_total),
        paste0("OK:                   ", n_ok),
        paste0("Download:             ", n_download),
        paste0("Delete and Download:  ", n_delete_download),
        paste0("No online:            ", n_no_online),
        paste0("Local only:           ", n_local_only),
        sep = "\n"
      )

      shiny::showNotification("Verificaci\u00f3n local / online finalizada.", type = "message")
    })

    shiny::observeEvent(input$toggle_download_plan_clicked, {
      if (isTRUE(rv$download_plan_locked)) {
        unlock_download_plan()
      } else {
        lock_download_plan()
      }
    })

    shiny::observeEvent(input$reset_download_plan, {
      clear_download_plan()
      shiny::showNotification("Lista de descarga reseteada.", type = "message")
    })

    # ---------------------------------------------------------------------------
    # Polling descarga segundo proceso
    # ---------------------------------------------------------------------------

    shiny::observe({
      shiny::invalidateLater(1000, session)

      if (!isTRUE(rv$download_job_running)) {
        return()
      }

      job <- rv$download_job
      status <- read_download_job_status(job)
      log_lines <- read_download_job_log(job)

      if (length(log_lines) > 0) {
        rv$download_log <- log_lines
      }

      if (!is.null(status)) {
        rv$download_status <- status$status

        if (isFALSE(status$running) || isTRUE(status$done)) {
          rv$download_job_running <- FALSE
          shinyjs::enable("start_download")

          notification_type <- ifelse(status$n_error == 0, "message", "warning")

          shiny::showNotification(
            paste0("Descarga finalizada. OK: ", status$n_ok, " | Errores: ", status$n_error),
            type = notification_type
          )
        }
      }
    })

    # ---------------------------------------------------------------------------
    # Eventos pesta\u00f1a 4
    # ---------------------------------------------------------------------------

    shiny::observeEvent(input$start_download, {
      if (isTRUE(rv$download_job_running)) {
        shiny::showNotification("Ya hay una descarga en proceso.", type = "warning")
        return()
      }

      if (!isTRUE(rv$download_plan_locked) || is.null(rv$download_plan)) {
        shiny::showNotification("Primero debe verificar y congelar la lista de descarga en la pesta\u00f1a 3.", type = "warning")
        return()
      }

      plan <- rv$download_plan

      if (nrow(plan) == 0) {
        shiny::showNotification("No hay archivos para descargar. La lista congelada est\u00e1 vac\u00eda.", type = "message")
        add_download_log("Lista congelada vac\u00eda. No hay nada para descargar.")
        return()
      }

      rv$download_status <- data.frame(
        producto = plan$product,
        archivo = plan$online_file,
        accion_original = plan$action,
        destino = plan$destination,
        estado = "pendiente",
        detalle = "Esperando inicio del proceso secundario...",
        stringsAsFactors = FALSE
      )

      rv$download_log <- c(
        sprintf("[%s] Preparando descarga en segundo proceso...", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        sprintf("[%s] La app seguir\u00e1 respondiendo mientras se descarga.", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      )

      job <- tryCatch(
        expr = start_download_worker(plan),
        error = function(e) {
          shiny::showNotification(
            paste("No se pudo iniciar el proceso de descarga:", conditionMessage(e)),
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
      shinyjs::disable("start_download")
      shiny::showNotification("Descarga iniciada en segundo proceso. Puede seguir usando la app.", type = "message")
    })
  })
}


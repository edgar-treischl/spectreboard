#' Validation user interface
#'
#' @param id ID for the module
#' @export
#

validationReportUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_column_wrap(
    width = "100%",
    fill = FALSE,
    bslib::card(
      class = "shadow-sm",
      bslib::card_header(
        class = "bg-light",
        shiny::div(
          class = "d-flex justify-content-between align-items-center",
          shiny::h4("Validation Report", class = "m-0"),
          shiny::div(
            class = "text-end mb-4",
            shiny::downloadButton(ns("download_report"), "Download Report",
                           class = "btn-sm btn-outline-primary")
          )
        )
      ),
      bslib::card_body(
        shiny::uiOutput(ns("validation_loading")),
        # Validation report output
        shiny::div(
          id = ns("validation_container"),
          style = "overflow: auto; max-height: 800px; width: 100%;",
          shiny::htmlOutput(ns("validation_report"))
        ),
        shiny::hr(),
        shiny::p("This report provides detailed validation metrics for the selected dataset.",
                 class = "mb-4")
      )
    )
  )
}


#' Validation report server logic
#'
#' @param id ID for the module
#' @param dataset Reactive expression for the selected dataset
#' @param version Reactive expression for the selected version
#' @export
#'
validationReportServer <- function(id, dataset, version) {
  shiny::moduleServer(id, function(input, output, session) {

    # Store error/success state
    report_state <- shiny::reactiveVal(list(valid = TRUE, error = NULL))

    # Load report content based on dataset + version
    validation_report <- shiny::reactive({
      ds <- dataset()
      ver <- version()

      if (is.null(ds) || ds == "" || is.null(ver) || ver == "") {
        return(list(success = FALSE, error = "No dataset or version selected."))
      }

      tryCatch({
        # Pull pointer from duckdb
        pointer <- duckdb_table(table = "pointers", name = ds)
        pointer <- pointer |> dplyr::filter(version == ver) |>
          dplyr::select(report_path)

        # Path from pointer
        report_path <- file.path("data", pointer$report_path)

        if (file.exists(report_path)) {
          report_content <- readLines(report_path, warn = FALSE)
          return(list(success = TRUE, content = paste(report_content, collapse = "\n"), path = report_path))
        } else {
          return(list(success = FALSE, error = paste0("Report file not found: ", report_path)))
        }
      }, error = function(e) {
        return(list(success = FALSE,
                    error = paste0("Error loading validation report: ", conditionMessage(e))))
      })
    })

    # Track state
    shiny::observe({
      result <- validation_report()
      report_state(list(valid = result$success, error = result$error))
    })

    # Render UI
    output$validation_report <- shiny::renderUI({
      state <- report_state()

      if (state$valid) {
        shiny::HTML(validation_report()$content)
      } else {
        shiny::div(
          class = "d-flex flex-column justify-content-center align-items-center",
          style = "min-height: 400px; background-color: #f8f9fa;",
          shiny::icon("exclamation-circle", class = "text-warning fa-4x mb-3"),
          shiny::h4("Report Not Available", class = "text-danger"),
          shiny::p(class = "text-center text-muted", shiny::HTML(state$error)),
          shiny::p(class = "text-center", "Please select a different dataset or contact the administrator.")
        )
      }
    })

    # Download report
    output$download_report <- shiny::downloadHandler(
      filename = function() {
        paste0(dataset(), "_validation_report.html")
      },
      content = function(file) {
        result <- validation_report()

        if (result$success) {
          file.copy(result$path, file)
        } else {
          writeLines(result$error, file)
        }
      }
    )
  })
}

#' UI Module for Validation Report
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


#' Server Module for Validation Report panel
#'
#' @param id ID for the module
#' @param dataset Reactive expression that returns the selected dataset
#' @export
#
validationReportServer <- function(id, dataset) {
  shiny::moduleServer(id, function(input, output, session) {
    # Create reactive value for tracking report state
    report_state <- shiny::reactiveVal(list(valid = TRUE, error = NULL))

    # Reactive to safely try loading the report and capture any errors
    validation_report <- shiny::reactive({
      # Get dataset value
      ds_value <- dataset()
      if (is.null(ds_value) || ds_value == "") {
        return(list(success = FALSE, error = "No dataset selected"))
      }

      # Path to saved HTML report files in your package
      report_path <- system.file(
        paste0("validation/", ds_value, "_validation.html"),
        package = "spectr"
      )

      # Check if report exists
      if (file.exists(report_path)) {
        tryCatch({
          # Read HTML content
          report_content <- readLines(report_path, warn = FALSE)
          report_html <- paste(report_content, collapse = "\n")
          return(list(success = TRUE, content = report_html, path = report_path))
        }, error = function(e) {
          return(list(
            success = FALSE,
            error = paste0("Error loading validation report for dataset: ", ds_value,
                           "<br>Technical details: ", conditionMessage(e))
          ))
        })
      } else {
        return(list(
          success = FALSE,
          error = paste0("No validation report available for dataset: ", ds_value)
        ))
      }
    })

    # Update report state when validation_report changes
    shiny::observe({
      result <- validation_report()
      report_state(list(valid = result$success, error = result$error))
    })

    # Display validation report or error message
    output$validation_report <- shiny::renderUI({
      state <- report_state()

      if (state$valid) {
        # Return the report HTML content
        shiny::HTML(validation_report()$content)
      } else {
        # Return an error message with consistent styling
        shiny::div(
          class = "d-flex flex-column justify-content-center align-items-center",
          style = "min-height: 400px; background-color: #f8f9fa;",
          shiny::icon("exclamation-circle", class = "text-warning fa-4x mb-3"),
          shiny::h4("Report Not Available", class = "text-danger"),
          shiny::p(
            class = "text-center text-muted",
            shiny::HTML(state$error)
          ),
          shiny::p(
            class = "text-center",
            "Please select a different dataset or contact the administrator."
          )
        )
      }
    })

    # Download handler for validation report
    output$download_report <- shiny::downloadHandler(
      filename = function() {
        paste0(dataset(), "_validation_report.html")
      },
      content = function(file) {
        result <- validation_report()

        if (result$success) {
          file.copy(result$path, file)
        } else {
          # Create a simple HTML file with our error message
          error_html <- paste0(
            "<!DOCTYPE html>",
            "<html>",
            "<head>",
            "  <title>Report Not Available</title>",
            "  <style>",
            "    body { font-family: Arial, sans-serif; text-align: center; padding: 50px; }",
            "    .error-icon { font-size: 48px; color: #f0ad4e; margin-bottom: 20px; }",
            "    .error-title { color: #d9534f; }",
            "    .error-message { color: #666; margin: 20px 0; }",
            "  </style>",
            "</head>",
            "<body>",
            "  <div class='error-icon'>⚠️</div>",
            "  <h2 class='error-title'>Report Not Available</h2>",
            "  <p class='error-message'>", result$error, "</p>",
            "  <p>Please select a different dataset or contact the administrator.</p>",
            "</body>",
            "</html>"
          )

          writeLines(error_html, file)
        }
      }
    )
  })
}

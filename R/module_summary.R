#' Summary user interface
#'
#' @param id Namespace ID for the module
#' @export
#'
summaryUI <- function(id) {
  ns <- shiny::NS(id)

  bslib::card(
    class = "shadow-sm",
    bslib::card_header("Summary Table"),
    bslib::card_body(
      shiny::uiOutput(ns("summary_table"))  # Matches renderUI in server
    )
  )
}



#' Summary server logic
#'
#' @param id ID for the module
#'
#' @export
#
summaryServer <- function(id, dataset, version) {
  shiny::moduleServer(id, function(input, output, session) {
    # Create reactive value for tracking table state
    table_state <- shiny::reactiveVal(list(valid = TRUE, error = NULL))

    # Reactive to safely try generating the table and capture any errors
    summary_table_data <- shiny::reactive({
      ds_value <- dataset()
      ver_value <- version()

      if (is.null(ds_value) || ds_value == "") {
        return(list(success = FALSE, error = "No dataset selected"))
      }

      tryCatch({
        table_obj <- table_pointer(pointer_name = ds_value, date = ver_value)
        list(success = TRUE, table = table_obj)
      }, error = function(e) {
        list(
          success = FALSE,
          error = paste0("Unable to generate summary table for dataset: ", ds_value,
                         "<br>Technical details: ", conditionMessage(e))
        )
      })
    })

    # Update our table state whenever summary_table_data changes
    shiny::observe({
      result <- summary_table_data()
      table_state(list(valid = result$success, error = result$error))
    })

    # Render GT table or error message
    output$summary_table <- shiny::renderUI({
      state <- table_state()

      if (state$valid) {
        gt::gt_output(session$ns("gt_summary_table"))
      } else {
        shiny::div(
          class = "d-flex flex-column justify-content-center align-items-center",
          style = "min-height: 300px; background-color: #f8f9fa;",
          shiny::icon("exclamation-circle", class = "text-warning fa-4x mb-3"),
          shiny::h4("Summary Not Available", class = "text-danger"),
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

    # The actual GT table rendering (only when data is valid)
    output$gt_summary_table <- gt::render_gt({
      state <- table_state()
      shiny::req(state$valid)

      summary_table_data()$table
    })
  })
}

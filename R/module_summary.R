#' UI Module for Summary panel
#'
#' @param id ID for the module
#'
#' @export
#
summaryUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_column_wrap(
    width = "100%",
    fill = FALSE,
    bslib::card(
      class = "shadow-sm",
      bslib::card_header(
        class = "bg-light",
        shiny::h4("Summary", class = "m-0")
      ),
      gt::gt_output(ns("summary_table"))
    )
  )
}

# Server Module for Summary panel
# summaryServer <- function(id, dataset) {
#   shiny::moduleServer(id, function(input, output, session) {
#     # Render GT table
#     output$summary_table <- gt::render_gt({
#       spectr::pointer_table(dataset())
#     })
#   })
# }

#' Server Module for Summary panel
#'
#' @param id ID for the module
#' @param dataset Reactive expression that returns the dataset to be summarized
#' @export
#

summaryServer <- function(id, dataset) {
  shiny::moduleServer(id, function(input, output, session) {
    # Create reactive value for tracking table state
    table_state <- shiny::reactiveVal(list(valid = TRUE, error = NULL))

    # Reactive to safely try generating the table and capture any errors
    summary_table_data <- shiny::reactive({
      # Get dataset value
      ds_value <- dataset()
      if (is.null(ds_value) || ds_value == "") {
        return(list(success = FALSE, error = "No dataset selected"))
      }

      # Try to create the summary table, catching any errors
      tryCatch({
        table_obj <- spectr::table_pointer(ds_value)
        return(list(success = TRUE, table = table_obj))
      }, error = function(e) {
        # Return error message in a user-friendly format
        return(list(
          success = FALSE,
          error = paste0("Unable to generate summary table for dataset: ", ds_value,
                         "<br>Technical details: ", conditionMessage(e))
        ))
      })
    })

    # Update our table state whenever summary_table_data changes
    shiny::observe({
      result <- summary_table_data()
      # Update our table state
      table_state(list(valid = result$success, error = result$error))
    })

    # Render GT table or error message
    output$summary_table <- shiny::renderUI({
      state <- table_state()

      if (state$valid) {
        # Return the GT table
        gt::gt_output(session$ns("gt_summary_table"))
      } else {
        # Return an error message with consistent styling
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

      # Return the actual table object
      summary_table_data()$table
    })
  })
}

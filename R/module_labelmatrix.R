#' UI Module for labelMatrix
#'
#' @param id The namespace ID for the module.
#' @export
#

labelMatrixUI <- function(id) {
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
          shiny::h4("Label Matrix", class = "m-0"),
          shiny::span(
            class = "badge bg-info",
            "Visualizes labels across versions"
          )
        )
      ),
      shiny::hr(),
      shiny::uiOutput(ns("labelmatrix")),
      bslib::card_footer(
        "The label matrix shows which factorial labels are included in a given data set."
      )
    )
  )
}


#' Server Module for Type Matrix panel
#'
#' @param id The namespace ID for the module.
#' @param dataset A reactive expression that returns the selected dataset.
#' @export
#

labelMatrixServer <- function(id, dataset) {
  shiny::moduleServer(id, function(input, output, session) {
    # Create reactive value for tracking data validity and error messages
    plot_state <- shiny::reactiveVal(list(valid = TRUE, error = NULL))

    # Reactive to safely try plotting and capture any errors
    plot_result <- shiny::reactive({
      # Ensure we have a dataset value
      ds_value <- dataset()
      if (is.null(ds_value) || ds_value == "") {
        return(list(success = FALSE, error = "No dataset selected"))
      }

      # Check if the dataset exists in our allowed list
      if (!(ds_value %in% c("penguins", "dataset2"))) {
        return(list(success = FALSE, error = paste0("No validation report available for dataset: ", ds_value)))
      }

      # Try to create the plot, catching any errors
      tryCatch({
        plot_obj <- plot_LabelMatrix(table = ds_value)
        return(list(success = TRUE, plot = plot_obj))
      }, error = function(e) {
        # Return error message in a user-friendly format
        return(list(
          success = FALSE,
          error = paste0("Unable to generate visualization for dataset: ", ds_value,
                         "<br>Technical details: ", conditionMessage(e))
        ))
      })
    })

    # Update our plot state whenever plot_result changes
    shiny::observe({
      result <- plot_result()
      # Update our plot state - but don't show notifications
      plot_state(list(valid = result$success, error = result$error))
    })

    # Output that will display either the plot or an error message
    output$labelmatrix <- shiny::renderUI({
      state <- plot_state()

      # Check if plotting was successful
      if (state$valid) {
        # Return the plot output
        shiny::plotOutput(session$ns("type_plot"), height = "600px")
      } else {
        # Return an error message
        shiny::div(
          class = "d-flex flex-column justify-content-center align-items-center",
          style = "height: 600px; background-color: #f8f9fa;",
          shiny::icon("exclamation-circle", class = "text-warning fa-4x mb-3"),
          shiny::h4("Data Visualization Error", class = "text-danger"),
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

    # Render type matrix plot (only executed when data is valid)
    output$type_plot <- shiny::renderPlot({
      # This will only be called when plot_state()$valid is TRUE
      state <- plot_state()
      shiny::req(state$valid)

      # At this point we know the plot can be generated
      plot_result()$plot
    })
  })
}

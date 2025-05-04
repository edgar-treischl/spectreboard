#' UI Module for Presence Matrix panel
#'
#' @param id ID for the module
#' @export
#

presenceMatrixUI <- function(id) {
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
          shiny::h4("Variable Matrix", class = "m-0"),
          shiny::span(
            class = "badge bg-info",
            "Visualizes included variables of a givent table."
          )
        )
      ),
      shiny::hr(),
      # Use uiOutput for conditional content display
      shiny::uiOutput(ns("plot_or_message"), height = "400px", width = "100%"),
      bslib::card_body(
        # Add numeric input for skip parameter only on this tab
        shiny::fluidRow(
          shiny::column(
            width = 8,
            shiny::p(
              "Number of old data version to skip. ",
              class = "text-muted small pt-2"
            )
          ),
          shiny::column(
            width = 4,
            shiny::sliderInput(ns("skip"), "Skip Rows",
                               min = 0, max = 10, value = 0, step = 1)
          )
        )
      ),
      bslib::card_footer(
        "The variable matrix shows which columns are included in a given data set."
      )
    )
  )
}


#' Server Module for Presence Matrix panel
#'
#' @param id ID for the module
#' @param dataset Reactive expression that returns the dataset to be visualized
#' @export
#
presenceMatrixServer <- function(id, dataset) {
  shiny::moduleServer(id, function(input, output, session) {
    # Create reactive value for tracking plot state
    plot_state <- shiny::reactiveVal(list(valid = TRUE, error = NULL))

    # Reactive to safely try plotting and capture any errors
    plot_result <- shiny::reactive({
      # Ensure we have a dataset value
      ds_value <- dataset()
      if (is.null(ds_value) || ds_value == "") {
        return(list(success = FALSE, error = "No dataset selected"))
      }

      # Try to create the plot, catching any errors
      tryCatch({
        skip_value <- input$skip
        plot_obj <- spectr::plot_PresenceMatrixWeb(ds_value, skip_value)
        return(list(success = TRUE, plot = plot_obj))
      }, error = function(e) {
        # Return error message in a user-friendly format
        return(list(
          success = FALSE,
          error = paste0("Unable to generate visualization for dataset: ", ds_value,
                         "<br>Technical details: ", conditionMessage(e))
        ))
      })
    }) |> shiny::bindCache(dataset(), input$skip, cache = "session")

    # Update our plot state whenever plot_result changes
    shiny::observe({
      result <- plot_result()
      # Just update plot state without showing notifications
      plot_state(list(valid = result$success, error = result$error))
    })

    # Output that will display either the plot or an error message
    output$plot_or_message <- shiny::renderUI({
      state <- plot_state()

      # Check if plotting was successful
      if (state$valid) {
        # Return the plot output
        shiny::plotOutput(session$ns("presence_plot"), height = "600px")
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
            "Please select a different dataset or adjust settings."
          )
        )
      }
    })

    # Render presence matrix plot (only executed when data is valid)
    output$presence_plot <- shiny::renderPlot({
      # This will only be called when plot_state()$valid is TRUE
      state <- plot_state()
      shiny::req(state$valid)

      # At this point we know the plot can be generated
      plot_result()$plot
    },res = 96, height = function() session$clientData$output_responsivePlot_width * 0.7) |>
      shiny::bindCache(dataset(), input$skip, cache = "session")
  })
}

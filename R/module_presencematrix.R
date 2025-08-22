#' Presence user interface
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
            "Inspect which variables (names) are present over time."
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
            shiny::sliderInput(ns("skip"), "Skip older versions from the plot:",
                               min = 0, max = 10, value = 0, step = 1)
          )
        )
      ),
      bslib::card_footer(
        "The variable matrix shows which columns (names) are (no longer) included in the data."
      )
    )
  )
}


#' Presence server logic
#'
#' @param id ID for the module
#' @param dataset Reactive expression that returns the dataset to be visualized
#' @export
#
presenceMatrixServer <- function(id, dataset) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive value to track plot state
    plot_state <- shiny::reactiveVal(list(valid = TRUE, error = NULL))

    # Reactive expression that tries to generate the plot safely
    plot_result <- shiny::reactive({
      ds <- shiny::req(dataset())
      skip_value <- input$skip

      tryCatch({
        plot_obj <- plot_PresenceMatrixWeb(table = ds, skip = skip_value)
        list(success = TRUE, plot = plot_obj)
      }, error = function(e) {
        list(
          success = FALSE,
          error = paste0(
            "Unable to generate visualization for dataset: ", ds,
            "<br>Technical details: ", conditionMessage(e)
          )
        )
      })
    }) |> shiny::bindCache(dataset(), input$skip, cache = "session")

    # Update plot state when plot result changes
    shiny::observe({
      result <- plot_result()
      plot_state(list(valid = result$success, error = result$error))
    })

    # UI output: either plot or error message
    output$plot_or_message <- shiny::renderUI({
      state <- plot_state()

      if (state$valid) {
        shiny::plotOutput(session$ns("presence_plot"), height = "600px")
      } else {
        shiny::div(
          class = "d-flex flex-column justify-content-center align-items-center",
          style = "height: 600px; background-color: #f8f9fa;",
          shiny::icon("exclamation-circle", class = "text-warning fa-4x mb-3"),
          shiny::h4("Data Visualization Error", class = "text-danger"),
          shiny::p(class = "text-center text-muted", shiny::HTML(state$error)),
          shiny::p(class = "text-center", "Please select a different dataset or adjust settings.")
        )
      }
    })

    # Plot output (only shown if valid)
    output$presence_plot <- shiny::renderPlot({
      shiny::req(plot_state()$valid)
      plot_result()$plot
    }, res = 96, height = 600)
  })
}


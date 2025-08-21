#' Type user interface
#'
#' @param id ID for the module
#' @export
#
typeMatrixUI <- function(id) {
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
          shiny::h4("Class Matrix", class = "m-0"),
          shiny::span(
            class = "badge bg-info",
            "Visualizes data classes across versions"
          )
        )
      ),
      shiny::hr(),
      shiny::uiOutput(ns("plot_or_message")),
      bslib::card_footer(
        "The class matrix shows which classes are included in a given data set."
      )
    )
  )
}



#' Type server logic
#'
#' @param id ID for the module
#' @param dataset Reactive expression that returns the selected dataset
#'
#' @export
#
typeMatrixServer <- function(id, dataset) {
  shiny::moduleServer(id, function(input, output, session) {

    # Track state of plot generation
    plot_state <- shiny::reactiveVal(list(valid = TRUE, error = NULL))

    # Attempt to generate the plot safely
    plot_result <- shiny::reactive({
      ds <- shiny::req(dataset())

      tryCatch({
        plot_obj <- plot_TypeMatrixWeb(table = ds)
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
    }) |> shiny::bindCache(dataset(), cache = "session")

    # Update state whenever plot generation is attempted
    shiny::observe({
      result <- plot_result()
      plot_state(list(valid = result$success, error = result$error))
    })

    # Show either the plot or an error message
    output$plot_or_message <- shiny::renderUI({
      state <- plot_state()

      if (state$valid) {
        shiny::plotOutput(session$ns("type_plot"), height = "600px")
      } else {
        shiny::div(
          class = "d-flex flex-column justify-content-center align-items-center",
          style = "height: 600px; background-color: #f8f9fa;",
          shiny::icon("exclamation-circle", class = "text-warning fa-4x mb-3"),
          shiny::h4("Data Visualization Error", class = "text-danger"),
          shiny::p(class = "text-center text-muted", shiny::HTML(state$error)),
          shiny::p(class = "text-center", "Please select a different dataset or contact the administrator.")
        )
      }
    })

    # Actual plot rendering
    output$type_plot <- shiny::renderPlot({
      shiny::req(plot_state()$valid)
      plot_result()$plot
    }, res = 96, height = 600)
  })
}


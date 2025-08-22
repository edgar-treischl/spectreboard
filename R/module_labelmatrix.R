#' Label matrix user interface
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
            "Which labels are included? Did they change?"
          )
        )
      ),
      shiny::hr(),
      shiny::uiOutput(ns("labelmatrix")),
      bslib::card_footer(
        "The label matrix shows which factorial labels are (no longer) included."
      )
    )
  )
}


#' Label matrix server module
#'
#' @param id The namespace ID for the module.
#' @param dataset A reactive expression that returns the selected dataset.
#' @export
#'
labelMatrixServer <- function(id, dataset) {
  shiny::moduleServer(id, function(input, output, session) {

    # Track state of plot generation
    plot_state <- shiny::reactiveVal(list(valid = TRUE, error = NULL))

    # Attempt to generate the plot safely
    plot_result <- shiny::reactive({
      ds <- shiny::req(dataset())

      tryCatch({
        plot_obj <- plot_LabelMatrix(table = ds)
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

    # Update plot state
    shiny::observe({
      result <- plot_result()
      plot_state(list(valid = result$success, error = result$error))
    })

    # UI output: plot or error message
    output$labelmatrix <- shiny::renderUI({
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

    # Render the actual plot
    output$type_plot <- shiny::renderPlot({
      shiny::req(plot_state()$valid)
      plot_result()$plot
    }, res = 96, height = 600)
  })
}

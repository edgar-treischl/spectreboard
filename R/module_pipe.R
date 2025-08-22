#' Pipe Plot UI
#'
#' @param id Module ID
#' @export
pipePlotUI <- function(id) {
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
          shiny::h4("Pipe Validation Matrix", class = "m-0"),
          shiny::span(
            class = "badge bg-info",
            "Heatmap of validation steps per column"
          )
        )
      ),
      shiny::hr(),
      shiny::uiOutput(ns("plot_or_message")),
      bslib::card_footer(
        "This matrix shows which validations are applied to which columns."
      )
    )
  )
}



#' Pipe Plot Server
#'
#' @param id Module ID
#' @param dataset A reactive expression returning the table name (character)
#' @export
pipePlotServer <- function(id, dataset) {
  shiny::moduleServer(id, function(input, output, session) {

    # Track state of plot generation
    plot_state <- shiny::reactiveVal(list(valid = TRUE, error = NULL))

    # Safely attempt to generate the plot
    plot_result <- shiny::reactive({
      tbl <- shiny::req(dataset())

      tryCatch({
        plot_obj <- plot_Pipe(table = tbl)
        list(success = TRUE, plot = plot_obj)
      }, error = function(e) {
        list(
          success = FALSE,
          error = paste0(
            "Unable to generate pipe plot for table: ", tbl,
            "<br>Technical details: ", conditionMessage(e)
          )
        )
      })
    }) |> shiny::bindCache(dataset(), cache = "session")

    # Update internal state
    shiny::observe({
      result <- plot_result()
      plot_state(list(valid = result$success, error = result$error))
    })

    # Conditional UI rendering
    output$plot_or_message <- shiny::renderUI({
      state <- plot_state()

      if (state$valid) {
        shiny::plotOutput(session$ns("pipe_plot"), height = "600px")
      } else {
        shiny::div(
          class = "d-flex flex-column justify-content-center align-items-center",
          style = "height: 600px; background-color: #f8f9fa;",
          shiny::icon("exclamation-circle", class = "text-warning fa-4x mb-3"),
          shiny::h4("Pipe Plot Error", class = "text-danger"),
          shiny::p(class = "text-center text-muted", shiny::HTML(state$error)),
          shiny::p(class = "text-center", "Please select a different table or contact the administrator.")
        )
      }
    })

    # Actual plot rendering
    output$pipe_plot <- shiny::renderPlot({
      shiny::req(plot_state()$valid)
      plot_result()$plot
    }, res = 96, height = 600)
  })
}

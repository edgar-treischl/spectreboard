#' Presence user interface
#'
#' @param id ID for the module
#' @export
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
      shiny::uiOutput(ns("plot_or_message"), height = "400px", width = "100%"),
      bslib::card_body(
        shiny::fluidRow(
          shiny::column(
            width = 8,
            shiny::p("Skip older versions from the plot.", class = "text-muted small pt-2")
          ),
          shiny::column(
            width = 4,
            shiny::uiOutput(ns("skip_ui"))  # dynamically generated select input
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
presenceMatrixServer <- function(id, dataset) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Get unique versions
    version_list <- shiny::reactive({
      ds <- shiny::req(dataset())
      table_obj <- duckdb_table(table = "pointers", name = ds)
      versions <- unique(table_obj$version)
      seq_along(versions) - 1  # skip values: 0 to (n-1)
    })

    # Render the dynamic dropdown for skipping versions
    output$skip_ui <- shiny::renderUI({
      shiny::selectInput(
        inputId = ns("skip"),
        label = "Skip older versions:",
        choices = version_list(),
        selected = 0
      )
    })

    # Reactive value to track plot state
    plot_state <- shiny::reactiveVal(list(valid = TRUE, error = NULL))

    # Reactive expression for generating the plot
    plot_result <- shiny::reactive({
      skip_value <- as.numeric(input$skip %||% 0)
      ds <- shiny::req(dataset())

      tryCatch({
        plot_obj <- plot_PresenceMatrixWeb(table = ds, skip = skip_value)
        list(success = TRUE, plot = plot_obj)
      }, error = function(e) {
        list(
          success = FALSE,
          error = paste0(
            "Unable to generate visualization.<br>",
            "Technical details: ", shiny::HTML(conditionMessage(e))
          )
        )
      })
    }) |> shiny::bindCache(dataset(), input$skip, cache = "session")

    # Track plot state for UI feedback
    shiny::observe({
      result <- plot_result()
      plot_state(list(valid = result$success, error = result$error))
    })

    # Render plot or error message
    output$plot_or_message <- shiny::renderUI({
      state <- plot_state()

      if (state$valid) {
        shiny::plotOutput(ns("presence_plot"), height = "600px")
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

    # Actual output
    output$presence_plot <- shiny::renderPlot({
      shiny::req(plot_state()$valid)
      plot_result()$plot
    }, res = 96, height = 600)
  })
}



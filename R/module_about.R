
#' About user interface
#'
#' This function initializes and runs the Shiny application.
#' @param id The namespace ID for the module.
#'
#' @export
#
aboutUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_column_wrap(
    width = "100%",
    fill = FALSE,
    bslib::card(
      class = "shadow-sm",
      bslib::card_header(
        class = "bg-light",
        shiny::h2("About", class = "m-0")
      ),
      bslib::card_body(
        shiny::p("SpectreApp silently infiltrates the OddJob repository, extracting validation results and decoding them into sleek visual intel. Validation results and pointer data create a rich meta data for a complete audit trail."),
        shiny::p("The next table gives an overview of the most recent validation runs:"),
        shiny::hr(),
        shiny::uiOutput(ns("overview_table")),
        # Add spinner only to the output
        # shinycssloaders::withSpinner(
        #   # Use uiOutput instead of gt_output to support both pre-rendered HTML
        #   # and dynamically rendered gt tables
        #   shiny::uiOutput(ns("overview_table")),
        #   type = 1,  # Choose spinner type (1-8)
        #   color = "#6c757d"
        # ),
        shiny::p("Furthermore, this app includes:"),
        shiny::tags$ul(
          shiny::tags$li(shiny::strong("Overview:"), "An overview based on the pointer file."),
          shiny::tags$li(shiny::strong("Validation:"), "The validation report created with Octopussy."),
          shiny::tags$li(shiny::strong("Variable Matrix:"), "Displays which variables the data includes."),
          shiny::tags$li(shiny::strong("Class Matrix:"), "Depicts which classes the data includes."),
          shiny::tags$li(shiny::strong("Label Matrix:"), "Shows which labels the data includes.")
        ),
        shiny::hr(),
        shiny::p("Created with ", shiny::icon("heart", style = "color: red;"), ", shiny, and", shiny::img(src = "images/octo.png", height = "32px", class = "me-2"))
      )
    )
  )
}

#' About server logic
#'
#' @param id The namespace ID for the module.
#' @export
#'
aboutServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {


    global_data <- duckdb_table(table = "global_data", name = NULL)

    output$overview_table <- shiny::renderUI({
      tryCatch({
        table_obj <- table_overview(data = global_data)
        return(table_obj)
      }, error = function(e) {
        shiny::div(
          class = "text-danger",
          shiny::HTML(paste0("Unable to generate summary table:<br><em>", conditionMessage(e), "</em>"))
        )
      })
    })

  })
}

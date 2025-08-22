
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
        shiny::h2("SpectreApp", class = "m-0")
      ),
      bslib::card_body(
        shiny::p("The SpectreApp gathers validation results from the oddjob repository. Validation results and pointer data create a rich meta data for a complete audit trail."),
        shiny::p("The next table gives an overview of the most recent validations and datasets:"),
        shiny::hr(),
        # Add spinner only to the output
        shinycssloaders::withSpinner(
          # Use uiOutput instead of gt_output to support both pre-rendered HTML
          # and dynamically rendered gt tables
          shiny::uiOutput(ns("overview_table")),
          type = 1,  # Choose spinner type (1-8)
          color = "#6c757d"
        ),
        shiny::p("Furthermore, this app includes:"),
        shiny::tags$ul(
          shiny::tags$li(shiny::strong("Summary:"), "A summary of a picked data set."),
          shiny::tags$li(shiny::strong("Validation:"), "The results of the last validation run."),
          shiny::tags$li(shiny::strong("Presence Matrix:"), "Displays which variables are included in a given version of the data set."),
          shiny::tags$li(shiny::strong("Type Matrix:"), "Dispays which types of variables a given data set includes.")
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

    # Load global_data once (no filtering by table)
    # global_data <- reactive({
    #   duckdb_table(table = "global_data", name = NULL)
    # })

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




# # UI Module for About panel
# aboutUI <- function(id) {
#   ns <- shiny::NS(id)
#   bslib::layout_column_wrap(
#     width = "100%",
#     fill = FALSE,
#     bslib::card(
#       class = "shadow-sm",
#       bslib::card_header(
#         class = "bg-light",
#         shiny::h2("About Xplore", class = "m-0")
#       ),
#       bslib::card_body(
#         shiny::p("The Palmer Penguins dataset contains size measurements for three penguin species observed on three islands in the Palmer Archipelago, Antarctica."),
#         shiny::p("The dataset was collected by Dr. Kristen Gorman and the Palmer Station, Antarctica LTER."),
#         shiny::p("The data includes measurements for penguin species, island, bill length, bill depth, flipper length, body mass, sex, and year."),
#         shiny::hr(),
#         gt::gt_output(ns("overview_table")),
#         shiny::p("This app provides tools to explore and visualize the Palmer Penguins dataset:"),
#         shiny::tags$ul(
#           shiny::tags$li(shiny::strong("Data Summary:"), " View summary statistics of the dataset"),
#           shiny::tags$li(shiny::strong("Presence Matrix:"), " Visualize data completeness and missing values"),
#           shiny::tags$li(shiny::strong("Validation Report:"), " Examine data quality with Pointblank validation")
#         ),
#         shiny::hr(),
#         shiny::p("Created with ", shiny::icon("heart", style = "color: red;"), ", Shiny, and", shiny::img(src = "spectr/octo.png", height = "32px", class = "me-2"))
#       )
#     )
#   )
# }
#
# # Server Module for About panel (no server logic needed for this module)
# aboutServer <- function(id) {
#   shiny::moduleServer(id, function(input, output, session) {
#
#     # Create reactive for table with error handling
#     overview_table <- shiny::reactive({
#       # Try to create the summary table, catching any errors
#       tryCatch({
#         table_obj <- spectr::table_overview()
#         return(list(success = TRUE, table = table_obj))
#       }, error = function(e) {
#         # Return error message in a user-friendly format
#         return(list(
#           success = FALSE,
#           error = paste0("Unable to generate summary table: ",
#                          "<br>Technical details: ", conditionMessage(e))
#         ))
#       })
#     })
#
#     # Connect reactive to output with error handling
#     output$overview_table <- gt::render_gt({
#       result <- overview_table()
#
#       if (result$success) {
#         # If successful, return the table
#         result$table
#       } else {
#         # If there was an error, create a simple gt table with the error message
#         gt::gt(data.frame(Error = result$error)) |>
#           gt::fmt_markdown(columns = gt::everything()) |>
#           gt::tab_options(table.font.color = "red")
#       }
#     })
#   })
# }

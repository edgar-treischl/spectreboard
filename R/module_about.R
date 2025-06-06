
#' UI Module for About panel
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
        shiny::h2("Spectre", class = "m-0")
      ),
      bslib::card_body(
        shiny::p("The `spectre` package is the undercover agency of your validation pipeline. It monitors tables via pointer files and launching the appropriate validation pipeline whenever changes are detected."),
        shiny::p("The results are stored as versioned metadata pointers (in YAML format) in the oddjob GitLab repository. Validation pipelines, reports, and pointer data create a rich meta data for a complete audit trail."),
        shiny::p("This app retrieves the data from oddjob and visualizes it. The following validation results are found:"),
        shiny::hr(),
        # Add spinner only to the output
        shinycssloaders::withSpinner(
          # Use uiOutput instead of gt_output to support both pre-rendered HTML
          # and dynamically rendered gt tables
          shiny::uiOutput(ns("overview_table")),
          type = 1,  # Choose spinner type (1-8)
          color = "#6c757d"
        ),
        shiny::p("Furthermore, this app shows:"),
        shiny::tags$ul(
          shiny::tags$li(shiny::strong("Table Summary:"), "A table summary which includes the meta data."),
          shiny::tags$li(shiny::strong("Validation:"), "The results of the last validation run."),
          shiny::tags$li(shiny::strong("Presence Matrix:"), "Despicts which variables are included in a given version of the data."),
          shiny::tags$li(shiny::strong("Type Matrix:"), "Shows which types of variables a given data set includes.")
        ),
        shiny::hr(),
        shiny::p("Created with ", shiny::icon("heart", style = "color: red;"), ", Shiny, and", shiny::img(src = "spectr/octo.png", height = "32px", class = "me-2"))
      )
    )
  )
}

#' Server Module for About panel
#'
#' @param id The namespace ID for the module.
#' @export
#

aboutServer <- function(id, overview_table_data = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    # If overview_table_data was not provided, create it locally
    local_overview_table_data <- if (is.null(overview_table_data)) {
      shiny::reactive({
        tryCatch({
          table_obj <- table_overview()
          return(list(success = TRUE, table = table_obj))
        }, error = function(e) {
          return(list(
            success = FALSE,
            error = paste0("Unable to generate summary table: ",
                           "<br>Technical details: ", conditionMessage(e))
          ))
        })
      })
    } else {
      overview_table_data
    }

    # Immediately output pre-rendered HTML if available
    observe({
      data <- local_overview_table_data()

      if (data$success && !is.null(data$html)) {
        # Use the pre-rendered HTML
        output$overview_table <- renderUI({
          HTML(data$html)
        })
      } else {
        # Use the regular gt_output/render_gt approach as backup
        output$overview_table <- gt::render_gt({
          if (data$success) {
            data$table
          } else {
            gt::gt(data.frame(Error = data$error)) |>
              gt::fmt_markdown(columns = gt::everything()) |>
              gt::tab_options(table.font.color = "red")
          }
        })
      }
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

# # UI Module for Presence Matrix panel
# typeMatrixUI <- function(id) {
#   ns <- shiny::NS(id)
#   bslib::layout_column_wrap(
#     width = "100%",
#     fill = FALSE,
#     bslib::card(
#       class = "shadow-sm",
#       bslib::card_header(
#         class = "bg-light",
#         shiny::div(
#           class = "d-flex justify-content-between align-items-center",
#           shiny::h4("Data Type Visualization", class = "m-0"),
#           shiny::span(
#             class = "badge bg-info",
#             "Visualizes data types across versions"
#           )
#         )
#       ),
#       shiny::uiOutput(ns("type_error_message")),
#       shiny::hr(),
#       shiny::plotOutput(ns("type_plot"), height = "600px"),
#       bslib::card_body(
#         # Add numeric input for skip parameter only on this tab
#         shiny::fluidRow(
#           shiny::column(
#             width = 8,
#             shiny::p(
#               "Number of old data version to skip. ",
#               class = "text-muted small pt-2"
#             )
#           ),
#           shiny::column(
#             width = 4,
#             shiny::sliderInput(ns("skip"), "Skip Rows",
#                                min = 0, max = 10, value = 0, step = 1)
#           )
#         )
#       ),
#       bslib::card_footer(
#         "The type matrix shows data types in versions of a data set."
#       )
#     )
#   )
# }
#
# # Server Module for Type Matrix panel
# typeMatrixServer <- function(id, dataset) {
#   shiny::moduleServer(id, function(input, output, session) {
#     # Function to strip ANSI color codes from strings
#     strip_ansi_colors <- function(text) {
#       # Regular expression to match ANSI color codes
#       gsub("\033\\[[0-9;]*m", "", text)
#     }
#
#     # Create a reactive value to store error message
#     skip_error <- shiny::reactiveVal(NULL)
#
#     # Render error message UI
#     output$type_error_message <- shiny::renderUI({
#       error_msg <- skip_error()
#       if (!is.null(error_msg)) {
#         bslib::card(
#           class = "border-danger mb-3",
#           bslib::card_body(
#             shiny::div(
#               class = "d-flex align-items-center",
#               shiny::icon("exclamation-triangle", class = "text-danger me-2"),
#               shiny::p(error_msg, class = "text-danger m-0")
#             )
#           )
#         )
#       }
#     })
#
#     # Render type matrix plot with error handling and caching
#     output$type_plot <- shiny::renderPlot({
#       # Clear previous error
#       skip_error(NULL)
#
#       # Try to render the plot with error handling
#       tryCatch({
#         spectr::plot_TypeMatrixWeb(dataset(), input$skip)
#       },
#       error = function(e) {
#         # Convert error to character and strip ANSI color codes
#         error_text <- strip_ansi_colors(as.character(e))
#
#         # Extract the maximum skip value from the error message using a more flexible pattern
#         max_skip_match <- regexpr("Maximum skip can only be [0-9]+", error_text)
#
#         if (max_skip_match > 0) {
#           # Extract the max skip number from the error message
#           max_skip_text <- regmatches(error_text, max_skip_match)
#           max_skip <- as.numeric(gsub("Maximum skip can only be ", "", max_skip_text))
#
#           # Set user-friendly error message
#           skip_error(paste0("Skip value is too large! Maximum skip can only be ", max_skip, ". Please reduce the value."))
#
#           # Return an empty plot with error message
#           plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
#           text(0, 0, "Cannot display plot: Skip value is too large.", col = "red", cex = 1.5)
#         } else {
#           # General error handling - strip any ANSI color codes first
#           clean_message <- strip_ansi_colors(e$message)
#           skip_error(paste0("Error: ", clean_message))
#
#           # Return an empty plot with error message
#           plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
#           text(0, 0, "Cannot display plot due to an error.", col = "red", cex = 1.5)
#         }
#       })
#     }) |>
#       # Add caching - applied correctly to the entire renderPlot
#       shiny::bindCache(dataset(), input$skip, cache = "session")
#   })
# }

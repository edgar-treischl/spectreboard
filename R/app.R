#' Preload overview table data
#'
#' This function loads the overview table data before the app UI is rendered,
#' improving initial load time for users.
#'
#' @return A list with success indicator and either the table or error message
#' @keywords internal

preload_overview_data <- function() {
  message("Preloading overview table data...")
  tryCatch({
    # Start timing
    start_time <- Sys.time()

    # Get the data
    table_obj <- table_overview()

    # Pre-render the gt table to HTML
    # This moves the rendering work before the app starts
    rendered_html <- gt::as_raw_html(table_obj)

    message("Data loading completed in ", round(difftime(Sys.time(), start_time, units = "secs"), 2), " seconds")

    list(
      success = TRUE,
      table = table_obj,
      html = rendered_html
    )
  }, error = function(e) {
    warning("Failed to preload overview table data: ", conditionMessage(e))
    list(
      success = FALSE,
      error = paste0("Unable to generate summary table: ",
                     "<br>Technical details: ", conditionMessage(e)),
      html = NULL
    )
  })
}

#' Modern UI function for the application
#' @export
app_ui <- function() {
  bslib::page_navbar(
    title = shiny::span(
      shiny::img(src = "spectr/logo.png", height = "60px", class = "me-2"),
      "spectre"
    ),
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "lux",
      primary = "#0d6efd",
      secondary = "#6c757d",
      success = "#198754",
      info = "#0dcaf0",
      font_scale = 0.9,
      "enable-transitions" = TRUE,
      # Add IBM Plex Sans font
      base_font = bslib::font_google("IBM Plex Sans")
    ),

    sidebar = bslib::sidebar(
      theme = bslib::bs_theme(version = 5),
      fillable = FALSE,
      width = 300,
      bg = "#f8f9fa",
      title = "Controls",
      class = "shadow-sm rounded",

      bslib::card(
        class = "border-0",
        shiny::p("Select a data set to get the intel.",
                 class = "text-muted small"),
        shiny::selectInput("dataset", "Select Data:",
                           choices = c("penguins", "dataset2"),
                           selected = "penguins"),
        shiny::hr(),
        shiny::p("Meta data made simple with spectr.",
                 class = "text-muted small")
      )
    ),

    bslib::nav_panel(
      title = "About",
      icon = shiny::icon("info-circle"),
      aboutUI("about")
    ),

    bslib::nav_panel(
      title = "Summary",
      icon = shiny::icon("brain"),
      summaryUI("summary")
    ),

    bslib::nav_panel(
      title = "Validation Report",
      icon = shiny::icon("flag"),
      validationReportUI("validation")
    ),
    bslib::nav_panel(
      title = "Variable Matrix",
      icon = shiny::icon("table"),
      presenceMatrixUI("presence")
    ),

    bslib::nav_panel(
      title = "Class Matrix",
      icon = shiny::icon("binoculars"),
      typeMatrixUI("type")
    ),

    bslib::nav_panel(
      title = "Labels",
      icon = shiny::icon("tag"),
      labelMatrixUI("label")
    )
  )
}

#' Main server function for the application
#'
#' @param input Input objects
#' @param output Output objects
#' @param session Session object
#' @param preloaded_data Preloaded overview table data
#' @export
app_server <- function(input, output, session, preloaded_data = NULL) {
  # Reactive value for the selected dataset
  selected_dataset <- shiny::reactive({
    input$dataset
  })

  # Create reactive for overview table data
  overview_table_data <- shiny::reactive({
    preloaded_data
  })

  # Show notification that data is ready
  if (!is.null(preloaded_data) && preloaded_data$success) {
    shiny::showNotification("Data already loaded and ready to use", type = "message")
  }

  # Initialize all module servers
  aboutServer("about", overview_table_data = overview_table_data)
  summaryServer("summary", selected_dataset)
  validationReportServer("validation", selected_dataset)
  presenceMatrixServer("presence", selected_dataset)
  typeMatrixServer("type", selected_dataset)
  labelMatrixServer("label", selected_dataset)
}

#' Run the Shiny application
#'
#' This function initializes and runs the Shiny application.
#'
#' @export
run_app <- function() {
  # Preload data before starting the app
  preloaded_data <- preload_overview_data()

  # Add resource path for images
  shiny::addResourcePath("spectr", system.file("images", package = "spectr"))

  # Create UI
  ui <- app_ui()

  # Create server function with preloaded data
  server <- function(input, output, session) {
    app_server(input, output, session, preloaded_data = preloaded_data)
  }

  # Launch the app
  shiny::shinyApp(ui = ui, server = server)
}

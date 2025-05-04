# library(spectr)
# library(shiny)
# library(bslib)
# library(dplyr)
# library(ggplot2)
# library(gt)
# library(tidyr)


# Source all modules
# source("R/module_about.R")
# source("R/module_summary.R")
# source("R/module_octopussy.R")
# source("R/module_presencematrix.R")
# source("R/module_typematrix.R")
# source("R/module_labelmatrix.R")


# Set up image path




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
#' @export

app_server <- function(input, output, session) {
  # Reactive value for the selected dataset
  selected_dataset <- shiny::reactive({
    input$dataset
  })

  # Initialize all module servers
  aboutServer("about")
  summaryServer("summary", selected_dataset)
  validationReportServer("validation", selected_dataset)
  presenceMatrixServer("presence", selected_dataset)
  typeMatrixServer("type", selected_dataset)
  labelMatrixServer("label", selected_dataset)
}

# Run the app

#' Run the Shiny application
#'
#' This function initializes and runs the Shiny application.
#'
#' @export
run_app <- function() {
  shiny::addResourcePath("spectr", system.file("images", package = "spectr"))
  shiny::shinyApp(ui = app_ui(), server = app_server)
}

#shiny::shinyApp(ui, server)


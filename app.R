# Source ####
source("R/duckDB.R")
source("R/table.R")
source("R/get_oddjob.R")
source("R/plot.R")

# Source module scripts
source("R/module_about.R")
source("R/module_overview.R")
source("R/module_validation.R")
source("R/module_labelmatrix.R")
source("R/module_typematrix.R")
source("R/module_presencematrix.R")


# App ######

# app_ui.R
app_ui <- function() {
  bslib::page_navbar(
    title = shiny::span(
      shiny::img(src = "images/logo.png", height = "60px", class = "me-2"),
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
        shiny::h4("Select the intel for:", class = "text-muted small"),
        shiny::selectInput("dataset", "Data:", choices = NULL),
        shiny::uiOutput("version_ui"),
        shiny::hr(),
        shiny::p("Monitoring data made simple with spectre.", class = "text-muted small")
      )
    ),

    # Navigation panels
    bslib::nav_panel("About", icon = shiny::icon("info-circle"), aboutUI("about")),
    bslib::nav_panel("Overview", icon = shiny::icon("brain"), overviewUI("overview")),
    bslib::nav_panel("Validation Report", icon = shiny::icon("flag"), validationReportUI("validation")),
    bslib::nav_panel("Variables", icon = shiny::icon("table"), presenceMatrixUI("presence")),
    bslib::nav_panel("Classes", icon = shiny::icon("binoculars"), typeMatrixUI("type")),
    bslib::nav_panel("Labels", icon = shiny::icon("tag"), labelMatrixUI("label"))
  )
}



# app_server.R
app_server <- function(input, output, session, preloaded_data = NULL) {
  # Load pointers table reactively
  pointers_data <- reactive({
    db_path <- here::here("data", "meta.duckdb")
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    DBI::dbReadTable(con, "pointers")
  })

  # Populate dataset choices
  observe({
    tbls <- unique(pointers_data()$table)
    updateSelectInput(session, "dataset", choices = tbls, selected = tbls[1])
  })

  # Dynamic version selector
  output$version_ui <- renderUI({
    req(input$dataset)

    versions_df <- pointers_data() |>
      dplyr::filter(table == input$dataset) |>
      dplyr::mutate(
        timestamp_str = stringr::str_extract(version, "[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}-[0-9]{2}-[0-9]{2}"),
        timestamp = lubridate::ymd_hms(stringr::str_replace_all(timestamp_str, "-", ":"), tz = "UTC")
      ) |>
      dplyr::arrange(dplyr::desc(timestamp))

    versions <- versions_df$version
    latest <- versions[1]

    shiny::selectInput(
      "version",
      "Version (optional):",
      choices = versions,
      selected = latest
    )
  })

  # Now connect all modules that use these inputs
  selected_table <- reactive(input$dataset)
  selected_version <- reactive(input$version)

  aboutServer("about")
  overviewServer("overview", dataset = selected_table, version = selected_version)
  validationReportServer("validation", dataset = selected_table, version = selected_version)
  presenceMatrixServer("presence", dataset = selected_table)
  typeMatrixServer("type", dataset = selected_table)
  labelMatrixServer("label", dataset = selected_table)
}




# Shine
shiny::shinyApp(ui = app_ui(), server = app_server)




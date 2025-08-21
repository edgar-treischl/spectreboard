
#' Common input module for selecting datasets and versions
#'
#' @param id ID
#' @export

inputUI <- function(id) {
  ns <- NS(id)
  bslib::card(
    class = "border-0",
    shiny::p("Select a data set to get the intel.", class = "text-muted small"),
    shiny::selectInput(ns("dataset"), "Select Data:", choices = NULL),
    shiny::uiOutput(ns("version_ui")),
    shiny::hr(),
    shiny::p("Meta data made simple with spectr.", class = "text-muted small")
  )
}

inputServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Load pointers table
    pointers_data <- reactive({
      db_path <- here::here("meta.duckdb")
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
      DBI::dbReadTable(con, "pointers")
    })

    # Populate dataset selectInput
    observe({
      tbls <- unique(pointers_data()$table)
      updateSelectInput(session, "dataset", choices = tbls, selected = tbls[1])
    })

    # Dynamic version UI
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
        ns("version"),
        "Select Version (optional)",
        choices = versions,
        #choices = c("All versions" = "", versions),
        selected = latest
      )
    })

    return(list(
      table = reactive(input$dataset),
      version = reactive(input$version),
      pointers = pointers_data
    ))
  })
}

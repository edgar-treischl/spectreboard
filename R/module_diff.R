#' Diff visualization UI
#'
#' @param id Module ID
#' @param table Name of the table/file to diff (default "penguins")
#' @export
diffVisUI <- function(id) {
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
          shiny::h4("Diff Visualization", class = "m-0"),
          shiny::span(
            class = "badge bg-info",
            "Visualize file diffs"
          )
        )
      ),
      shiny::hr(),
      shiny::uiOutput(ns("diff_or_message")),
      bslib::card_footer(
        "Showing the diff for the latest commit affecting the selected file."
      )
    )
  )
}

#' Diff visualization Server
#'
#' @param id Module ID
#' @param table Name of the table/file to diff (default "penguins")
#' @export
diffVisServer <- function(id, table = "penguins") {
  shiny::moduleServer(id, function(input, output, session) {

    # Track state of rendering
    render_state <- shiny::reactiveVal(list(valid = TRUE, error = NULL))

    # Call get_diff internally (this will run on module init or whenever `table` changes)
    diff_lines <- shiny::reactive({
      tryCatch({
        get_diff(table = table)
      }, error = function(e) {
        render_state(list(valid = FALSE, error = paste("Failed to get diff:", e$message)))
        character(0)  # return empty character vector on error
      })
    })

    # Generate the HTML diff safely
    diff_html <- shiny::reactive({
      lines <- diff_lines()
      if (length(lines) == 0) {
        return(NULL)
      }

      tryCatch({
        visualize_diff(diff = lines, browse = FALSE)
      }, error = function(e) {
        render_state(list(valid = FALSE, error = paste("Failed to render diff:", e$message)))
        NULL
      })
    })

    # Update render state based on diff_html availability
    shiny::observe({
      if (is.null(diff_html())) {
        # state should already be set on errors above
        return()
      }
      render_state(list(valid = TRUE, error = NULL))
    })

    # Show diff or error message
    output$diff_or_message <- shiny::renderUI({
      state <- render_state()

      if (state$valid) {
        shiny::htmlOutput(session$ns("diff_html"))
      } else {
        shiny::div(
          class = "d-flex flex-column justify-content-center align-items-center",
          style = "height: 600px; background-color: #f8f9fa;",
          shiny::icon("exclamation-circle", class = "text-warning fa-4x mb-3"),
          shiny::h4("Diff Error", class = "text-danger"),
          shiny::p(class = "text-center text-muted", shiny::HTML(state$error)),
          shiny::p(class = "text-center", "Please check your GitLab connection or table name.")
        )
      }
    })

    # Render the HTML diff
    output$diff_html <- shiny::renderUI({
      shiny::req(render_state()$valid)
      diff_html()
    })
  })
}

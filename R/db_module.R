# db_module.R
#' Database Connection Module
#'
#' Handles database connections and queries
#'
dbUI <- function(id) {
  # No UI needed for this module
  NULL
}


#' UI Module for Summary panel
#'
#' @param id ID for the module
#' @param db_config Configuration list for database connection
#'
#' @export
#'
#'
dbServer <- function(id, db_config = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    # Establish single database connection at startup
    db_conn <- NULL

    # Initialize connection
    tryCatch({
      db_path <- if(!is.null(db_config$path)) db_config$path else "meta.duckdb"
      read_only <- if(!is.null(db_config$read_only)) db_config$read_only else FALSE

      db_conn <<- DBI::dbConnect(
        duckdb::duckdb(),
        dbdir = db_path,
        read_only = read_only
      )

      # Verify connection works
      DBI::dbGetQuery(db_conn, "SELECT 1 as test")
      message("Database connection established successfully")

    }, error = function(e) {
      message("Failed to connect to database: ", e$message)
      db_conn <<- NULL
    })

    # Clean up connection when session ends
    onStop(function() {
      if (!is.null(db_conn) && DBI::dbIsValid(db_conn)) {
        tryCatch({
          DBI::dbDisconnect(db_conn)
          message("Database connection closed")
        }, error = function(e) {
          message("Error closing database connection: ", e$message)
        })
      }
    })

    # Helper function to check connection
    is_connected <- function() {
      !is.null(db_conn) && DBI::dbIsValid(db_conn)
    }

    # Load initial data at startup (cached)
    initial_data <- shiny::reactive({
      if (!is_connected()) return(NULL)

      tryCatch({
        DBI::dbGetQuery(db_conn, "SELECT * FROM global_data")
      }, error = function(e) {
        message("Error loading initial data: ", e$message)
        NULL
      })
    })

    # Return database functions
    return(list(
      # Connection status
      is_connected = shiny::reactive(is_connected()),

      # Initial data (loaded once and cached)
      initial_data = initial_data,

      # Overview data
      get_overview_data = shiny::reactive({
        if (!is_connected()) return(NULL)

        tryCatch({
          DBI::dbGetQuery(db_conn, "SELECT * FROM global_data WHERE category = 'overview'")
        }, error = function(e) {
          message("Error getting overview data: ", e$message)
          NULL
        })
      }),

      # Validation data
      get_validation_data = shiny::reactive({
        if (!is_connected()) return(NULL)

        tryCatch({
          DBI::dbGetQuery(db_conn, "SELECT * FROM global_data WHERE category = 'validation'")
        }, error = function(e) {
          message("Error getting validation data: ", e$message)
          NULL
        })
      }),

      # Dataset summary (non-reactive function for specific queries)
      get_dataset_summary = function(dataset_id) {
        if (!is_connected()) return(NULL)

        tryCatch({
          query <- "SELECT * FROM global_data WHERE dataset_id = $1"
          DBI::dbGetQuery(db_conn, query, params = list(dataset_id))
        }, error = function(e) {
          message("Error getting dataset summary: ", e$message)
          NULL
        })
      },

      # Generic query function
      execute_query = function(query, params = list()) {
        if (!is_connected()) return(NULL)

        tryCatch({
          if (length(params) > 0) {
            DBI::dbGetQuery(db_conn, query, params = params)
          } else {
            DBI::dbGetQuery(db_conn, query)
          }
        }, error = function(e) {
          message("Error executing query: ", e$message)
          NULL
        })
      }
    ))
  })
}

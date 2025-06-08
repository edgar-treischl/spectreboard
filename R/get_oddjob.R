#' Call Oddjob via the GitLab API
#'
#' @param table Optional string to specify the table name.
#'
#' @returns Results from the GitLab API.
#' @export
#'
call_oddjob <- function(table = NULL) {
  #glcon <- AmtSchulGit::gitlab_connect()
  #userkeys <- keyring::key_list("gitlab_api")
  #user <- userkeys$username

  glcon <- tryCatch({
    gitlabr::set_gitlab_connection(
      gitlab_url = "https://gitlab.lrz.de",
      private_token = Sys.getenv("GITLAB_API_TOKEN") #keyring::key_get(service = "gitlab_api", username = user)
    )
  },
  warning = function(w) {
    rlang::warn(message = w$message, class = "gitlab_warning")
  },
  error = function(e) {
    rlang::abort(message = e$message, class = "gitlab_error")
  })

  if (rlang::is_null(glcon)) {
    cli::cli_abort("Can't connect to GitLab.")
  }

  # Get file from GitLab
  result <- gitlabr::gl_get_file(
    project = "216273",
    file_path = "penguins/pointers/penguins_2025-04-30T07-23-28.yaml",
  )

  # Disconnect from GitLab
  gitlabr::unset_gitlab_connection()

  return(result)
}




#' Get Information from Oddjob
#'
#' @param table_name Name of the table to retrieve information for.
#'
#' @returns List with meta data.
#' @export
#'
#' @examples
get_oddjob <- function(table_name = NULL) {

  received_table <- call_oddjob(table = table_name)


  pointer_data <- yaml::yaml.load(received_table)


  pointer_df <- tibble::tibble(
    table = pointer_data$table,
    version = pointer_data$version,
    status = pointer_data$validation_status,
    schema_hash = pointer_data$schema_hash,
    data_hash = pointer_data$data_hash,
    validated_by = pointer_data$validated_by,
    git_sha = pointer_data$git_commit_sha
  )

  columns_df <- purrr::map_dfr(pointer_data$metadata$columns, ~tibble::tibble(
    column_name = .x$column_name,
    label = .x$label,
    type = .x$type,
    levels = .x$levels,
    description = .x$description
  ))

  columns_df

  #return both as list
  pointer_list <- list(
    pointer = pointer_df,
    columns = columns_df
  )

  return(pointer_list)
}

#get_oddjob(table_name = "penguins")




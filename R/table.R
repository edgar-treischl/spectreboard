#' Create a table with pointer information
#'
#' @param pointer_name Name of the pointer/data
#'
#' @returns A gt table with pointer information
#' @export
#'
table_pointer <- function(pointer_name) {
  table_pointers <- spectr::read_pointer(data = pointer_name)
  table_pointersindex <- table_pointers[[1]]
  table_meta <- table_pointers[[2]]

  informant_data <- table_meta |>
    dplyr::select(column = column_name, label, type, levels, description)



  table_title <- paste0("**Data:** ", table_pointersindex$table)
  table_sub <- paste0(emoji::emoji("package"),
                      " **Build:** ",
                      table_pointersindex$version,
                      "<br>",
                      emoji::emoji("checkmark"),
                      " **Validation:** ",
                      table_pointersindex$status)

  table_cap <- paste0(emoji::emoji("detective"), " **Agent:** ", table_pointersindex$validated_by)


  table <- informant_data |>
    gt::gt() |>
    gt::tab_header(
      title = gt::md(table_title),
      subtitle = gt::md(table_sub)
    ) |>
    gt::cols_label(
      column = gt::md("**Column**"),
      label = gt::md("**Label**"),
      type = gt::md("**Type**"),
      levels = gt::md("**Levels**"),
      description = gt::md("**Description**")
    ) |>
    gt::fmt_markdown(columns = dplyr::everything()) |>
    gt::tab_style(
      style = gt::cell_text(
        size = gt::px(22),
        weight = 500,
        align = "left",
        color = "#444444"
      ),
      locations = gt::cells_title("title")
    ) |>
    gt::tab_style(
      style = gt::cell_text(
        size = gt::px(18),
        align = "left"
      ),
      locations = gt::cells_title("subtitle")
    ) |>
    gt::tab_options(
      table.width = "650",
      table.font.size = gt::pct(110)
    ) |>
    gt::opt_table_font(font = gt::google_font("IBM Plex Sans")) |>
    gt::tab_source_note(source_note = gt::md(table_cap))


  return(table)
}

#pointer_table("penguins_raw")

#' Table Validation Overview
#'
#' @returns A `gt` table with the validation overview.
#' @export
#'
table_overview <- function() {

  glcon <- AmtSchulGit::gitlab_connect()

  # Push file content directly to GitLab
  result <- gitlabr::gl_get_file(
    project = "216273",
    file_path = "global_index.yml",
  )

  # Disconnect from GitLab
  gitlabr::unset_gitlab_connection()


  df <- yaml::yaml.load(result)

  df <- do.call(rbind, df) |> as.data.frame()
  df$version <- sapply(df$version, as.character)

  df$version <- lubridate::ymd_hms(df$version)



  df <- df |>
    dplyr::mutate(
      html_link = paste0(
        '<a href="https://gitlab.lrz.de/edgar-treischl/OddJob/-/tree/main/', table, '/pointers?ref_type=heads" target="_blank">', basename(unlist(path)), '</a>'
      )
    )


  mycols <- c("table", "version", "status","html_link", "validated_by")

  informant_data <- df |>
    dplyr::select(dplyr::all_of(mycols))


  table_title <- paste0(emoji::emoji("100"), " **Validation Overview** ")
  table_sub <- paste0("According to OddJob, these are the most recent datasets")



  table_cap <- paste0(emoji::emoji("tophat"), ' <a href="https://gitlab.lrz.de/edgar-treischl/OddJob"', ' target="_blank">', 'Visit the OddJob Repository', '</a>')



  table <- informant_data |>
    gt::gt() |>
    gt::tab_header(
      title = gt::md(table_title),
      subtitle = gt::md(table_sub)
    ) |>
    gt::cols_label(
      table = gt::md(paste0(emoji::emoji("data"), " **Table**")),
      version = gt::md(paste0(emoji::emoji("compass"), " **Version**")),
      status = gt::md(paste0(emoji::emoji("checkmark"), " **Status**")),
      html_link = gt::md(paste0(emoji::emoji("dart"), " **Link**")),
      validated_by = gt::md(paste0(emoji::emoji("detective"), " **Agent**")),
    ) |>
    gt::fmt_markdown(columns = dplyr::everything()) |>
    gt::tab_style(
      style = gt::cell_text(
        size = gt::px(22),
        weight = 500,
        align = "left",
        color = "#444444"
      ),
      locations = gt::cells_title("title")
    ) |>
    gt::tab_style(
      style = gt::cell_text(
        size = gt::px(18),
        align = "left"
      ),
      locations = gt::cells_title("subtitle")
    ) |>
    gt::tab_options(
      table.width = "650",
      table.font.size = gt::pct(110)
    ) |>
    gt::opt_table_font(font = gt::google_font("IBM Plex Sans")) |>
    gt::tab_source_note(source_note = gt::md(table_cap))

  return(table)

}






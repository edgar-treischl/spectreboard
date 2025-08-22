#' Table Validation Overview
#'
#' @param data Data File
#'
#' @returns A `gt` table with the validation overview.
#' @export
#'
table_overview <- function(data) {

  # Get data
  df <- data
  # Or read from DuckDB for DEV
  #df <- duckdb_table(table = "global_data")
  df$version <- sapply(df$version, as.character)
  df$version <- lubridate::ymd_hms(df$version)


  # Add html link for table
  df <- df |>
    dplyr::mutate(
      html_link = paste0(
        '<a href="https://gitlab.lrz.de/edgar-treischl/OddJob/-/tree/main/', table, '/pointers?ref_type=heads" target="_blank">', basename(unlist(path)), '</a>'
      )
    )

  # Provide nicer column names
  mycols <- c("table", "version", "status","html_link", "validated_by")

  informant_data <- df |>
    dplyr::select(dplyr::all_of(mycols))

  # Add title/subtitle and caption
  table_title <- paste0(emoji::emoji("100"), " **Latest Run** ")
  table_sub <- paste0("(According to OddJob)")
  table_cap <- paste0(emoji::emoji("tophat"), ' <a href="https://gitlab.lrz.de/edgar-treischl/OddJob"', ' target="_blank">', 'Visit the OddJob Repository', '</a>')

  # Create the gt table
  table <- informant_data |>
    gt::gt() |>
    gt::tab_header(
      title = gt::md(table_title)
      #subtitle = gt::md(table_sub)
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
    # Align all column headers (column labels) to the left
    gt::tab_style(
      style = gt::cell_text(
        align = "left"
      ),
      locations = gt::cells_column_labels(columns = dplyr::everything())
    ) |>
    # gt::tab_style(
    #   style = gt::cell_text(
    #     size = gt::px(18),
    #     align = "left"
    #   ),
    #   locations = gt::cells_title("subtitle")
    # ) |>
    gt::tab_options(
      table.width = "650",
      table.font.size = gt::pct(110)
    ) |>
    gt::opt_table_font(font = gt::google_font("IBM Plex Sans")) |>
    gt::tab_source_note(source_note = gt::md(table_cap))

  return(table)

}

#' Create a table with pointer information
#'
#' @param pointer_name Name of the pointer/data
#'
#' @returns A gt table with pointer information
#' @export
#'
table_pointer <- function(pointer_name = "penguins",
                          date = "2025-08-20T13-52-15") {
  # Get data from DuckDB
  table_pointersindex <- duckdb_table(table = "pointers", name = pointer_name)

  # Filter the pointers index for the specific pointer and date
  table_pointersindex <- table_pointersindex |>
    dplyr::filter(table == pointer_name) |>
    dplyr::filter(version == date)

  # Do the same for the columns table
  table_meta <- duckdb_table(table = "columns", name = pointer_name)

  table_meta <- table_meta |>
    dplyr::filter(table == pointer_name) |>
    dplyr::filter(version == date)

  # Select relevant columns for the table
  informant_data <- table_meta |>
    dplyr::select(column = column_name, label, type, levels, description)


  # Add some sugar
  table_title <- paste0("**Data:** ", table_pointersindex$table)
  table_sub <- paste0(emoji::emoji("package"),
                      " **Build:** ",
                      table_pointersindex$version,
                      "<br>",
                      emoji::emoji("checkmark"),
                      " **Validation:** ",
                      table_pointersindex$status)

  table_cap <- paste0(emoji::emoji("detective"), " **Agent:** ", table_pointersindex$validated_by)

  # Create the gt table
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









#' Plot the Presence Matrix
#'
#' @param table Table/Data Name
#' @param skip Skip the first n versions
#' @param clip_date Logical, if TRUE, it will remove the time part from the version
#'
#' @returns A ggplot object
#' @export
#'
plot_PresenceMatrix <- function(table,
                                skip = 0,
                                clip_date = TRUE) {
  # Step 1: Read column-level metadata in long format
  data <- spectr::read_all_pointers(table)$columns

  # Step 2: Ensure no duplicates
  data <- dplyr::distinct(data, version, column_name)

  data |> dplyr::filter(column_name == "gender")

  # Step 3: Create full presence grid
  all_versions <- unique(data$version)
  all_columns <- unique(data$column_name)

  full_grid <- tidyr::expand_grid(
    version = all_versions,
    column_name = all_columns
  )

  meta <- read_all_pointers(table)$pointers

  latest_version <- meta |>
    dplyr::arrange(dplyr::desc(version)) |>
    dplyr::slice(1) |>
    dplyr::pull(version)

  validator <- meta |>
    dplyr::arrange(dplyr::desc(version)) |>
    dplyr::slice(1) |>
    dplyr::pull(validated_by)

  # Step 4: Join and compute presence
  presence_data <- dplyr::left_join(
    full_grid,
    data |> dplyr::mutate(found = TRUE),
    by = c("version", "column_name")
  ) |>
    dplyr::mutate(present = tidyr::replace_na(found, FALSE)) |>
    dplyr::select(version, column_name, present)

  # Load the custom font (IBM Plex Sans)
  sysfonts::font_add_google("IBM Plex Sans", "IBM Plex Sans")
  showtext::showtext_auto()  # Activate showtext

  #IF skip it not NULL
  if (!is.null(clip_date)) {
    #check if the skip is numeric
    if (!is.numeric(skip)) {
      cli::cli_abort("Skip must be a numeric value.")
    }

    # Get the unique versions
    unique_versions <- unique(data$version)

    # Calculate the maximum possible skip (max version - 1)
    max_skip <- length(unique_versions) - 1

    # Ensure the skip is within bounds
    if (skip > max_skip) {
      cli::cli_abort("Skip value is too large! Maximum skip can only be {.val {max_skip}}")
    }

    # Create a sequence to select versions, skipping the oldest `skip` versions
    selected_versions <- unique_versions[(skip + 1):length(unique_versions)]


    # Filter the data by selected versions using the pipe operator and namespace notation
    presence_data <- presence_data |>
      dplyr::filter(version %in% selected_versions)
  }

  if (clip_date) {
    presence_data$version <- sub("T.*", "", presence_data$version)
  }



  #Step 5: Plot
  plot <- ggplot2::ggplot(presence_data, ggplot2::aes(x = version, y = column_name, fill = present)) +
    ggplot2::geom_tile(color = "white", alpha = 0.9) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "#31572c", "FALSE" = "#d00000"),
      labels = c("TRUE" = "Yes", "FALSE" = "No"),
      name = "Present"
    ) +
    ggplot2::labs(
      x = "Version",
      y = "Column"
    ) +
    ggplot2::theme_minimal(base_size = 12,
                           base_family = "IBM Plex Sans") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
      axis.text.y = ggplot2::element_text(size = 10)
    )


  title_text <- paste("Table: ", table)

  title <- cowplot::ggdraw() +
    cowplot::draw_text(
      title_text,
      x = 0.01, hjust = 0, fontface = "bold", size = 20, family = "IBM Plex Sans", color = "#333333"
    )

  # Caption (Bottom)
  caption_text <- paste0(
    "Latest version:", latest_version, "   |   ",
    "Validated by: ", validator
  )


  caption <- cowplot::ggdraw() +
    cowplot::draw_text(
      caption_text,
      x = 0.01, hjust = 0,
      size = 12, family = "IBM Plex Sans", color = "#555555"
    )

  # Step 3: Combine title and plot
  final_plot <- cowplot::plot_grid(
    title,
    plot,
    caption,
    ncol = 1,
    rel_heights = c(0.1, 1, 0.08)
  )

  return(final_plot)
}

#plot_PresenceMatrix("penguins_raw")


#' Plot the Presence Matrix
#'
#' @param table Table/Data Name
#' @param skip Skip the first n versions
#' @param clip_date Logical, if TRUE, it will remove the time part from the version
#'
#' @returns A ggplot object
#' @export
#'

plot_PresenceMatrixWeb <- function(table,
                                skip = 0,
                                clip_date = TRUE) {
  # Step 1: Read column-level metadata in long format
  data <- spectr::read_all_pointers(table)$columns

  # Step 2: Ensure no duplicates
  data <- dplyr::distinct(data, version, column_name)

  data |> dplyr::filter(column_name == "gender")

  # Step 3: Create full presence grid
  all_versions <- unique(data$version)
  all_columns <- unique(data$column_name)

  full_grid <- tidyr::expand_grid(
    version = all_versions,
    column_name = all_columns
  )

  meta <- read_all_pointers(table)$pointers

  latest_version <- meta |>
    dplyr::arrange(dplyr::desc(version)) |>
    dplyr::slice(1) |>
    dplyr::pull(version)

  validator <- meta |>
    dplyr::arrange(dplyr::desc(version)) |>
    dplyr::slice(1) |>
    dplyr::pull(validated_by)

  # Step 4: Join and compute presence
  presence_data <- dplyr::left_join(
    full_grid,
    data |> dplyr::mutate(found = TRUE),
    by = c("version", "column_name")
  ) |>
    dplyr::mutate(present = tidyr::replace_na(found, FALSE)) |>
    dplyr::select(version, column_name, present)

  # Load the custom font (IBM Plex Sans)
  sysfonts::font_add_google("IBM Plex Sans", "IBM Plex Sans")
  showtext::showtext_auto()  # Activate showtext

  #IF skip it not NULL
  if (!is.null(clip_date)) {
    #check if the skip is numeric
    if (!is.numeric(skip)) {
      cli::cli_abort("Skip must be a numeric value.")
    }

    # Get the unique versions
    unique_versions <- unique(data$version)

    # Calculate the maximum possible skip (max version - 1)
    max_skip <- length(unique_versions) - 1

    # Ensure the skip is within bounds
    if (skip > max_skip) {
      cli::cli_abort("Skip value is too large! Maximum skip can only be {.val {max_skip}}")
    }

    # Create a sequence to select versions, skipping the oldest `skip` versions
    selected_versions <- unique_versions[(skip + 1):length(unique_versions)]


    # Filter the data by selected versions using the pipe operator and namespace notation
    presence_data <- presence_data |>
      dplyr::filter(version %in% selected_versions)
  }

  if (clip_date) {
    presence_data$version <- sub("T.*", "", presence_data$version)
  }

  #Sort factor variable

  fuck <- presence_data |>
    dplyr::arrange(dplyr::desc(column_name)) |>
    dplyr::pull(column_name) |>
    unique()

  presence_data$column_name <- factor(presence_data$column_name, levels = fuck)



  #Step 5: Plot
  plot <- ggplot2::ggplot(presence_data, ggplot2::aes(x = version, y = forcats::fct_infreq(column_name), fill = present)) +
    ggplot2::geom_tile(color = "white", alpha = 0.9) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "#31572c", "FALSE" = "#d00000"),
      labels = c("TRUE" = "Yes", "FALSE" = "No"),
      name = "Present"
    ) +
    ggplot2::labs(
      x = "Version",
      y = "Column"
    ) +
    ggplot2::theme_minimal(base_size = 16,
                           base_family = "IBM Plex Sans") +
    ggplot2::theme(
      plot.margin = ggplot2::margin(10, 15, 10, 10),
      axis.text.x = ggplot2::element_text(size = 14, angle = 0, hjust = 0.5),
      axis.text.y = ggplot2::element_text(size = 16),
      aspect.ratio = 0.8
    )+
    ggplot2::coord_cartesian(expand = TRUE, clip = "off")

  #face = "bold"

  # title_text <- paste("Table: ", table)
  #
  # title <- cowplot::ggdraw() +
  #   cowplot::draw_text(
  #     title_text,
  #     x = 0.01, hjust = 0, fontface = "bold", size = 20, family = "IBM Plex Sans", color = "#333333"
  #   )

  # Caption (Bottom)
  # caption_text <- paste0(
  #   "Latest version:", latest_version, "   |   ",
  #   "Validated by: ", validator
  # )
  #
  #
  # caption <- cowplot::ggdraw() +
  #   cowplot::draw_text(
  #     caption_text,
  #     x = 0.01, hjust = 0,
  #     size = 12, family = "IBM Plex Sans", color = "#555555"
  #   )
  #
  # # Step 3: Combine title and plot
  # final_plot <- cowplot::plot_grid(
  #   plot,
  #   caption,
  #   ncol = 1,
  #   rel_heights = c(1, 0.08)
  # )

  return(plot)
}


#plot_PresenceMatrixWeb("penguins")




#' Plot the Type Matrix
#'
#' @param table Table/Data Name
#' @param skip Skip the first n versions
#' @param clip_date Logical, if TRUE, it will remove the time part from the version
#'
#' @returns A ggplot object
#' @export
#'
#'
plot_TypeMatrixWeb <- function(table, skip = 0, clip_date = TRUE) {
  # Step 1: Read column-level metadata in long format
  data <- spectr::read_all_pointers(table)$columns


  # Step 2: Ensure no duplicates
  data <- dplyr::distinct(data, version, column_name, type)

  # Step 3: Create full grid of all versions × all columns
  all_versions <- unique(data$version)
  all_columns <- unique(data$column_name)

  full_grid <- tidyr::expand_grid(
    version = all_versions,
    column_name = all_columns
  )

  meta <- spectr::read_all_pointers(table)$pointers

  latest_version <- meta |>
    dplyr::arrange(dplyr::desc(version)) |>
    dplyr::slice(1) |>
    dplyr::pull(version)

  validator <- meta |>
    dplyr::arrange(dplyr::desc(version)) |>
    dplyr::slice(1) |>
    dplyr::pull(validated_by)

  # Step 4: Join grid with real data
  type_data <- dplyr::left_join(
    full_grid,
    data,
    by = c("version", "column_name")
  )

  # Handle optional skip logic
  if (!is.null(clip_date)) {
    if (!is.numeric(skip)) {
      cli::cli_abort("Skip must be a numeric value.")
    }

    unique_versions <- unique(type_data$version)
    max_skip <- length(unique_versions) - 1

    if (skip > max_skip) {
      cli::cli_abort("Skip value is too large! Maximum skip can only be {.val {max_skip}}")
    }

    selected_versions <- unique_versions[(skip + 1):length(unique_versions)]
    type_data <- type_data |>
      dplyr::filter(version %in% selected_versions)
  }

  if (clip_date) {
    type_data$version <- sub("T.*", "", type_data$version)
  }

  # Sort factor variable levels for consistent plotting
  type_data$column_name <- factor(type_data$column_name, levels = rev(unique(type_data$column_name)))

  # Ensure `type` is factor with nice colors
  type_data$type <- factor(type_data$type)

  # palette <- c(
  #   "fct" = "#1b9e77",
  #   "dbl" = "#d95f02",
  #   "int" = "#7570b3",
  #   "chr" = "#e7298a",
  #   "lgl" = "#66a61e",
  #   "date" = "#e6ab02"
  # )

  # Load custom font
  sysfonts::font_add_google("IBM Plex Sans", "IBM Plex Sans")
  showtext::showtext_auto()

  # Plot
  plot <- ggplot2::ggplot(type_data, ggplot2::aes(x = version, y = column_name, fill = type)) +
    ggplot2::geom_tile(color = "white", alpha = 0.95) +
    # ggplot2::scale_fill_manual(
    #   values = palette,
    #   name = "Type"
    # ) +
    #ggplot2::scale_fill_viridis_d()+
    ggplot2::scale_fill_viridis_d(name = "Type",
                                  na.value = "grey50")+
    ggplot2::labs(
      x = "Version",
      y = "Column",
    ) +
    ggplot2::theme_minimal(base_size = 18, base_family = "IBM Plex Sans") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 14, angle = 0, hjust = 0.5),
      axis.text.y = ggplot2::element_text(size = 16)
    )

  # Add caption with metadata
  # caption_text <- paste0(
  #   "Latest version: ", latest_version, "   |   ",
  #   "Validated by: ", validator
  # )
  #
  # caption <- cowplot::ggdraw() +
  #   cowplot::draw_text(
  #     caption_text,
  #     x = 0.01, hjust = 0,
  #     size = 12, family = "IBM Plex Sans", color = "#555555"
  #   )
  #
  # final_plot <- cowplot::plot_grid(
  #   plot,
  #   caption,
  #   ncol = 1,
  #   rel_heights = c(1, 0.08)
  # )

  return(plot)
}


#plot_TypeMatrixWeb("penguins")

#' Plot Label Matrix
#'
#' @param table Table name
#' @returns A ggplot object
#' @export
#'
#'

plot_LabelMatrix <- function(table) {

  data <- spectr::read_all_pointers(table)$columns


  label_data <- data |>
    dplyr::filter(type == "factor") |>
    dplyr::select(version, column_name, levels) |>
    tidyr::separate_rows(levels, sep = ",\\s*")

  # 2. Create label signature per version x column
  label_matrix <- label_data |>
    dplyr::mutate(version = sub("T.*", "", version)) |>
    dplyr::group_by(column_name, version) |>
    dplyr::summarise(
      label_signature = paste(sort(unique(levels)), collapse = "|"),
      .groups = "drop"
    ) |>
    dplyr::group_by(column_name) |>
    dplyr::arrange(version) |>
    dplyr::mutate(
      prev_signature = dplyr::lag(label_signature),
      changed = label_signature != prev_signature,
      changed = dplyr::if_else(is.na(changed), FALSE, changed)
    ) |>
    dplyr::ungroup()

  # 3. Plot the change matrix
  sysfonts::font_add_google("IBM Plex Sans", "IBM Plex Sans")
  showtext::showtext_auto()

  plot <- ggplot2::ggplot(label_matrix, ggplot2::aes(x = version, y = column_name, fill = changed)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(
      ggplot2::aes(label = stringr::str_trunc(label_signature, 20)),
      size = 5, color = "white"
    ) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "#d00000", "FALSE" = "#31572c"),
      labels = c("TRUE" = "Changed", "FALSE" = "No Change"),
      name = ""
    ) +
    ggplot2::labs(
      x = "Version",
      y = "Column"
    ) +
    ggplot2::theme_minimal(base_size = 18,
                           base_family = "IBM Plex Sans") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 14, angle = 0, hjust = 0.5),
      axis.text.y = ggplot2::element_text(size = 18),
      legend.position = "bottom"
    )

  return(plot)
}

#plot_LabelMatrix(table = "penguins")




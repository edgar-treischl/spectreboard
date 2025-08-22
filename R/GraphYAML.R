# # Read the YAML file
# pipeline <- yaml::read_yaml("penguins/pipe_penguins.yml")
#
# # Function to clean and extract column names from a string like 'c("a", "b")'
# extract_columns <- function(col_string) {
#   cols <- gsub('c\\(|\\)', '', col_string)         # Remove c( and )
#   cols <- gsub('\\"', '', cols)                    # Remove double quotes
#   strsplit(cols, ",\\s*")[[1]]                     # Split by comma and optional space
# }
#
# # Process each validation step into a data frame
# step_info <- lapply(seq_along(pipeline$steps), function(i) {
#   step <- pipeline$steps[[i]]
#   validation_type <- names(step)
#   params <- step[[validation_type]]
#
#   # Extract and clean column names
#   columns <- if (!is.null(params$columns)) extract_columns(params$columns) else NA
#
#   # Build readable parameter string (excluding columns)
#   params_str <- params
#   params_str$columns <- NULL
#   if (length(params_str) > 0) {
#     params_str <- paste(paste(names(params_str), params_str, sep = "="), collapse = "; ")
#   } else {
#     params_str <- ""
#   }
#
#   data.frame(
#     step_number = i,
#     validation_type = validation_type,
#     columns = paste(columns, collapse = ", "),
#     parameters = params_str,
#     stringsAsFactors = FALSE
#   )
# })
#
# # Combine all step data frames
# steps_df <- dplyr::bind_rows(step_info)
#
# # Expand columns so each row is one (column, validation_type) pair
# columns_expanded <- steps_df |>
#   dplyr::mutate(cols = stringr::str_split(columns, ",\\s*")) |>
#   tidyr::unnest(cols)
#
# # Create wide validation matrix
# validation_matrix <- columns_expanded |>
#   dplyr::distinct(cols, validation_type) |>
#   dplyr::mutate(count = 1) |>
#   tidyr::pivot_wider(names_from = validation_type, values_from = count)
#
# # Convert to long format for heatmap
# validation_long <- validation_matrix |>
#   tidyr::pivot_longer(-cols, names_to = "validation_type", values_to = "count")
#
# # Plot heatmap
# ggplot2::ggplot(validation_long, ggplot2::aes(x = validation_type, y = cols, fill = count)) +
#   ggplot2::geom_tile(color = "gray80") +
#   ggplot2::scale_fill_viridis_c(option = "viridis", na.value = "white") +
#   ggplot2::labs(
#     x = "Validation step",
#     y = "Column"
#   ) +
#   ggplot2::theme_minimal(base_size = 12) +
#   ggplot2::theme(
#     legend.position = "none",
#     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
#     panel.grid = ggplot2::element_blank()
#   )

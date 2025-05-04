#' UI function for scatterplot module
#'
#' @param id The module ID
#' @return A shiny UI element with control inputs
#' @export
#'
scatterplotModuleUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::card(
      bslib::card_header("Plot Settings"),

      shiny::selectInput(inputId = ns("x_var"),
                         label = "X Variable:",
                         choices = c("Bill Length (mm)" = "bill_length_mm",
                                     "Bill Depth (mm)" = "bill_depth_mm",
                                     "Flipper Length (mm)" = "flipper_length_mm",
                                     "Body Mass (g)" = "body_mass_g"),
                         selected = "bill_length_mm"),

      shiny::selectInput(inputId = ns("y_var"),
                         label = "Y Variable:",
                         choices = c("Bill Length (mm)" = "bill_length_mm",
                                     "Bill Depth (mm)" = "bill_depth_mm",
                                     "Flipper Length (mm)" = "flipper_length_mm",
                                     "Body Mass (g)" = "body_mass_g"),
                         selected = "body_mass_g"),

      shiny::selectInput(inputId = ns("color_by"),
                         label = "Color By:",
                         choices = c("None" = "none",
                                     "Species" = "species",
                                     "Island" = "island",
                                     "Sex" = "sex"),
                         selected = "none"),

      shiny::checkboxInput(inputId = ns("add_regression"),
                           label = "Add Regression Line",
                           value = FALSE),

      shiny::sliderInput(inputId = ns("point_size"),
                         label = "Point Size:",
                         min = 1,
                         max = 5,
                         value = 2,
                         step = 0.5)
    )
  )
}

#' UI function for scatterplot module output
#'
#' @param id The module ID
#' @return A shiny UI element with the plot output
#' @export
#'
scatterplotOutputUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::plotOutput(outputId = ns("scatterPlot"), height = "700px", width = "100%"),
    bslib::card(
      bslib::card_header("Summary Statistics"),
      bslib::card_body(
        shiny::uiOutput(outputId = ns("summaryStats"))
      )
    )
  )
}

#' Server function for scatterplot module
#'
#' @param id The module ID
#' @return A Shiny module server function
#' @export
#'
scatterplotModuleServer <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      # Load the required packages
      #require(palmerpenguins)
      #require(ggplot2)
      #require(dplyr)

      # Prepare the data
      penguins_data <- shiny::reactive({
        # Remove rows with NA in the selected variables
        palmerpenguins::penguins |>
          dplyr::filter(!is.na(!!rlang::sym(input$x_var)),
                        !is.na(!!rlang::sym(input$y_var)),
                        if(input$color_by != "none") !is.na(!!rlang::sym(input$color_by)) else TRUE)
      })

      # Create the scatterplot
      output$scatterPlot <- shiny::renderPlot({
        shiny::req(penguins_data())
        data <- penguins_data()

        # Create the base plot using tidy evaluation instead of aes_string
        x_var <- rlang::sym(input$x_var)
        y_var <- rlang::sym(input$y_var)

        p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var)) +
          ggplot2::theme_light(base_size = 14) +
          ggplot2::labs(
            x = gsub("_", " ", tools::toTitleCase(input$x_var)),
            y = gsub("_", " ", tools::toTitleCase(input$y_var)),
            title = paste("Relationship between",
                          gsub("_", " ", tools::toTitleCase(input$x_var)),
                          "and",
                          gsub("_", " ", tools::toTitleCase(input$y_var)))
          )

        # Add colors if requested using tidy evaluation
        if (input$color_by != "none") {
          color_var <- rlang::sym(input$color_by)
          p <- p + ggplot2::aes(color = !!color_var) +
            ggplot2::scale_color_brewer(palette = "Set1")
        }

        # Add points with specified size
        p <- p + ggplot2::geom_point(size = input$point_size, alpha = 0.7)

        # Add regression line if requested
        if (input$add_regression) {
          if (input$color_by != "none") {
            color_var <- rlang::sym(input$color_by)
            p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE, ggplot2::aes(group = !!color_var))
          } else {
            p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE)
          }
        }

        p
      })

      # Display summary statistics with value boxes
      output$summaryStats <- shiny::renderUI({
        shiny::req(penguins_data())
        data <- penguins_data()

        x_var_name <- gsub("_", " ", tools::toTitleCase(input$x_var))
        y_var_name <- gsub("_", " ", tools::toTitleCase(input$y_var))

        if (input$color_by != "none") {
          # Calculate summary stats by group
          stats_by_group <- data |>
            dplyr::group_by(!!rlang::sym(input$color_by)) |>
            dplyr::summarize(
              n = dplyr::n(),
              mean_x = round(mean(!!rlang::sym(input$x_var), na.rm = TRUE), 2),
              mean_y = round(mean(!!rlang::sym(input$y_var), na.rm = TRUE), 2),
              cor = round(cor(!!rlang::sym(input$x_var), !!rlang::sym(input$y_var), use = "complete.obs"), 3)
            )

          # Generate value boxes for each group
          group_sections <- lapply(1:nrow(stats_by_group), function(i) {
            group_row <- stats_by_group[i, ]
            group_name <- group_row[[input$color_by]]

            # Determine colors based on group index
            colors <- c("#1976D2", "#388E3C", "#D32F2F", "#7B1FA2", "#FFA000")
            color_index <- (i-1) %% length(colors) + 1

            shiny::tagList(
              shiny::h4(paste("Group:", group_name), class = "mt-4"),
              bslib::layout_column_wrap(
                width = 1/4,
                bslib::value_box(
                  title = "Count",
                  value = group_row$n,
                  showcase = bsicons::bs_icon("people-fill"),
                  theme = bslib::value_box_theme(bg = colors[color_index], fg = "white")
                ),
                bslib::value_box(
                  title = "Correlation",
                  value = group_row$cor,
                  showcase = bsicons::bs_icon("arrow-down-up"),
                  theme = bslib::value_box_theme(bg = paste0(colors[color_index], "99"), fg = "white")
                ),
                bslib::value_box(
                  title = paste("Mean", x_var_name),
                  value = group_row$mean_x,
                  showcase = bsicons::bs_icon("graph-up-arrow"),
                  theme = bslib::value_box_theme(bg = paste0(colors[color_index], "CC"), fg = "white")
                ),
                bslib::value_box(
                  title = paste("Mean", y_var_name),
                  value = group_row$mean_y,
                  showcase = bsicons::bs_icon("graph-up-arrow"),
                  theme = bslib::value_box_theme(bg = paste0(colors[color_index], "CC"), fg = "white")
                )
              ),

              if (i < nrow(stats_by_group)) shiny::hr()
            )
          })

          return(shiny::tagList(group_sections))

        } else {
          # Calculate overall summary stats
          stats_overall <- data |>
            dplyr::summarize(
              n = dplyr::n(),
              mean_x = round(mean(!!rlang::sym(input$x_var), na.rm = TRUE), 2),
              mean_y = round(mean(!!rlang::sym(input$y_var), na.rm = TRUE), 2),
              cor = round(cor(!!rlang::sym(input$x_var), !!rlang::sym(input$y_var), use = "complete.obs"), 3)
            )

          # Return overall stats as value boxes
          return(
            shiny::tagList(
              bslib::layout_column_wrap(
                width = 1/4,
                bslib::value_box(
                  title = "Count",
                  value = stats_overall$n,
                  showcase = bsicons::bs_icon("people-fill"),
                  theme = bslib::value_box_theme(bg = "gray", fg = "white")
                ),
                bslib::value_box(
                  title = paste("Mean", x_var_name),
                  value = stats_overall$mean_x,
                  showcase = bsicons::bs_icon("graph-up-arrow"),
                  theme = bslib::value_box_theme(bg = "gray", fg = "white")
                ),

                bslib::value_box(
                  title = paste("Mean", y_var_name),
                  value = stats_overall$mean_y,
                  showcase = bsicons::bs_icon("graph-up-arrow"),
                  theme = bslib::value_box_theme(bg = "gray", fg = "white")
                ),

                bslib::value_box(
                  title = "Correlation",
                  value = stats_overall$cor,
                  showcase = bsicons::bs_icon("arrow-down-up"),
                  theme = bslib::value_box_theme(bg = "gray", fg = "white")
                )
              )
            )
          )
        }
      })
    }
  )
}

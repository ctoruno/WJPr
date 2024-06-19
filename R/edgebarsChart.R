#' Plot a Horizontal Edgebars Chart following WJP style guidelines
#'
#' @description
#' `wjp_edgebars()` takes a data frame with a specific data structure (usually long shaped) and returns a ggplot
#' object with an edgebar chart following WJP style guidelines.
#'
#' @param data A data frame containing the data to be plotted.
#' @param y_value A string specifying the variable in the data frame that contains the numeric values to be plotted as bars.
#' @param x_var A string specifying the variable in the data frame that contains the categories for the x-axis.
#' @param label_var A string specifying the variable in the data frame that contains the labels to be displayed near the bars.
#' @param x_lab_pos A string specifying the variable in the data frame that contains the order in which the bars will be displayed. Default is NULL.
#' @param y_lab_pos A numeric value specifying the y-axis position for displaying labels. Default is 0.
#' @param nudge_lab A numeric value specifying the padding for displaying labels in milimeters. Default is 2.5.
#' @param bar_color A string specifying the color for the bars. Default is "#2a2a94".
#' @param margin_top A numeric value specifying the top margin of the plot. Default is 20.
#'
#' @return A ggplot object representing the edge bars plot.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   y_value = c(30, 50, 20),
#'   x_var = c("Category A", "Category B", "Category C"),
#'   label_var = c("Label A", "Label B", "Label C")
#' )
#' 
#' wjp_edgebars(data, "y_value", "x_var", "label_var")
#' }
#'
#'
#' @export

wjp_edgebars <- function(
    data         = NULL,
    y_value      = NULL,
    x_var        = NULL,
    label_var    = NULL,
    x_lab_pos    = NULL,
    y_lab_pos    = 0,
    nudge_lab    = 2.5,
    bar_color    = "#2a2a94",
    margin_top   = 20  
  ) {
  
  # Renaming variables in the data frame to match the function naming
  if (is.null(x_lab_pos)) {
    data <- data %>%
      ungroup() %>%
      mutate(
        x_lab_pos = row_number()
      ) %>%
      dplyr::rename(
        y_value   = all_of(y_value),
        x_var     = all_of(x_var),
        label_var = all_of(label_var)
      )
  } else {
    data <- data %>%
      rename(
        y_value   = all_of(y_value),
        x_var     = all_of(x_var),
        label_var = all_of(label_var),
        x_lab_pos = all_of(x_lab_pos)
      )
  }
  
  # Creating plot
  plt <- ggplot(
    data = data, 
    aes(
      x    = reorder(x_var,x_lab_pos),
      y    = y_value, 
    )
  ) +
    geom_bar(
      fill     = bar_color,
      position = "stack", 
      stat     = "identity",
      width    = 0.35, 
      show.legend = F
    ) +
    geom_richtext(
      aes(
        x        = reorder(x_var,x_lab_pos), 
        y        = y_lab_pos,
        label    = label_var, 
        family   = "Lato Full", 
        fontface = "plain"
      ),
      fill  = NA, 
      hjust = 0, 
      vjust = 0, 
      size  = 3.514598,
      label.color = NA,
      label.padding = unit(c(0, 0, nudge_lab, 0), "mm"),
    ) +
    geom_text(
      aes(
        x = reorder(x_var, x_lab_pos),
        y = y_value,
        label = paste0(format(round(y_value, 0),
                              nsmall = 0),
                       "%")
      ),
      color    = "#4a4a49",
      family   = "Lato Full",
      fontface = "bold", 
      size     = 3.514598, 
      hjust    = -0.1
    ) +
    scale_fill_manual(
      values = c("value"       = bar_color,
                 "empty_value" = "#f3f3f3")
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0,0.15))
    ) +
    coord_flip(clip = "off") +
    WJP_theme() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.y        = element_blank(),
      axis.title.x       = element_blank(),
      axis.title.y       = element_blank(),
      axis.text.x        = element_blank(),
      plot.margin        = margin(margin_top, 10, -15, 0),
      plot.background    = element_blank()
    )
  
  return(plt)
}

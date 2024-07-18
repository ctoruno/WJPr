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
#' @param color_var A string specifying the variable in the data frame that contains the the color groupings for the bars. Default is NULL.
#' @param bar_colors A named vector specifying the colors for the bars. Default is NULL.
#' @param x_lab_pos A string specifying the variable in the data frame that contains the order in which the bars will be displayed. Default is NULL.
#' @param y_lab_pos A numeric value specifying the y-axis position for displaying labels. Default is 0.
#' @param nudge_lab A numeric value specifying the padding for displaying labels in milimeters. Default is 2.5.
#' @param margin_top A numeric value specifying the top margin of the plot. Default is 20.
#' @param bar_width A numeric value specifying the width of the bars. For single bars the default value of 0.35 is recommended, for plots with two bars a value of 0.5 is more suitable.
#' @param ptheme A ggplot aesthetic theme to be applied to the chart. Default is the WJP_theme initilized with the package.
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
    data,
    y_value,
    x_var,
    label_var,
    color_var    = NULL,
    bar_colors   = NULL,
    x_lab_pos    = NULL,
    y_lab_pos    = 0,
    nudge_lab    = 2.5,
    margin_top   = 20,
    bar_width    = 0.35,
    ptheme       = WJP_theme()
  ) {
  
  # Renaming variables in the data frame to match the function naming
  if(is.null(color_var)) {
    color_var  <- "color"
    bar_colors <- c("anchor" = "#2a2a94")
    data <- data %>%
      mutate(
        color = "anchor"
      )
  }
  
  if (is.null(x_lab_pos)) {
    x_lab_pos <- "label_position"
    data <- data %>%
      ungroup() %>%
      mutate(
        label_position = row_number()
      )
  } 
  
  data <- data %>%
    rename(
      y_value   = all_of(y_value),
      x_var     = all_of(x_var),
      label_var = all_of(label_var),
      x_lab_pos = all_of(x_lab_pos),
      color_var = all_of(color_var)
    )
  
  # Creating plot
  plt <- ggplot(
    data = data, 
    aes(
      x    = reorder(x_var,x_lab_pos),
      y    = y_value, 
      fill = color_var
    )
  ) +
    geom_bar(
      position = "dodge", 
      stat     = "identity",
      width    = bar_width, 
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
      position = position_dodge(width = bar_width),
      family   = "Lato Full",
      fontface = "bold", 
      size     = 3.514598, 
      hjust    = -0.1
    ) +
    scale_fill_manual(
      values = bar_colors
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0,0.15))
    ) +
    coord_flip(clip = "off") +
    ptheme +
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

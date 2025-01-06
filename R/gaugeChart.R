#' Plot a Gauge Chart following WJP style guidelines
#'
#' This function creates a gauge chart using ggplot2 based on the provided data frame.
#'
#' @param data A data frame containing the data to be plotted.
#' @param target A string specifying the variable in the data frame that contains the values to be plotted.
#' @param colors A string specifying the variable in the data frame that represents the color groupings for the segments.
#' @param cvec A vector of colors to apply to the segments.
#' @param factor_order A vector specifying the order in which the segments should be plotted.
#' @param labels A string specifying the variable in the data frame that contains the labels to be displayed. Default is NULL.
#' @param crop A numeric vector specifying the amount of space to crop from the Top, Right, Bottom, and Right margins, respectively. Default is c(-10,0,0,-8).
#' @param ptheme A ggplot2 theme object to be applied to the plot. Default is WJP_theme().
#'
#' @return A ggplot object representing the gauge chart.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   target = c(10, 20, 30, 40),
#'   colors = c("red", "blue", "green", "yellow"),
#'   labels = c("A", "B", "C", "D")
#' )
#' cvec <- c("red", "blue", "green", "yellow")
#' 
#' wjp_gauge(data, "target", "colors", cvec, labels = "labels")
#' }
#'
#'
#' @export

wjp_gauge <- function(
    data,                    
    target,             
    colors,
    cvec           = NULL,
    factor_order   = NULL,
    labels         = NULL,
    crop           = c(-10,0,0,-8),
    ptheme         = WJP_theme()
){
  
  # Renaming variables in the data frame to match the function naming
  if (is.null(labels)) {
    data <- data %>%
      mutate(labels_var    = "") %>%
      rename(
        target_var    = all_of(target),
        colors_var    = all_of(colors)
      )
  } else {
    data <- data %>%
      rename(
        target_var    = all_of(target),
        colors_var    = all_of(colors),
        labels_var    = all_of(labels)
      )
  }
  
  
  # Sorting values if necessary
  if (!is.null(factor_order)){
    data <- data %>%
      ungroup() %>%
      mutate(
        colors_var = factor(
          colors_var,
          levels  = factor_order,
          ordered = T
        )
      ) %>%
      arrange(colors_var)
  }
  
  # Getting coordinates
  data <- data %>%
    ungroup() %>%
    mutate(
      ymax       = cumsum(target_var),
      ymin       = ymax-target_var,
      labpos     = ymin  + ((ymax-ymin)/2),
      labels_var = if_else(target_var >= 5, 
                           labels_var,
                           "")
    )
  
  # Drawing chart
  plt <- ggplot(
    data, 
    aes(fill = colors_var, 
        ymax = ymax, 
        ymin = ymin, 
        xmax = 2, 
        xmin = 1)
  ) + 
    geom_rect() + 
    geom_text(
      aes(label = labels_var,
          y     = labpos,
          x     = 1.5),
      color     = "white",
      size      = 2.166058*.pt,
      family    = "Lato Full",
      fontface  = "bold"
    ) +
    scale_x_continuous(limits = c(0,2)) +
    scale_y_continuous(limits = c(0,200)) +
    scale_fill_manual(values  = cvec) +
    coord_polar(theta = "y",
                start = -pi/2) +
    ptheme +
    labs(y = "",
         x = "") +
    theme(
      legend.position  = "none",
      plot.margin      = grid::unit(crop, "mm"),
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      axis.title.x     = element_blank(),
      axis.text.x      = element_blank(),
      axis.text.y      = element_blank()
    )
  
  with_ggtrace(
    x = plt + theme(aspect.ratio = .52),
    method = Layout$render,
    trace_steps = 5L,
    trace_expr = quote({
      panels <- lapply(panels, editGrob, vp = viewport(yscale = c(0.49, 1)))
    }),
    out = "g"
  )
  
}

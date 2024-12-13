wjp_lollipops <- function(
    data,
    target,
    grouping,
    order         = NULL,
    line_size     = 3,
    point_size    = 4,
    line_color    = "#c4c4c4",
    point_color   = "#2a2a94"
) {
  
  # Renaming variables in the data frame to match the function naming
  if (is.null(order)) {
    data <- data %>%
      dplyr::mutate(order_var     = row_number()) %>%
      dplyr::rename(target_var    = all_of(target),
                    grouping_var  = all_of(grouping))
  } else {
    data <- data %>%
      dplyr::rename(target_var    = all_of(target),
                    grouping_var  = all_of(grouping),
                    order_var     = all_of(order))
  }
  
  # Creating plot
  ggplot(data) +
    geom_linerange(
      aes(x    = reorder(grouping_var, order_var),  
          ymin = 0, 
          ymax = target_var), 
      size  = line_size, 
      color = line_color
    ) +
    geom_point(
      aes(y = target_var, 
          x = reorder(grouping_var, order_var)),
      size  = line_size, 
      shape = 16, 
      color = point_color
    ) +
    geom_text(
      aes(y = target_var + 7, 
          x = grouping_var, 
          label = paste0(round(target_var, 0),"%")),
      size     = 3.514598, 
      color    = "black",
      family   = "Lato Full", 
      fontface = "bold"
    ) +
    scale_y_continuous(breaks   = seq(0, 100, by = 10),
                       limits   = c(0,105),
                       labels   = paste0(seq(0, 100, by = 10),"%"), 
                       position = "right") +
    coord_flip() +
    theme_minimal() +
    WJP_theme() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major.y = element_blank()
    )
}
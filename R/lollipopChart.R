wjp_lollipops <- function(
    data,
    target,
    grouping,
    order         = NULL,
    line_size     = 3,
    point_size    = 4,
    line_color    = "#c4c4c4",
    point_color   = "#2a2a94",
    
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
  ggplot(data2plot) +
    geom_linerange(
      aes(y    = reorder(grouping_var, order_var),  
          xmin = 0, 
          xmax = target_var), 
      size  = line_size, 
      color = line_color
    ) +
    geom_point(
      aes(x = target_var, 
          y = reorder(grouping_var, order_value)),
      size  = line_size, 
      shape = 16, 
      color = point_color
    ) +
    geom_text(
      aes(x = target_var + 0.075, 
          y = grouping_var, 
          label = paste0(target_var*100,"%")),
      size     = 3.514598, 
      color    = "black",
      family   = "Lato Full", 
      fontface = "bold"
    ) +
    coord_cartesian(clip="off") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(0, 1, by = 0.1),limits = c(0,1),
                       labels = scales::percent_format(accuracy = 1), position = "top") +
    WJP_theme()
}
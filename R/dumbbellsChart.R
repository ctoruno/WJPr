

wjp_dumbbells <- function(
    data,             
    target, 
    rows,
    color,
    cgroups,  
    cvec      = NULL, 
    order     = NULL,
    ptheme    = WJP_theme()
){
  
  # Renaming variables in the data frame to match the function naming
  data <- data %>%
    pivot_wider(
      id_cols     = all_of(rows),
      names_from  = color,
      values_from = target
    ) %>%
    rename(
      group = all_of(rows),
      start = all_of(cgroups[1]),
      end   = all_of(cgroups[2])
    )
  
  if (is.null(order)){
    data <- data %>%
      ungroup() %>%
      mutate(
        order = row_number()
      )
    
  } else {
   data <- data %>% 
     mutate(
       order = recode(group, !!!order)
     )
  }
  
  # Creating a strip pattern
  strips <- data %>%
    group_by(group) %>%
    summarise() %>%
    mutate(ymin = 0,
           ymax = 100,
           xposition = rev(1:nrow(.)),
           xmin = xposition - 0.5,
           xmax = xposition + 0.5,
           fill = rep(c("grey", "white"), 
                      length.out = nrow(.))) %>%
    pivot_longer(c(xmin, xmax),
                 names_to  = "cat",
                 values_to = "x") %>%
    select(-cat) %>%
    filter(fill != "white")
  
  # Drawing plot
  plt <- ggplot() +
    geom_blank(data      = data,
               aes(x     = reorder(group, -order),
                   y     = end)) +
    geom_ribbon(data      = strips,
                aes(x     = x,
                    ymin  = ymin,
                    ymax  = ymax,
                    group = xposition,
                    fill  = fill),
                show.legend = F) +
    scale_fill_manual(
      values   = c("grey"  = "#EBEBEB",
                   "white" = "#FFFFFF"),
      na.value = NULL
    ) +
    geom_segment(
      data = data,
      aes(
        x    = group,
        xend = group,
        y    = start,
        yend = end
      ),
      color = cvec[2],
      size  = 1.5 
    ) +
    geom_point(
      data = data,
      aes(
        x     = group,
        y     = start
      ),
      color = cvec[1],
      size  = 3
    ) +
    geom_point(
      data = data,
      aes(
        x     = group,
        y     = end
      ),
      color = cvec[2],
      size  = 3
    ) +
    scale_color_manual(values = cvec) +
    scale_y_continuous(limits = c(0,100),
                       breaks = seq(0,100,20),
                       labels = paste0(seq(0,100,20),
                                       "%"),
                       position = "right") +
    coord_flip() +
    ptheme +
    theme(axis.title.x       = element_blank(),
          axis.title.y       = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.background   = element_blank(), 
          panel.ontop = T,
          axis.text.y = element_text(color = "#222221",
                                     hjust = 0))
    
  
}


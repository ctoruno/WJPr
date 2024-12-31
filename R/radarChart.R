#' Plot a Radar Chart following WJP style guidelines
#'
#'#' @description
#' `wjp_radar()` takes a data frame with a specific data structure (usually long shaped) and returns a ggplot
#' object with a radar chart following WJP style guidelines.
#'
#' @param data A data frame containing the data to be plotted.
#' @param axis_var A string specifying the variable in the data frame that contains the groups for the axis.
#' @param target_var A string specifying the variable in the data frame that contains the values to be plotted.
#' @param label_var A string specifying the variable in the data frame that contains the labels to be displayed.
#' @param color_var A string specifying the variable in the data frame that contains the color groupings.
#' @param cvec A vector of colors to apply to lines. The first color will be applied to percentages in labels.
#' @param order_var A string specifying the variable in the data frame that contains the display order of categories. Default is NULL.
#' @param maincat A string indicating the category of labels to show in the radar.
#' @param source A string which can take two values (GPP or QRQ). 
#'
#' @return A ggplot object representing the radar plot.
#'
#' @examples
#' # Always load the WJP fonts (optional)
#' wjp_fonts()
#' 
#' # Preparing data
#' gpp_data <- WJPr::gpp
#' 
#' library(dplyr)
#' library(tidyr)
#' library(haven)
#' 
#' data4radar <- gpp_data %>%
#' select(gend, starts_with("q49")) %>%
#'   mutate(
#'     gender = case_when(
#'       gend == 1 ~ "Male",
#'       gend == 2 ~ "Female"
#'     ),
#'     across(
#'       starts_with("q49"),
#'       \(x) case_when(
#'         x <= 2  ~ 1,
#'         x <= 99 ~ 0
#'       )
#'     )
#'   ) %>%
#'   group_by(gender) %>%
#'   summarise(
#'     across(
#'       starts_with("q49"),
#'       \(x) mean(x, na.rm = T)*100
#'     )
#'   ) %>%
#'   pivot_longer(
#'     !gender,
#'     names_to  = "category",
#'     values_to = "percentage"
#'   ) %>%
#'   mutate(
#'     axis_label = category
#'   )
#' 
#' # Plotting chart
#' wjp_radar(
#'   data4radar,             
#'  axis_var    = "category",         
#'   target_var  = "percentage",       
#'   label_var   = "axis_label",        
#'   color_var   = "gender"
#' )
#' 


wjp_radar <- function(
    data,             
    axis_var,         
    target_var,       
    label_var,        
    color_var,
    maincat,
    cvec      = NULL,   
    order_var = NULL,
    source    = "GPP"
){
  
  # Renaming variables in the data frame to match the function naming
  data <- data %>%
    rename(axis_var    = all_of(axis_var),
           target_var  = all_of(target_var),
           label_var   = all_of(label_var),
           color_var   = all_of(color_var))
  
  if (is.null(order_var)){
    data <- data %>%
      group_by(color_var) %>% 
      mutate(order_var = row_number())
  } else {
    data <- data %>%
      rename(order_var = all_of(order_var))
  }
  
  if (source == "GPP") {
    data <- data %>%
      mutate(
        target_var = target_var/100
      )
  }
  
  # Default colors
  if (is.null(cvec)){
    cvec   <- c("#1D4E89", "#F79256")
  }
  cvec <- c(cvec, "#7D787D")
    
  # Counting number of axis for the radar
  nvertix <- length(unique(data$axis_var))
  
  # Distance to the center of the web 
  central_distance <- 0.2
  
  # Function to generate radar coordinates
  circle_coords <- function(r, n_axis = nvertix){
    fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
    x <- r*cos(fi)
    y <- r*sin(fi)
    
    tibble(x, y, r)
  }
  
  # Function to generate axis lines
  axis_coords <- function(n_axis = nvertix){
    fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2
    x1 <- central_distance*cos(fi)
    y1 <- central_distance*sin(fi)
    x2 <- (1 + central_distance)*cos(fi)
    y2 <- (1 + central_distance)*sin(fi)
    
    tibble(x = c(x1, x2), y = c(y1, y2), id = rep(1:n_axis, 2))
  }
  
  # Function to generate axis coordinates
  text_coords <- function(r      = 1.5, 
                          n_axis = nvertix){
    fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2 + 0.01*2*pi/r
    x <- r*cos(fi)
    y <- r*sin(fi)
    
    tibble(x, y, r = r - central_distance)
  }
  
  # Y-Axis labels
  axis_measure <- tibble(
    r         = seq(0, 1, 0.2),
    parameter = rep(
      data %>% 
        filter(order_var == 1) %>% 
        ungroup() %>%
        distinct(axis_var) %>% 
        pull(axis_var),
      6
    )
  ) %>%
    bind_cols(
      map_df(
        seq(0, 1, 0.2) + central_distance, 
        text_coords
      ) %>% 
        distinct(r, .keep_all = T) %>% 
        select(-r)
    )
  
  if (source == "GPP"){
    axis_measure <- axis_measure %>%
      mutate(
        r = paste0(r*100, "%")
      )
  }
  
  # Generating data points
  rescaled_coords <- function(r, n_axis = nvertix){
    fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
    tibble(r, fi) %>% 
      mutate(x = r*cos(fi), y = r*sin(fi)) %>% 
      select(-fi)
  }
  
  rescaled_data <- data %>% 
    bind_rows(
      data %>% 
        filter(
          axis_var %in% 
            (data %>% 
               filter(order_var == 1) %>% 
               distinct(axis_var) %>% 
               pull(axis_var))
        ) %>%
        mutate(axis_var  = "copy",
               order_var = nvertix)
    ) %>%
    group_by(color_var) %>%
    arrange(order_var) %>%
    mutate(
      coords = rescaled_coords(target_var + central_distance)
    ) %>%
    unnest(cols = c(coords)) 
  
  # Generating ggplot
  radar <-
    
    # We set up the ggplot
    ggplot(
      data = map_df(seq(0, 1, 0.20) + central_distance, circle_coords),
      aes(x = x, 
          y = y)
    ) +
    
    # We draw the outter ring
    geom_polygon(
      data     = circle_coords(1 + central_distance),
      linetype = "dotted",
      color    = "#d1cfd1",
      fill     = NA
    ) +
    
    # We draw the inner rings
    geom_path(
      aes(group = r), 
      lty       = 2, 
      color     = "#d1cfd1"
    ) +
    
    # We draw the ZERO ring
    geom_polygon(
      data = map_df(seq(0, 1, 0.20) + central_distance, circle_coords) %>%
        filter(r == 0.2),
      fill      = NA,
      linetype  = "solid",
      color     = "#d1cfd1"
    ) +
    
    # Then, we draw the Y-axis lines
    geom_line(
      data = axis_coords(), 
      aes(x     = x, 
          y     = y, 
          group = id),
      color = "#d1cfd1"
    ) +
    
    # Along with its labels
    geom_text(
      data = axis_measure,
      aes(x     = x,
          y     = y,
          label = r,
          family   = "Lato Full",
          fontface = "plain",
          color    = "#524F4C"
      )) +
    
    # Then, we add the axis labels
    geom_richtext(
      data  = text_coords() %>%
        mutate(
          n = row_number()
        ),
      aes(x = x, 
          y = y), 
      label = data %>% 
        arrange(order_var) %>% 
        filter(color_var == data %>% 
                 ungroup() %>% 
                 distinct(color_var) %>% 
                 slice_head(n=1)) %>% 
        pull(label_var),
      family      = "Lato Full",
      fontface    = "plain",
      fill        = NA, 
      label.color = NA
    ) +
    
    # We add the data points along with its lines
    geom_point(
      data = rescaled_data, 
      aes(x     = x, 
          y     = y, 
          group = color_var, 
          color = as.factor(color_var)), 
      size      = 3
    ) +
    geom_path(
      data = rescaled_data, 
      aes(x     = x, 
          y     = y, 
          group = color_var, 
          color = as.factor(color_var)), 
      size = 1
    ) +
    
    # Remaining aesthetics
    coord_cartesian(clip = "off") + 
    scale_x_continuous(expand = expansion(mult = 0.125)) + 
    scale_y_continuous(expand = expansion(mult = 0.10)) + 
    scale_color_manual(values = cvec) +
    theme_void() +
    theme(
      panel.background   = element_blank(),
      plot.background    = element_blank(),
      legend.position    = "none"
    )
  
  return(radar)
  
}



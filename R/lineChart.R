#' Plot a Line Chart following WJP style guidelines
#'
#' @description
#' `wjp_lines()` takes a data frame with a specific data structure (usually long shaped) and returns a ggplot
#' object with a line chart following WJP style guidelines.
#'
#' @param data Data frame containing the data to plot
#' @param target String. Column name of the variable that will supply the values to plot.
#' @param grouping String. Column name of the variable that supplies the grouping values (X-Axis).
#' @param ngroups Vector containing each of the groups for the lineas. If there is only a single group, please input c = (1).
#' @param labels String. Column name of the variable containing the value labels to display in plot.
#' @param colors String. Column name of the variable that contains the color grouping.
#' @param cvec Named vector with the colors to apply to each line.
#' @param repel Boolean. If TRUE, function will apply the ggrepel package to repel labels. Default is FALSE.
#' @param transparency Boolean. If TRUE, function will apply different opacities patterns. Default is FALSE.
#' @param transparencies Named vector with the different opacities to apply to each line.
#' @param custom.axis Boolean. If TRUE, x.breaks and x.labels will be passed to the ggplot theme. Default is FALSE.
#' @param x.breaks Numeric vector with custom breaks for the X-Axis.
#' @param x.labels Character vector with labels for the x-axis. It has to be the same length than x.breaks.
#' @param sec.ticks Numeric vector containing the minor breaks for the plot X-Axis.
#' @param ptheme ggplot theme function to apply to the plot. By default, function applies WJP_theme()
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' # Always load the WJP fonts if not passing a custom theme to function
#' wjp_fonts()
#' 
#' # Preparing data
#' data2plot <- gpp %>% 
#'   select(year, q1a, q1b, q1c) %>% 
#'   mutate(across(!year, 
#'          ~case_when(.x == 1 | .x == 2 ~ 0, .x == 3 | .x == 4 ~ 1))) %>% 
#'   group_by(year) %>%
#'   summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) %>% 
#'   mutate(across(!year, \(x) x*100)) %>%
#'   pivot_longer(!year, names_to = "group", values_to = "value2plot")
#'  
#'  # Plotting chart
#'  wjp_lines(data = data2plot, target = "value2plot", grouping = "year", colors = "group", ngroups = data2plot$group)
#'  }

wjp_lines <- function(
    data,                    
    target,             
    grouping,
    ngroups,                 
    labels         = NULL,
    colors,
    cvec           = NULL,
    repel          = F, 
    transparency   = F,        
    transparencies = NULL,   
    custom.axis    = F,         
    x.breaks       = NULL,    
    x.labels       = NULL,    
    sec.ticks      = NULL,       
    ptheme         = WJP_theme()
){
  
  # Renaming variables in the data frame to match the function naming
  if (is.null(labels)) {
    data <- data %>%
      dplyr::mutate(labels_var    = "") %>%
      rename(target_var    = all_of(target),
             grouping_var  = all_of(grouping),
             colors_var     = all_of(colors))
  } else {
    data <- data %>%
      rename(target_var    = all_of(target),
             grouping_var  = all_of(grouping),
             labels_var    = all_of(labels),
             colors_var    = all_of(colors))
  }
  
  # Creating ggplot
  plt <- ggplot(data, 
                aes(x     = grouping_var,
                    y     = target_var,
                    color = colors_var,
                    label = labels_var,
                    group = ngroups))
    
  if (transparency == F) {
    plt <- plt +
      geom_point(size = 2,
                 show.legend = F) +
      geom_line(linewidth    = 1,
                show.legend  = F)
      
  } else {
    plt <- plt +
      geom_point(size = 2,
                 aes(alpha   = colors_var),
                 show.legend = F) +
      geom_line(linewidth    = 1,
                aes(alpha    = colors_var),
                show.legend  = F) +
      scale_alpha_manual(values = transparencies)
  }
  
  if (repel == F) {
    
    # Applying regular geom_text
    plt <- plt +
      geom_text(aes(y     = target_var + 7.5,
                    x     = grouping_var,
                    label = labels_var),
                family      = "Lato Full",
                fontface    = "bold",
                size        = 3.514598,
                show.legend = F)
    
  } else {
    
    # Applying ggrepel for a better visualization of plots
    plt <- plt +
      geom_text_repel(mapping = aes(y     = target_var,
                                    x     = grouping_var,
                                    label = labels_var),
                      family      = "Lato Full",
                      fontface    = "bold",
                      size        = 3.514598,
                      show.legend = F,
                      
                      # Additional options from ggrepel package:
                      min.segment.length = 1000,
                      seed               = 42,
                      box.padding        = 0.5,
                      direction          = "y",
                      force              = 5,
                      force_pull         = 1)
      
  }
  
  # Continuing with ggplot  
  
  if (custom.axis == F) {
    plt <- plt +
      scale_y_continuous(limits = c(0, 105),
                         expand = c(0,0),
                         breaks = seq(0,100,20),
                         labels = paste0(seq(0,100,20), "%"))
    
    if (!is.null(cvec)) {
      plt <- plt +
        scale_color_manual(values = cvec)
    }
    
  } else {
    plt <- plt +
      scale_y_continuous(limits = c(0, 105),
                         expand = c(0,0),
                         breaks = seq(0,100,20),
                         labels = paste0(seq(0,100,20), "%")) +
      scale_x_continuous(breaks = x.breaks,
                         expand = expansion(mult = c(0.075, 0.125)),
                         labels = x.labels,
                         guide = "axis_minor",
                         minor_breaks = sec.ticks)
    
    if (!is.null(cvec)) {
      plt <- plt +
        scale_color_manual(values = cvec)
    }
  }
  
  plt <- plt +
    ptheme +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#d1cfd1"),
          axis.title.x       = element_blank(),
          axis.title.y       = element_blank(),
          axis.line.x        = element_line(color    = "#d1cfd1"),
          axis.ticks.x       = element_line(color    = "#d1cfd1",
                                            linetype = "solid"),
          ggh4x.axis.ticks.length.minor = rel(1))
  
  return(plt)
}

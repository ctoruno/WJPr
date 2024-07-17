#' Plot a Bar Chart following WJP style guidelines
#'
#' @description
#' `wjp_bars()` takes a data frame with a specific data structure (usually long shaped) and returns a ggplot
#' object with a bar chart following WJP style guidelines.
#'
#' @param data Data frame containing the data to plot
#' @param target String. Column name of the variable that will supply the values to plot.
#' @param grouping String. Column name of the variable that supplies the grouping values.
#' @param labels String. Column name of the variable containing the value labels to display in plot.
#' @param colors String. Column name of the variable that contains the color grouping.
#' @param cvec Named vector with the colors to apply to bars.
#' @param direction String. Should the bars be plotted in a "horizontal" or "vertical" way?
#' @param stacked Boolean. If TRUE, bars will be stacked on top of each other per group. Default is FALSE.
#' @param lab_pos String. Column name of the variable that contains the coordinates for the value labels.
#' @param expand Boolean. If TRUE, the plot will give extra space for value labels. Default is FALSE.
#' @param order_var String. Column name of the variable that contains the custom order for labels.
#' @param width Numeric value between 0 and 1. Width of bars as a percentage of the space for each bar.
#' @param ptheme ggplot theme function to apply to the plot. By default, function applies WJP_theme()
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' # Always load the WJP fonts if not passing a custom theme to function
#' wjp_fonts()
#' 
#' # Preparing data
#' data2plot <- gpp %>% 
#'   select(country, q1a, q1b, q1c) %>%
#'   mutate(dplyr::across(!country, 
#'                        ~case_when(.x == 1 | .x == 2 ~ 0, .x == 3 | .x == 4 ~ 1))) %>%
#'   group_by(country) %>%
#'   summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) %>% 
#'   mutate(across(!country, \(x) x*100)) %>%
#'   pivot_longer(!country, names_to = "group", values_to = "value2plot")
#' 
#' # Plotting chart
#' wjp_bars(data = data2plot, target = "value2plot", grouping = "country", colors = "group")
#' }

wjp_bars <- function(
    data,              
    target,        
    grouping,      
    labels     = NULL,        
    colors,        
    cvec       = NULL,            
    direction  = "vertical",         
    stacked    = F,       
    lab_pos    = NULL,    
    expand     = F,      
    order      = NULL,
    width      = 0.9,
    ptheme     = WJP_theme()
){
  
  # Renaming variables in the data frame to match the function naming
  if (is.null(labels)) {
    data <- data %>%
      dplyr::mutate(labels_var    = "") %>%
      dplyr::rename(target_var    = all_of(target),
                    grouping_var  = all_of(grouping),
                    colors_var    = all_of(colors),
                    lab_pos       = all_of(lab_pos),
                    order_var     = all_of(order))
  } else {
    
    data <- data %>%
      dplyr::rename(target_var    = all_of(target),
                    grouping_var  = all_of(grouping),
                    labels_var    = all_of(labels),
                    colors_var    = all_of(colors),
                    order_var     = all_of(order))
    
    if (is.null(lab_pos)){
      data <- data %>%
        dplyr::mutate(lab_pos = target_var)
    } else {
      data <- data %>%
        dplyr::rename(lab_pos = all_of(lab_pos))
    }
    
  }
  if (grouping == colors) {
    data <- data %>%
      dplyr::mutate(grouping_var = colors_var)
  }
  
  # Creating plot
  if(is.null(order)) {
    
    if (stacked == F) {
      plt <- ggplot(data, 
                    aes(x     = grouping_var,
                        y     = target_var,
                        label = labels_var,
                        fill  = colors_var)) +
        geom_bar(stat = "identity",
                 show.legend = F, width = width) +
        geom_text(aes(y    = lab_pos),
                  color    = "#4a4a49",
                  family   = "Lato Full",
                  fontface = "bold")
    } else {
      plt <- ggplot(data, 
                    aes(x     = grouping_var,
                        y     = target_var,
                        label = labels_var,
                        fill  = colors_var)) +
        geom_bar(stat         = "identity",
                 position     = "stack", 
                 show.legend  = F,  width = width) +
        geom_text(aes(y    = lab_pos),
                  color    = "#ffffff",
                  family   = "Lato Full",
                  fontface = "bold")
    }
    
  } else {
    
    if (stacked == F) {
      plt <- ggplot(data, 
                    aes(x     = reorder(grouping_var, order_var),
                        y     = target_var,
                        label = labels_var,
                        fill  = colors_var)) +
        geom_bar(stat = "identity",
                 show.legend = F,  width = width) +
        geom_text(aes(y    = target_var + lab_pos),
                  color    = "#4a4a49",
                  family   = "Lato Full",
                  fontface = "bold")
    } else {
      plt <- ggplot(data, 
                    aes(x     = reorder(grouping_var, order_var),
                        y     = target_var,
                        label = labels_var,
                        fill  = colors_var)) +
        geom_bar(stat         = "identity",
                 position     = "stack", 
                 show.legend  = F,  width = width) +
        geom_text(aes(y    = lab_pos),
                  color    = "#ffffff",
                  family   = "Lato Full",
                  fontface = "bold")
    }
  }
  
  plt <- plt +
    labs(y = "% of respondents")
  
  if (!is.null(cvec)) {
    plt <- plt +
      scale_fill_manual(values = cvec)
  }
  
  if (expand == F) {
    uplimitV = 100
    uplimitH = 105
  } else {
    uplimitV = 110
    uplimitH = 105
  }
  
  if (direction == "vertical") {
    plt  <- plt +
      scale_y_continuous(limits = c(0, uplimitV),
                         breaks = seq(0,100,20),
                         labels = paste0(seq(0,100,20), "%")) +
      ptheme +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "#D0D1D3"),
            axis.title.x       = element_blank())
  }
  
  if (direction == "horizontal") {
    plt  <- plt +
      scale_y_continuous(limits = c(0, uplimitH),
                         breaks = seq(0,100,20),
                         labels = paste0(seq(0,100,20), "%"),
                         position = "right") +
      scale_x_discrete(limits = rev) +
      coord_flip() +
      ptheme +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "#D0D1D3"),
            axis.title.y       = element_blank(),
            axis.title.x       = element_blank(),
            axis.text.y        = element_text(hjust = 0))
  }
    
  return(plt)
  
}

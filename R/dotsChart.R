#' Plot a Dots Chart following WJP style guidelines
#'
#' @description
#' `wjp_dotsChart()` takes a data frame with a specific data structure (usually long shaped) and returns a ggplot
#' object with a dots chart following WJP style guidelines.
#' 
#' @param data Data frame containing the data to plot
#' @param target String. Column name of the variable that will supply the values to plot.
#' @param grouping String. Column name of the variable that supplies the grouping values. The plot will show a different color per group.
#' @param labels String. Column name of the variable that supplies the Y-Axis labels to show in the plot.
#' @param cvec Named vector with the colors to apply to the dots. Default is NULL.
#' @param order String. Column name of the variable that contains the custom order for the labels.
#' @param diffOpac Boolean. If TRUE, the plot will expect different levels of opacities for the dots. Default is FALSE.
#' @param opacities Named vector with the opacity levels to apply to the dots. Default is NULL.
#' @param diffShp Boolean. If TRUE, the plot will expect different shapes for the dots. Default is FALSE.
#' @param shapes Named vector with shapes to be displayed. Default is NULL.
#' @param ptheme ggplot theme function to apply to the plot. By default, function applies WJP_theme().
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
#' select(country, q49a)%>%
#'   mutate(div = case_when(
#'     q49a == 1  ~ "Very confident",
#'     q49a == 2  ~ "Fairly confident",
#'     q49a == 3  ~ "Not very confident",
#'     q49a == 4  ~ "Not at all confident",
#'     q49a == 99 ~ NA_character_
#'   )
#'   )%>% drop_na()%>%
#'   group_by(country, div)%>%
#'   count()%>%
#'   group_by(country)%>%
#'   transmute(div, value2plot = n/sum(n)*100)%>%
#'   mutate(order_value = case_when(
#'     country == "Atlantis" ~ 1,
#'     country == "Narnia" ~ 2,
#'     country == "Neverland" ~ 3))
#' 
#' # Plotting chart
#' wjp_dotsChart(data= data2plot, target = "value2plot", grouping = "div", order = "order_value", labels = "country")
#' }
#' 
#' 

wjp_dots <- function(
    data,             
    target,      
    grouping,  
    labels,  
    cvec      = NULL, 
    order     = NULL,
    diffOpac  = F,  
    opacities = NULL,      
    diffShp   = F,     
    shapes    = NA,
    ptheme    = WJP_theme()
){
  
  # Renaming variables in the data frame to match the function naming
  if (is.null(order)){
    data <- data %>%
      rename(target_var    = all_of(target),
             grouping_var  = all_of(grouping),
             labels_var    = all_of(labels)) %>%
      group_by(grouping_var) %>%
      mutate(
        order_var = row_number()
      )
    
  } else {
    data <- data %>%
      rename(target_var    = all_of(target),
             grouping_var  = all_of(grouping),
             labels_var    = all_of(labels),
             order_var     = all_of(order))
  }
  
  # Creating a strip pattern
  strips <- data %>%
    group_by(labels_var) %>%
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
    
  
  # Creating ggplot
  plt <- ggplot() +
    geom_blank(data      = data,
               aes(x     = reorder(labels_var, -order_var),
                   y     = target_var,
                   label = labels_var,
                   color = grouping_var)) +
    geom_ribbon(data      = strips,
                aes(x     = x,
                    ymin  = ymin,
                    ymax  = ymax,
                    group = xposition,
                    fill  = fill),
                show.legend = F) +
    scale_fill_manual(values = c("grey"  = "#EBEBEB",
                                 "white"  = "#FFFFFF"),
                      na.value = NULL)
  
  if (diffShp == F) {
    
    if (diffOpac == F) {
      plt <- plt +
        geom_point(data      = data,
                   aes(x     = reorder(labels_var, -order_var),
                       y     = target_var,
                       color = grouping_var),
                   size = 4,
                   show.legend = F)
    } else {
      plt <- plt +
        geom_point(data = data,
                   aes(x     = reorder(labels_var, -order_var),
                       y     = target_var,
                       color = grouping_var,
                       alpha = grouping_var),
                   size      = 4,
                   show.legend   = F) +
        scale_alpha_manual(values = opacities)
    }
    
  } else {
    
    if (diffOpac == F) {
      plt <- plt +
        geom_point(data      = data,
                   aes(x     = reorder(labels_var, -order_var),
                       y     = target_var,
                       color = grouping_var,
                       shape = grouping_var),
                   fill   = NA,
                   size   = 4,
                   stroke = 2,
                   show.legend = F) +
        scale_shape_manual(values = shapes)
      
    } else {
      plt <- plt +
        geom_point(data = data,
                   aes(x     = reorder(labels_var, -order_var),
                       y     = target_var,
                       color = grouping_var,
                       shape = grouping_var,
                       alpha = grouping_var),
                   fill   = NA,
                   size   = 4,
                   stroke = 2,
                   show.legend    = F) +
        scale_shape_manual(values = shapes) +
        scale_alpha_manual(values = opacities)
    }
    
  }
  
  if (!is.null(cvec)){
    plt <- plt +
    scale_color_manual(values = cvec)
  }
  
  plt <- plt +
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
    
  return(plt)
  
}

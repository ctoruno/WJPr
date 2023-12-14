#' Plot a Diverging Horizontal Bar Chart following WJP style guidelines
#'
#' @description
#' `wjp_divbars()` takes a data frame with a specific data structure (usually long shaped) and returns a ggplot
#' object with a diverging horizontal bar chart following WJP style guidelines.
#' 
#' @param data Data frame containing the data to plot
#' @param target String. Column name of the variable that will supply the values to plot.
#' @param grouping String. Column name of the variable that supplies the grouping values (Y-Axis Labels).
#' @param diverging String. Column name of the variable that supplies the diverging values.
#' @param negative String. Value that indicates that the bar should be in the negative quadrant.
#' @param cvec Named vector with the colors to apply to each bar segment. Default is NULL.
#' @param labels String. Column name of the variable that supplies the labels to show in the plot. Default is NULL.
#' @param custom_order Boolean. If TRUE, the plot will expect a custom order of the graph labels. Default is FALSE.
#' @param order String. Vector that contains the custom order for the y-axis labels. Default is NULL.
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
#'   select(country, q49a)%>%
#'   mutate(div = case_when(
#'                    q49a == 1  ~ "Very confident",
#'                    q49a == 2  ~ "Fairly confident",
#'                    q49a == 3  ~ "Not very confident",
#'                    q49a == 4  ~ "Not at all confident",
#'                    q49a == 99 ~ NA_character_
#'                )
#'     )%>% drop_na()%>%
#'     group_by(country, div)%>%
#' count()%>%
#' group_by(country)%>%
#' transmute(div, value2plot = n/sum(n)*100)
#' 
#' data2plot$Label<- paste0(round(data2plot$value2plot,1), "%")
#' 
#' colors4plot<- c("lightpink1", "palevioletred1", "violet", "mediumpurple1")
#' names(colors4plot)<- c("Very confident", "Fairly confident", "Not very confident", "Not at all confident")
#' 
#' data2plot$value2plot<- ifelse(data2plot$div %in% c("Not very confident", "Not at all confident"), data2plot$value2plot*-1, data2plot$value2plot)
#' data2plot$div<- as.factor(data2plot$div)
#' data2plot$div<- factor(data2plot$div, levels = c("Not at all confident", "Not very confident", "Very confident", "Fairly confident"))
#' 
#' # Plotting chart
#' wjp_divbars(data = data2plot, target = "value2plot", grouping = "country", diverging = "div", cvec = colors4plot, labels = "Label")
#' }


wjp_divbars <- function(
    data,             
    target,       
    grouping,         
    diverging,     
    negative = NULL,   
    cvec = NULL,
    labels = NULL,       
    custom_order = F, 
    order = NULL,  
    ptheme= WJP_theme()
){
  
  # Renaming variables in the data frame to match the function naming
  if (!is.null(labels)) {
    data <- data %>%
      rename(target_var    = all_of(target),
             rows_var      = all_of(grouping),
             grouping_var  = all_of(diverging),
             labels_var    = all_of(labels),
             order_var     = any_of(order))
  } else{
  data <- data %>%
    rename(target_var    = all_of(target),
           rows_var      = all_of(grouping),
           grouping_var  = all_of(diverging),
           order_var     = any_of(order))
  data$labels_var <- rep("", nrow(data))
  }
  # Creating ggplot
  if (custom_order == F) {
    chart <- ggplot(data, aes(x     = rows_var,
                              y     = target_var,
                              fill  = grouping_var,
                              label = labels_var))
  } else {
    chart <- ggplot(data, aes(x     = reorder(rows_var, order_var),
                              y     = target_var,
                              fill  = grouping_var,
                              label = labels_var))
  }
  
  # Axis breaks
  brs <-  c(-100, -75, -50, -25, 0, 25, 50, 75, 100)
  
  # Adding geoms
  chart <- chart +
    geom_bar(stat        = "identity",
             position    = "stack",
             show.legend = F,
             width       = 0.85) +
    geom_hline(yintercept = 0,
               linetype   = "solid",
               size       = 0.5,
               color      = "#262424")
  
  if (!is.null(cvec)) {
    chart <- chart +
      scale_fill_manual(values = cvec)
  }
  
    chart <- chart +
    scale_y_continuous(limits   = c(-105,105),
                       breaks   = brs,
                       labels   = paste0(abs(brs), "%"),
                       position = "right") +
    scale_x_discrete(limits   = rev) +
    coord_flip() +
    WJP_theme() +
    geom_text(aes(label = labels_var), position=position_stack(vjust=0.5))+
    theme(panel.grid.major = element_blank(),
          axis.text.x      = element_text(family = "Lato Full",
                                          face   = "bold",
                                          size   = 3.514598*.pt,
                                          color  = "#262424",
                                          hjust  = 0),
          axis.text.y      = element_text(family = "Lato Full",
                                          face   = "bold",
                                          size   = 3.514598*.pt,
                                          color  = "#262424",
                                          hjust  = 0),
          axis.title.x      = element_blank(),
          axis.title.y      = element_blank(),
          axis.line.x       = element_line(linetype   = "solid",
                                           size       = 0.5,
                                           color      = "#262424"))
  
  
  return(chart)
  
}

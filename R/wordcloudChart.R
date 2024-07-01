#' Plot a Word Cloud Chart following WJP style guidelines
#'
#' @description
#' `wjp_wordCloud()` takes a data frame containing a bag of words and their frequencies and 
#'                   returns a ggplot object with a word cloud following WJP style guidelines.
#' @param df A dataframe containing the words and their frequencies.
#' @param word_col The name of the column containing the words to plot.
#' @param freq_col The name of the column containing word frequencies.
#' @param max_size Specifies the size of the largest word. By default, set to 40.
#' @param min_freq Specifies the minimum frequency of a word to be included in the cloud.
#'  @param cvec Named vector with the colors to apply to the words. Default is NULL.
#' @param ptheme ggplot theme function to apply to the plot. By default, function applies WJP_theme().
#'
#' @return A ggplot object
#' @export
#' 

library(ggplot2)
library(ggwordcloud)

wjp_wordcloud <- function(df, word_col, freq_col, min_freq = 2, cvec = NULL, ptheme = WJP_theme()) {
  # If no color vector is provided, use default colors
  if (is.null(cvec)) {
    cvec <- c("#320D6D", "#FFBFB7", "#FFD447", "#700353", "#4C1C00")
  }
  
  
  # filter out low frequency words
  df <- df[df[[freq_col]] >= min_freq, ]
  
  # Create the word cloud plot
  p <- ggplot(df, aes_string(label = word_col, size = freq_col, color = freq_col)) +
    geom_text_wordcloud_area() +
    scale_size_area(max_size = 30) +
    scale_color_gradientn(colors = cvec) +
    theme_minimal() +
    ptheme
  
  return(p)
}



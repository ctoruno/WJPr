wjp_fonts <- function(path2SP){
  path2fonts<- paste0(path2SP, "6. Country Reports/0. Fonts/")
  sysfonts::font_add(family     = "Lato Full",
                     regular    = paste0(path2fonts, "Lato-Regular.ttf"),
                     italic     = paste0(path2fonts, "Lato-LightItalic.ttf"),
                     bold       = paste0(path2fonts, "Lato-Bold.ttf"),
                     bolditalic = paste0(path2fonts, "Lato-BoldItalic.ttf"))
  sysfonts::font_add(family  = "Lato Light",
                     regular = paste0(path2fonts, "Lato-Light.ttf"))
  sysfonts::font_add(family  = "Lato Black",
                     regular = paste0(path2fonts, "Lato-Black.ttf"))
  sysfonts::font_add(family  = "Lato Black Italic",
                     regular = paste0(path2fonts, "Lato-BlackItalic.ttf"))
  sysfonts::font_add(family  = "Lato Medium",
                     regular = paste0(path2fonts, "Lato-Medium.ttf"))
  showtext::showtext_auto()
}


WJP_theme <- function() {
  theme(panel.background   = element_blank(),
        plot.background    = element_blank(),
        panel.grid.major   = element_line(size     = 0.25,
                                          colour   = "#5e5c5a",
                                          linetype = "dashed"),
        panel.grid.minor   = element_blank(),
        axis.title.y       = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C",
                                          margin   = margin(0, 10, 0, 0)),
        axis.title.x       = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C",
                                          margin   = margin(10, 0, 0, 0)),
        axis.text.y        = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C"),
        axis.text.x = element_text(family = "Lato Full",
                                   face   = "plain",
                                   size   = 3.514598*.pt,
                                   color  = "#524F4C"),
        axis.ticks  = element_blank(),
        plot.margin  = unit(c(0, 0, 0, 0), "points")
  ) 
}

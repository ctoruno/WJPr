wjp_fonts <- function(){
  sysfonts::font_add_google(
    "Lato",
    family     = "Lato Full",
    regular.wt = 400,
    bold.wt    = 700,
    repo       = "http://fonts.gstatic.com/",
    db_cache   = TRUE,
    handle     = curl::new_handle()
  )
  sysfonts::font_add_google(
    "Lato",
    family     = "Lato Light",
    regular.wt = 300,
    bold.wt    = 700,
    repo       = "http://fonts.gstatic.com/",
    db_cache   = TRUE,
    handle     = curl::new_handle()
  )
  sysfonts::font_add_google(
    "Lato",
    family     = "Lato Black",
    regular.wt = 900,
    bold.wt    = 900,
    repo       = "http://fonts.gstatic.com/",
    db_cache   = TRUE,
    handle     = curl::new_handle()
  )
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



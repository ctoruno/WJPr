## code to prepare `gpp` dataset

library(haven)
gpp <- read_dta("data-raw/gpp-sample.dta")
usethis::use_data(gpp, overwrite = TRUE)

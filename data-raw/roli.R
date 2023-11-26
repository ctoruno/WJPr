## code to prepare `roli` dataset goes here
library(readxl)
roli <- read_excel("data-raw/ROLI_data.xlsx")
usethis::use_data(roli, overwrite = TRUE)

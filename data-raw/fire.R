## code to prepare `fire` dataset goes here
fire <- read.csv("FIREDAM.csv", stringsAsFactors = FALSE)
usethis::use_data(fire, overwrite = TRUE)

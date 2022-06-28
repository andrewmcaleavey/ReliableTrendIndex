## code to prepare `mac_height` dataset goes here

mac_height <- data.frame(obs = c(98, 98, 98, 99, 99, 99), time = c(1:6))

usethis::use_data(mac_height, overwrite = TRUE)

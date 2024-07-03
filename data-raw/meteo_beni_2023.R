## code to prepare `meteo_beni_2023` dataset goes here

local({
  meteo_beni_2023 <- readxl::read_excel("data-raw/raw/meteo_beni_2023.xlsx")
  colnames(meteo_beni_2023)[1] = "Fecha"
  meteo_beni_2023$Date = lubridate::as_date(meteo_beni_2023$Fecha)
  usethis::use_data(meteo_beni_2023, overwrite = TRUE)
})



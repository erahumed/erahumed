## code to prepare `meteo_beni_2023` dataset goes here

local({
  meteo_beni_2023 <-
    readxl::read_excel("data-raw/raw/meteo_beni_2023.xlsx",
                       range = cellranger::cell_cols("A:I")
                       ) |>
    dplyr::rename(Date = FECHA) |>
    dplyr::mutate(Date = as.Date(Date))

  usethis::use_data(meteo_beni_2023, overwrite = TRUE)
})



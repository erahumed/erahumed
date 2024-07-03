## code to prepare `albufera_outflows` dataset goes here

local({
  CHJ <- readr::read_delim("data-raw/raw/CHJ.csv",
                           delim = ";",
                           escape_double = FALSE,
                           trim_ws = TRUE)

  chj_stations <- c("08A01" = "Level",
                    "08A02" = "Pujol",
                    "08A03" = "Perellonet",
                    "08A04" = "Perello")

  albufera_outflows <- CHJ |>
    dplyr::filter(INDI_CHJ %in% names(chj_stations)) |>
    dplyr::transmute(
      Date = Fecha,
      name = chj_stations[INDI_CHJ],
      value = ifelse(name == "Level", `Nivel (m.s.n.m)`, `Caudal (m³/s)`)
      ) |>
    dplyr::mutate(Date = as.Date(Date, format = "%d-%m-%Y")) |>
    dplyr::mutate(value = as.numeric(sub(",", ".", value))) |>
    dplyr::mutate(
      value = replace(value,
                      name != "Level" & lubridate::year(Date) == 2017,
                      NA)
      ) |>
    tidyr::pivot_wider(names_from = name, values_from = value)

  usethis::use_data(albufera_outflows, overwrite = TRUE)
})

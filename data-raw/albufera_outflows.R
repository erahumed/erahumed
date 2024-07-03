## code to prepare `albufera_outflows` dataset goes here

local({
  CHJ <- readr::read_delim("data-raw/raw/CHJ.csv",
                           delim = ";",
                           escape_double = FALSE,
                           trim_ws = TRUE)

  res <- CHJ |>
    dplyr::filter(INDI_CHJ %in% c("08A02", "08A03", "08A04")) |>
    dplyr::select(Fecha, `Caudal (m³/s)`, INDI_CHJ) |>
    tidyr::pivot_wider(names_from = INDI_CHJ, values_from = `Caudal (m³/s)`)

  level <- CHJ |>
    dplyr::filter(INDI_CHJ %in% "08A01") |>
    dplyr::select(Fecha, `Nivel (m.s.n.m)` , INDI_CHJ) |>
    tidyr::pivot_wider(names_from = INDI_CHJ, values_from = `Nivel (m.s.n.m)` )

  res <- merge(level, res, by= "Fecha", all = T)

  res$Fecha <- lubridate::dmy(sub(" 00:00:00", "", res$Fecha))

  names(res) <- c("Date", "Level", "Pujol", "Perellonet", "Perello")

  res$Level <- as.numeric(sub(",", ".", res$Level))

  res$Pujol <- as.numeric(sub(",", ".", res$Pujol))

  res$Perellonet <- as.numeric(sub(",", ".", res$Perellonet))

  res$Perello <- as.numeric(sub(",", ".", res$Perello))

  res = res |>
    dplyr::mutate(
      Pujol = replace(Pujol, lubridate::year(Date) == 2017, NA),
      Perellonet = replace (Perellonet, lubridate::year(Date) == 2017, NA),
      Perello = replace(Perello, lubridate::year(Date) == 2017, NA)
      )

  albufera_outflows <- res

  usethis::use_data(albufera_outflows, overwrite = TRUE)
})



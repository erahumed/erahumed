library(dplyr)

### Meteorological data ########################################################

meteo_beni_2023 <-
  readxl::read_excel("data-raw/raw/meteo_beni_2023.xlsx",
                     range = cellranger::cell_cols("A:I")
  ) |>
  rename(date = FECHA) |>
  mutate(date = as.Date(date))



### CHJ data ###################################################################

CHJ <- readr::read_delim("data-raw/raw/CHJ.csv",
                         delim = ";",
                         escape_double = FALSE,
                         trim_ws = TRUE)

chj_stations <- c("08A01" = "Level",
                  "08A02" = "Pujol",
                  "08A03" = "Perellonet",
                  "08A04" = "Perello")

albufera_outflows <- CHJ |>
  filter(INDI_CHJ %in% names(chj_stations)) |>
  transmute(
    date = Fecha,
    name = chj_stations[INDI_CHJ],
    value = ifelse(name == "Level", `Nivel (m.s.n.m)`, `Caudal (m³/s)`)
  ) |>
  mutate(date = as.Date(date, format = "%d-%m-%Y")) |>
  mutate(value = as.numeric(sub(",", ".", value))) |>
  mutate(
    value = replace(value,
                    name != "Level" & format(date, format = "%Y") == "2017",
                    NA)
  ) |>
  tidyr::pivot_wider(names_from = name, values_from = value)



### CHJ data imputation ########################################################

# Principal GAMs
gam_water_level <- readRDS("data-raw/raw/mod_water_level.rds")
gam_pujol <- readRDS("data-raw/raw/mod_pujol.rds")
gam_perellonet <- readRDS("data-raw/raw/mod_perellonet.rds")
gam_perello <- readRDS("data-raw/raw/mod_perello.rds")

# Single variable calibration GAMs
gam_corr_water_level_low <- readRDS("data-raw/raw/mod_corr_water_level_low.rds")
gam_corr_pujol_low <- readRDS("data-raw/raw/mod_corr_pujol_low.rds")
gam_corr_pujol_hi <- readRDS("data-raw/raw/mod_corr_pujol_hi.rds")
gam_corr_perellonet_hi <- readRDS("data-raw/raw/mod_corr_perellonet_hi.rds")

moving_average <- function(x, k) {
  stats::filter(x, rep(1 / k, k), sides = 1) |> as.numeric()
}

meteo_ext <- meteo_beni_2023 |>
  mutate(
    P_average7 = moving_average(P, 7),
    ETP_average15 = moving_average(ETP, 15),
    year = as.numeric(format(date, "%Y")),
    day = as.numeric(format(date, "%j"))
  )

albufera_outflows <- inner_join(albufera_outflows, meteo_ext, by = "date")

gam_recalibrate <- function(predicted, gam) {
  mgcv::predict.gam(gam, newdata = data.frame(predicted))
}

albufera_outflows <- albufera_outflows |>
  mutate(
    level_pred = mgcv::predict.gam(gam_water_level, albufera_outflows),
    level_pred = ifelse(level_pred < -0.25 & year < 2010,
                        gam_recalibrate(level_pred, gam_corr_water_level_low),
                        level_pred),
    level_is_imputed = is.na(Level),
    Level = ifelse(level_is_imputed, level_pred, Level)
  )

albufera_outflows <- albufera_outflows |>
  mutate(
    pujol_pred = mgcv::predict.gam(gam_pujol, albufera_outflows),
    pujol_pred = ifelse(pujol_pred < -1.5,
                        gam_recalibrate(pujol_pred, gam_corr_pujol_low),
                        pujol_pred),
    pujol_pred = ifelse(pujol_pred > 13,
                        gam_recalibrate(pujol_pred, gam_corr_pujol_hi),
                        pujol_pred),
    pujol_is_imputed = is.na(Pujol),
    Pujol = ifelse(pujol_is_imputed, pujol_pred, Pujol)
  )

albufera_outflows <- albufera_outflows |>
  mutate(
    perellonet_pred = mgcv::predict.gam(gam_perellonet, albufera_outflows),
    perellonet_pred = ifelse(perellonet_pred > 13,
                             gam_recalibrate(perellonet_pred, gam_corr_perellonet_hi),
                             perellonet_pred),
    perellonet_is_imputed = is.na(Perellonet),
    Perellonet = ifelse(perellonet_is_imputed, perellonet_pred, Perellonet)
  )

albufera_outflows <- albufera_outflows |>
  mutate(
    perello_pred = mgcv::predict.gam(gam_perello, albufera_outflows),
    perello_pred = ifelse(perello_pred < 0, 0, perello_pred),
    perello_is_imputed = is.na(Perello),
    Perello = ifelse(perello_is_imputed, perello_pred, Perello)
  )

albufera_outflows <-albufera_outflows |>
  rename_with(tolower) |>
  select(
    date,
    level,
    pujol,
    perellonet,
    perello,
    level_is_imputed,
    pujol_is_imputed,
    perellonet_is_imputed,
    perello_is_imputed
  )



### Lake inflow by ditch data ##################################################

months_es <- c(
  "Enero", "Febrero", "Marzo",
  "Abril", "Mayo", "Junio",
  "Julio", "Agosto", "Septiembre",
  "Octubre", "Noviembre", "Diciembre"
  )

ditch_inflow_pcts <- readRDS("data-raw/raw/pct_soria_obs.rds") |>
  rename(month = mes) |>
  mutate(month = match(month, months_es))

names(ditch_inflow_pcts) <- gsub("acq", "d", names(ditch_inflow_pcts))

ditch_inflow_pcts



### Exports ####################################################################

usethis::use_data(meteo_beni_2023, overwrite = TRUE)
usethis::use_data(albufera_outflows, overwrite = TRUE)
usethis::use_data(ditch_inflow_pcts, overwrite = TRUE)


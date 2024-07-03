library(dplyr)

################################################################################
# _____________________________________________________________________ CHJ data

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
    Date = Fecha,
    name = chj_stations[INDI_CHJ],
    value = ifelse(name == "Level", `Nivel (m.s.n.m)`, `Caudal (m³/s)`)
  ) |>
  mutate(Date = as.Date(Date, format = "%d-%m-%Y")) |>
  mutate(value = as.numeric(sub(",", ".", value))) |>
  mutate(
    value = replace(value,
                    name != "Level" & format(Date, format = "%Y") == "2017",
                    NA)
  ) |>
  tidyr::pivot_wider(names_from = name, values_from = value)



################################################################################
# ____________________________________________________ Lake inflow by ditch data

ditch_inflows <- readRDS("data-raw/raw/pct_soria_obs.rds")


################################################################################
# __________________________________________________________ Meteorological data

meteo_beni_2023 <-
  readxl::read_excel("data-raw/raw/meteo_beni_2023.xlsx",
                     range = cellranger::cell_cols("A:I")
  ) |>
  rename(Date = FECHA) |>
  mutate(Date = as.Date(Date))



################################################################################
# __________________________________________________________ CHJ data imputation

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
  #zoo::rollmean(x, k = k, fill = NA, align = "right")
}

albufera_outflows_imputed <-
  meteo_beni_2023 |>
  mutate(
    P_average7 = moving_average(P, 7),
    ETP_average15 = moving_average(ETP, 15),
    year = as.numeric(format(Date, "%Y")),
    day = as.numeric(format(Date, "%j"))
    ) |>
  inner_join(albufera_outflows, by = "Date")

gam_recalibrate <- function(predicted, gam) {
  mgcv::predict.gam(gam, newdata = data.frame(predicted))
}

albufera_outflows_imputed <- albufera_outflows_imputed |>
  mutate(
    LevelPred = mgcv::predict.gam(gam_water_level, albufera_outflows_imputed),
    LevelPred = ifelse(
      LevelPred < -0.25 & year < 2010,
      gam_recalibrate(LevelPred, gam_corr_water_level_low),
      LevelPred),
    obs_pred_level = is.na(Level),
    Level = ifelse(obs_pred_level, LevelPred, Level)
  )


albufera_outflows_imputed <- albufera_outflows_imputed |>
  mutate(
    PujolPred = mgcv::predict.gam(gam_pujol, albufera_outflows_imputed),
    PujolPred = ifelse(
      PujolPred < -1.5,
      gam_recalibrate(PujolPred, gam_corr_pujol_low),
      PujolPred),
    PujolPred = ifelse(
      PujolPred > 13,
      gam_recalibrate(PujolPred, gam_corr_pujol_hi),
      PujolPred),
    obs_pred_pujol = is.na(Pujol),
    Pujol = ifelse(obs_pred_pujol, PujolPred, Pujol)
  )

albufera_outflows_imputed <- albufera_outflows_imputed |>
  mutate(
    PerellonetPred = mgcv::predict.gam(gam_perellonet, albufera_outflows_imputed),
    PerellonetPred = ifelse(
      PerellonetPred > 13,
      gam_recalibrate(PerellonetPred, gam_corr_perellonet_hi),
      PerellonetPred
    ),
    obs_pred_perellonet = is.na(Perellonet),
    Perellonet = ifelse(obs_pred_perellonet, PerellonetPred, Perellonet)
  )

albufera_outflows_imputed <- albufera_outflows_imputed |>
  mutate(
    PerelloPred = mgcv::predict.gam(gam_perello, albufera_outflows_imputed),
    PerelloPred = ifelse(PerelloPred < 0, 0, PerelloPred),
    obs_pred_perello = is.na(Perello),
    Perello = ifelse(obs_pred_perello, PerelloPred, Perello)
  )

albufera_outflows_imputed <- albufera_outflows_imputed |>
  select(
    Date,
    Level, Pujol, Perellonet, Perello,
    obs_pred_level, obs_pred_pujol, obs_pred_perellonet, obs_pred_perello
  )



################################################################################
# ______________________________________________________________________ Exports

usethis::use_data(albufera_outflows, overwrite = TRUE)
usethis::use_data(albufera_outflows_imputed, overwrite = TRUE)
usethis::use_data(ditch_inflows, overwrite = TRUE)
usethis::use_data(meteo_beni_2023, overwrite = TRUE)

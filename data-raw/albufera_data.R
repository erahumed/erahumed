renv::use(
  bit         = "bit@4.5.0",
  bit64       = "bit64@4.5.2",
  cellranger  = "cellranger@1.1.0",
  cli         = "cli@3.6.3",
  clipr       = "clipr@0.8.0",
  cpp11       = "cpp11@0.5.0",
  crayon      = "crayon@1.5.3",
  dplyr       = "dplyr@1.1.4",
  fansi       = "fansi@1.0.6",
  generics    = "generics@0.1.3",
  glue        = "glue@1.8.0",
  hms         = "hms@1.1.3",
  lattice     = "lattice@0.22-6",
  lifecycle   = "lifecycle@1.0.4",
  magrittr    = "magrittr@2.0.3",
  Matrix      = "Matrix@1.7-0",
  mgcv        = "mgcv@1.9-1",
  nlme        = "nlme@3.1-164",
  pillar      = "pillar@1.9.0",
  pkgconfig   = "pkgconfig@2.0.3",
  prettyunits = "prettyunits@1.2.0",
  progress    = "progress@1.2.3",
  purrr       = "purrr@1.0.2",
  R6          = "R6@2.5.1",
  readr       = "readr@2.1.5",
  readxl      = "readxl@1.4.3",
  rematch     = "rematch@2.0.0",
  renv        = "renv@1.0.11",
  rlang       = "rlang@1.1.4",
  stringi     = "stringi@1.8.4",
  stringr     = "stringr@1.5.1",
  tibble      = "tibble@3.2.1",
  tidyr       = "tidyr@1.3.1",
  tidyselect  = "tidyselect@1.2.1",
  tzdb        = "tzdb@0.4.0",
  utf8        = "utf8@1.2.4",
  vctrs       = "vctrs@0.6.5",
  vroom       = "vroom@1.6.5",
  withr       = "withr@3.0.1"
)

library(dplyr)

### Meteorological data ########################################################

albufera_weather <-
  readxl::read_excel("data-raw/raw/meteo_beni_2023.xlsx",
                     range = cellranger::cell_cols("A:I")
  ) |>
  rename(date = FECHA) |>
  mutate(date = as.Date(date))

albufera_weather_gam_input <- albufera_weather

albufera_weather <- albufera_weather |>
  transmute(date,
            temperature_ave = TME,
            temperature_min = TMIN,
            temperature_max = TMAX,
            precipitation_mm = P,
            evapotranspiration_mm = ETP
            )



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

meteo_ext <- albufera_weather_gam_input |>
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
    is_imputed_level = is.na(Level),
    Level = ifelse(is_imputed_level, level_pred, Level)
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

albufera_outflows <- albufera_outflows |>
  rename_with(tolower) |>
  transmute(
    date,
    level,
    outflow_pujol = pujol,
    outflow_perellonet = perellonet,
    outflow_perello = perello,
    is_imputed_level = is_imputed_level,
    is_imputed_outflow =
      pujol_is_imputed | perello_is_imputed | perellonet_is_imputed
  )



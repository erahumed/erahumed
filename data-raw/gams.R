local({
  gams <- list()

  gams$mod_water_level <- readRDS("data-raw/raw/mod_water_level.rds")
  gams$mod_corr_water_level_low <- readRDS("data-raw/raw/mod_corr_water_level_low.rds")
  gams$mod_corr_pujol_low <- readRDS("data-raw/raw/mod_corr_pujol_low.rds")
  gams$mod_corr_pujol_hi <- readRDS("data-raw/raw/mod_corr_pujol_hi.rds")
  gams$mod_pujol <- readRDS("data-raw/raw/mod_pujol.rds")
  gams$mod_perellonet <- readRDS("data-raw/raw/mod_perellonet.rds")
  gams$mod_corr_perellonet_hi <- readRDS("data-raw/raw/mod_corr_perellonet_hi.rds")
  gams$mod_perello <- readRDS("data-raw/raw/mod_perello.rds")

  usethis::use_data(gams, internal = TRUE)
})

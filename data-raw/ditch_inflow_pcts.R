local({
  pct_soria_obs <- readRDS("data-raw/raw/pct_soria_obs.rds")
  usethis::use_data(ditch_inflows, overwrite = TRUE)
})


ct_get_param <- function(chemical, parameter, chemical_db) {
  chemical_db [[ chemical ]] [[ parameter ]]
}

ct_fds <- function(pos, kd_cm3_g, bd_g_cm3) {
  # Fraction of chemical residues in dissolved form and within voids in the
  # sediment
  return( pos / (pos + (kd_cm3_g * bd_g_cm3)) )
}

ct_fdw <- function(kd_cm3_g, css_ppm) {
  # Fraction of chemical dissolved in water
  return( 1 / (1 + ppm_to_g_cm3(css_ppm) * kd_cm3_g) )
}

ct_kdifus_m_day <- function(pos, MW) {
  69.35 / 365 - pos * ((MW)^(-2/3))
}

ct_cover <- function(seed_day, jgrow, covmax) {
  igrow <- seed_day |> pmax2(0) |> pmin2(jgrow)
  cover <- covmax * (igrow / jgrow)
}

ct_is_empty <- function(height_m, thresh_m) {
  height_m < thresh_m
}

ct_setl <- function(ksetl_m_day, fpw, height_sod_m) {
  tol <- 1e-4  # 0.1mm to trigger settlement
  ifelse(height_sod_m > tol, ksetl_m_day * fpw / height_sod_m, 0)
}

ct_diff_s <- function(kdifus_m_day, fds, pos, dact_m) {
  kdifus_m_day * (fds / pos) / dact_m
}

ct_diff_w <-  function(kdifus_m_day, fdw, height_sod_m) {
  tol <- 1e-4  # 0.1mm to trigger diffusion
  ifelse(height_sod_m > tol, kdifus_m_day * fdw / height_sod_m, 0)
}

ct_temperature_arrhenius <- function(temperature_ave,
                                     temperature_min,
                                     temperature_max) {
  return(temperature_ave)
}

ct_deg_k <- function(k_ref, Q10, temperature, temperature_ref) {
  exp <- (temperature - temperature_ref) / 10
  fac <- Q10 ^ exp
  return(k_ref * fac)
}

ct_washout <- function(fet_cm, rain_cm) {
  fet_cm * rain_cm
}

ct_outflow_fac <- function(volume_eod_m3, outflow_m3) {
  tol <- 1e-4 # 0.1mm to trigger outflow
  ifelse(outflow_m3 + volume_eod_m3 > tol,
         outflow_m3 / (outflow_m3 + volume_eod_m3),
         0)
}

ct_mfapp <- function(application_kg, cover) {
  application_kg * cover
}

ct_mwapp <- function(application_kg, cover, is_empty) {
  application_kg * (1 - cover) * (!is_empty)
}

ct_msapp <- function(application_kg, cover, is_empty)
{
  application_kg * (1 - cover) * is_empty
}

ct_mw_max <- function(sol_ppm, volume_eod_m3) {
  ppm_to_kg_m3(sol_ppm) * volume_eod_m3
}

ct_total_inflow_m3 <- function(inflows_m3) {
  Reduce("+", inflows_m3, init = 0)
}

ct_mw_inflow_kg <- function(inflows_m3, inflows_densities_kg_m3) {
  lapply(seq_along(inflows_m3), function(i) {
    vol <- inflows_m3[[i]]
    dens <- inflows_densities_kg_m3[[i]]
    dens <- ifelse(!is.na(dens), dens, 0)
    vol * dens
    }) |>
    Reduce("+", x = _, init = 0)
}

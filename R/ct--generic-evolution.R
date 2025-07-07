ct_time_series <- function(
    application_kg,
    precipitation_mm,
    etp_mm,
    temperature_ave,
    temperature_min,
    temperature_max,
    volume_eod_m3,
    outflow_m3_s,
    inflows_m3_s,
    inflows_densities_kg_m3,
    area_m2,
    seed_day,
    harvesting,
    simulation,
    chemical_id
    )
{
  chemical_db <- get_etc(simulation, "chemical_db")

  volume_eps <- 1e-6 # Threshold below which densities are reported as NA

  drift <- get_input(simulation, "drift")
  covmax <- get_input(simulation, "covmax")
  jgrow <- get_input(simulation, "jgrow")
  dact_m <- get_input(simulation, "dact_m")
  css_ppm <- get_input(simulation, "css_ppm")
  foc <- get_input(simulation, "foc")
  bd_g_cm3 <- get_input(simulation, "bd_g_cm3")
  qseep_m_day <- get_input(simulation, "qseep_m_day")
  porosity <- get_input(simulation, "porosity")

  # Chemical input properties
  kd_cm3_g <- foc * ct_get_param(chemical_id, "koc_cm3_g", chemical_db)
  kf_day <- ct_get_param(chemical_id, "kf_day", chemical_db)
  kw_day <- ct_get_param(chemical_id, "kw_day", chemical_db)
  Q10_kw <- ct_get_param(chemical_id, "Q10_kw", chemical_db)
  kw_temp <- ct_get_param(chemical_id, "kw_temp", chemical_db)
  ks_sat_day <- ct_get_param(chemical_id, "ks_sat_day", chemical_db)
  Q10_ks_sat <- ct_get_param(chemical_id, "Q10_ks_sat", chemical_db)
  ks_sat_temp <- ct_get_param(chemical_id, "ks_sat_temp", chemical_db)
  ks_unsat_day <- ct_get_param(chemical_id, "ks_unsat_day", chemical_db)
  Q10_ks_unsat <- ct_get_param(chemical_id, "Q10_ks_unsat", chemical_db)
  ks_unsat_temp <- ct_get_param(chemical_id, "ks_unsat_temp", chemical_db)
  sol_ppm <- ct_get_param(chemical_id, "sol_ppm", chemical_db)
  dinc_m <- ct_get_param(chemical_id, "dinc_m", chemical_db)
  ksetl_m_day <- ct_get_param(chemical_id, "ksetl_m_day", chemical_db)
  kvolat_m_day <- ct_get_param(chemical_id, "kvolat_m_day", chemical_db)
  MW <- ct_get_param(chemical_id, "MW", chemical_db)
  fet_cm <- ct_get_param(chemical_id, "fet_cm", chemical_db)

  # Chemical derived properties
  fds <- ct_fds(pos = porosity, kd_cm3_g = kd_cm3_g, bd_g_cm3 = bd_g_cm3)
  fdw <- ct_fdw(kd_cm3_g = kd_cm3_g, css_ppm = css_ppm)
  fpw <- 1 - fdw
  kdifus_m_day <- ct_kdifus_m_day(pos = porosity, MW = MW)

  terms <- ct_ts_step_terms(application_kg = application_kg,
                            precipitation_mm = precipitation_mm,
                            etp_mm = etp_mm,
                            temperature_ave = temperature_ave,
                            temperature_min = temperature_min,
                            temperature_max = temperature_max,
                            volume_eod_m3 = volume_eod_m3,
                            outflow_m3_s = outflow_m3_s,
                            inflows_m3_s = inflows_m3_s,
                            inflows_densities_kg_m3 = inflows_densities_kg_m3,
                            area_m2 = area_m2,
                            seed_day = seed_day,
                            harvesting = harvesting,
                            drift = drift,
                            covmax = covmax,
                            jgrow = jgrow,
                            dact_m = dact_m,
                            css_ppm = css_ppm,
                            foc = foc,
                            bd_g_cm3 = bd_g_cm3,
                            qseep_m_day = qseep_m_day,
                            porosity = porosity,
                            kd_cm3_g = kd_cm3_g,
                            kf_day = kf_day,
                            kw_day = kw_day,
                            Q10_kw = Q10_kw,
                            kw_temp = kw_temp,
                            ks_sat_day = ks_sat_day,
                            Q10_ks_sat = Q10_ks_sat,
                            ks_sat_temp = ks_sat_temp,
                            ks_unsat_day = ks_unsat_day,
                            Q10_ks_unsat = Q10_ks_unsat,
                            ks_unsat_temp = ks_unsat_temp,
                            sol_ppm = sol_ppm,
                            dinc_m = dinc_m,
                            ksetl_m_day = ksetl_m_day,
                            kvolat_m_day = kvolat_m_day,
                            MW = MW,
                            fet_cm = fet_cm,
                            fds = fds,
                            fdw = fdw,
                            fpw = fpw,
                            kdifus_m_day = kdifus_m_day
                            )

  eAww <- terms[["eAww"]]
  eAws <- terms[["eAws"]]
  eAsw <- terms[["eAsw"]]
  eAss <- terms[["eAss"]]
  mf <- terms[["mf"]]
  qw <- terms[["qw"]]
  qs <- terms[["qs"]]
  mwapp <- terms[["mwapp"]]
  msapp <- terms[["msapp"]]

  mw_max <- terms[["mw_max"]]

  volume_sod_m3 <- terms[["volume_sod_m3"]]
  outflow_m3 <- terms[["outflow_m3"]]
  outflow_fac <- terms[["outflow_fac"]]

  n_time_steps <- length(outflow_m3_s)
  mw <- ms <- mw_outflow <- numeric(n_time_steps)
  for (t in 2:n_time_steps) {
    mw[t] <- eAww[t]*mw[t-1] + eAws[t]*ms[t-1] + qw[t]*mf[t-1]
    ms[t] <- eAsw[t]*mw[t-1] + eAss[t]*ms[t-1] + qs[t]*mf[t-1]

    mw_outflow[t] <- outflow_fac[t]*mw[t]
    mw[t] <- mw[t] - mw_outflow[t]

    mw[t] <- mw[t] + mwapp[t]
    ms[t] <- ms[t] + msapp[t]

    mw_excess <- mw[t] - mw_max[t]

    if (mw_excess > 0) {
      mw[t] <- mw_max[t]
      ms[t] <- ms[t] + mw_excess
    }
  }

  cw <- ifelse(volume_sod_m3 > volume_eps, mw / volume_sod_m3, NA)
  cs <- ms / (dact_m * area_m2)
  cw_outflow <- ifelse(outflow_m3 > volume_eps, mw_outflow / outflow_m3, NA)

  return(list(mf_kg = mf,
              mw_kg = mw,
              ms_kg = ms,
              mw_outflow_kg = mw_outflow,
              cw_kg_m3 = cw,
              cs_kg_m3 = cs,
              cs_g_kg = cs / bd_g_cm3,
              cw_outflow_kg_m3 = cw_outflow,
              volume_m3 = volume_sod_m3,
              outflow_m3 = outflow_m3)
         )
}

ct_ts_step_terms <- function(application_kg,
                             precipitation_mm,
                             etp_mm,
                             temperature_ave,
                             temperature_min,
                             temperature_max,
                             volume_eod_m3,
                             outflow_m3_s,
                             inflows_m3_s,
                             inflows_densities_kg_m3,
                             area_m2,
                             seed_day,
                             harvesting,
                             drift,
                             covmax,
                             jgrow,
                             dact_m,
                             css_ppm,
                             foc,
                             bd_g_cm3,
                             qseep_m_day,
                             porosity,
                             kd_cm3_g,
                             kf_day,
                             kw_day,
                             Q10_kw,
                             kw_temp,
                             ks_sat_day,
                             Q10_ks_sat,
                             ks_sat_temp,
                             ks_unsat_day,
                             Q10_ks_unsat,
                             ks_unsat_temp,
                             sol_ppm,
                             dinc_m,
                             ksetl_m_day,
                             kvolat_m_day,
                             MW,
                             fet_cm,
                             fds,
                             fdw,
                             fpw,
                             kdifus_m_day
                             )
{
  n_time_steps <- length(outflow_m3_s)  # Guaranteed to be of required length
  dt <- 1

  temp_arr <- ct_temperature_arrhenius(temperature_ave,
                                        temperature_min,
                                        temperature_max)

  # Hydro balance time series
  height_eod_m <- volume_eod_m3 / area_m2
  height_eod_cm <- height_eod_m * 100
  outflow_m3 <- outflow_m3_s * s_per_day()
  inflow_m3 <- ct_total_inflow_m3_s(inflows_m3_s) * s_per_day()
  rain_cm <- precipitation_mm / 10
  rain_m3 <- (precipitation_mm / 1000) * area_m2
  etp_m3 <- (etp_mm / 1000) * area_m2
  volume_sod_m3 <- c(NA_real_, volume_eod_m3[-n_time_steps])
  height_sod_m <- volume_sod_m3 / area_m2

  # Precomputed time series
  cover <- ct_cover(seed_day = seed_day, jgrow = jgrow, covmax = covmax)
  is_empty <- ct_is_empty(height_m = height_eod_m, thresh_m = 0.0001)


  ### Settlement
  Sw <- ct_setl(ksetl_m_day = ksetl_m_day, fpw = fpw, height_sod_m = height_sod_m)

  ### Diffusion
  Ds <- ct_diff_s(kdifus_m_day = kdifus_m_day, fds = fds, pos = porosity, dact_m = dact_m)
  Dw <- ct_diff_w(kdifus_m_day = kdifus_m_day, fdw = fdw, height_sod_m = height_sod_m)

  ### Degradation (applying Arrhenius kinetic equilibrium)
  kf <- kf_day
  kw <- ct_deg_k(kw_day, Q10_kw, temp_arr, kw_temp)

  ks_sat <- ct_deg_k(ks_sat_day, Q10_ks_sat, temp_arr, ks_sat_temp)
  ks_unsat <- ct_deg_k(ks_unsat_day, Q10_ks_unsat, temp_arr, ks_unsat_temp)
  ks <- (1-is_empty) * ks_sat + is_empty * ks_unsat
  ### Washout
  w <- ct_washout(fet_cm = fet_cm, rain_cm = rain_cm)

  ### Outflow
  outflow_fac <- ct_outflow_fac(volume_eod_m3 = volume_eod_m3, outflow_m3 = outflow_m3)

  ### Inflow
  mw_inflow_kg <- ct_mw_inflow_kg(inflows_m3_s, inflows_densities_kg_m3)

  ### Application
  mfapp <- ct_mfapp(application_kg, drift, cover)
  mwapp <- ct_mwapp(application_kg, drift, cover, is_empty) + mw_inflow_kg
  msapp <- ct_msapp(application_kg, drift, cover, is_empty)


  mw_max <- ct_mw_max(sol_ppm = sol_ppm, volume_eod_m3 = volume_eod_m3)

  a <- -(kw + Sw + Dw)
  b <- Ds
  c <- Dw + Sw
  d <- -(ks + Ds)
  u <- -(kf + w)
  v <- w


  log_decay_factor_f <- cumsum(u)
  mf <- numeric(n_time_steps)
  application_days_idxs <- which(mfapp != 0)
  harvesting_days_idxs <- which(harvesting)
  for (i in application_days_idxs) {
    f <- log_decay_factor_f - log_decay_factor_f[i]
    fac <- exp(f)
    fac[seq_len(i-1)] <- 0

    j <- harvesting_days_idxs[harvesting_days_idxs > i][1]
    if (!is.na(j)) {
      fac[j:length(fac)] <- 0
    }

    mf <- mf + mfapp[i] * fac
  }

  eA <- exp2by2(a = a, b = b, c = c, d = d)
  iC <- inv2by2(a = a - u, b = b, c = c, d = d - u)
  q1 <- (eA$E11 - exp(u))*iC$I11 + eA$E12*iC$I21
  q2 <- eA$E21*iC$I11 + (eA$E22-exp(u))*iC$I21
  q1 <- q1 * v
  q2 <- q2 * v

  res <- list(
    # Homogeneous term for linear layer of evolution
    eAww = eA$E11,
    eAws = eA$E12,
    eAsw = eA$E21,
    eAss = eA$E22,
    qw = q1,
    qs = q2,

    # Inhomogeneous term for linear layer of evolution
    mf = mf,
    mwapp = mwapp,
    msapp = msapp,

    # Threshold for mass in water compartment
    mw_max = mw_max,

    volume_sod_m3 = volume_sod_m3,
    outflow_m3 = outflow_m3,
    outflow_fac = outflow_fac
  )

  return(res)
}

ct_to_cluster_wrap <- function(cluster_ca_df,
                               drift,
                               covmax,
                               jgrow,
                               SNK,
                               dact_m,
                               css_ppm,
                               bd_g_cm3,
                               qseep_m_day,
                               wilting,
                               fc
)
{
  res_template <- list(
    cluster_id = cluster_ca_df[["cluster_id"]],
    date = cluster_ca_df[["date"]]
  )

  compute_masses <- function(chemical) {
    ct_to_cluster(
      application_kg = cluster_ca_df[[chemical]],
      precipitation_mm = cluster_ca_df[["precipitation_mm"]],
      etp_mm = cluster_ca_df[["evapotranspiration_mm"]],
      temperature_ave = cluster_ca_df[["temperature_ave"]],
      temperature_min = cluster_ca_df[["temperature_min"]],
      temperature_max = cluster_ca_df[["temperature_max"]],
      height_eod_cm = cluster_ca_df[["real_height_cm"]],
      outflow_m3_s = cluster_ca_df[["real_outflow_m3_s"]],
      inflow_m3_s = cluster_ca_df[["real_inflow_m3_s"]],
      area_m2 = cluster_ca_df[["area_m2"]][[1]],
      seed_day = cluster_ca_df[["seed_day"]],
      chemical = chemical,
      drift = drift,
      covmax = covmax,
      jgrow = jgrow,
      SNK = SNK,
      dact_m = dact_m,
      css_ppm = css_ppm,
      bd_g_cm3 = bd_g_cm3,
      qseep_m_day = qseep_m_day,
      wilting = wilting,
      fc = fc
    )
  }

  chemicals <- unique(erahumed::albufera_ca_schedules$chemical)
  chemicals <- names(cluster_ca_df)[names(cluster_ca_df) %in% chemicals]

  lapply(chemicals, \(chemical)
         c(res_template, list(chemical = chemical), compute_masses(chemical))
         ) |>
    data.table::rbindlist() |>
    as.data.frame()
}

ct_to_cluster <- function(application_kg,
                          precipitation_mm,
                          etp_mm,
                          temperature_ave,
                          temperature_min,
                          temperature_max,
                          height_eod_cm,
                          outflow_m3_s,
                          inflow_m3_s,
                          area_m2,
                          seed_day,
                          chemical,
                          drift,
                          covmax,
                          jgrow,
                          SNK,
                          dact_m,
                          css_ppm,
                          bd_g_cm3,
                          qseep_m_day,
                          wilting,
                          fc
)
{
  volume_eps <- 1e-6 # Threshold below which densities are reported as NA

  # Magic:
  # * Pass down all arguments to ct_compute_system_terms()
  # * Export all the elements of the returned list to execution env
  match.call() |>
    as.list() |>
    (\(x) x[-1])() |>
    lapply(eval, envir = parent.frame()) |>
    do.call(ct_compute_system_terms, args = _) |>
    list2env(environment())

  n_time_steps <- length(application_kg)
  mw <- ms <- mw_outflow <- numeric(n_time_steps)
  for (t in 2:n_time_steps) {
    mw[t] <- Awf[t]*mf[t-1] + Aww[t]*mw[t-1] + Aws[t]*ms[t-1]
    ms[t] <-                + Asw[t]*mw[t-1] + Ass[t]*ms[t-1]

    # At this stage:
    # mw[t] = outflow_fac * mw_before = mw_before - mw_outflow
    mw_outflow[t] <- mw[t] * (1 / outflow_fac[t] - 1)

    mw[t] <- mw[t] + bw[t]
    ms[t] <- ms[t] + bs[t]

    mw_excess <- mw[t] - mw_max[t]
    if (mw_excess > 0) {
      mw[t] <- mw_max[t]
      ms[t] <- ms[t] + mw_excess
    }
  }


  cw <- ifelse(volume_sod_m3 > volume_eps, mw / volume_sod_m3, NA)
  cs <- ms / (dact_m * area_m2)
  cw_outflow <- ifelse(outflow_m3 > volume_eps, mw_outflow / outflow_m3, NA)

  return(list(mf = mf, mw = mw, ms = ms, cw = cw, cs = cs, cw_outflow = cw_outflow))
}

ct_compute_system_terms <- function(application_kg,
                                    precipitation_mm,
                                    etp_mm,
                                    temperature_ave,
                                    temperature_min,
                                    temperature_max,
                                    height_eod_cm,
                                    outflow_m3_s,
                                    inflow_m3_s,
                                    area_m2,
                                    seed_day,
                                    chemical,
                                    drift,
                                    covmax,
                                    jgrow,
                                    SNK,
                                    dact_m,
                                    css_ppm,
                                    bd_g_cm3,
                                    qseep_m_day,
                                    wilting,
                                    fc
                                    )
{
  n_time_steps <- length(application_kg)
  dt <- 1

  # Chemicals parameters
  kf_day <- ct_get_param(chemical, "kf_day")
  kw_day <- ct_get_param(chemical, "kw_day")
  Q10_kw <- ct_get_param(chemical, "Q10_kw")
  kw_temp <- ct_get_param(chemical, "kw_temp")
  ks_sat_day <- ct_get_param(chemical, "ks_sat_day")
  Q10_ks_sat <- ct_get_param(chemical, "Q10_ks_sat")
  ks_sat_temp <- ct_get_param(chemical, "ks_sat_temp")
  ks_unsat_day <- ct_get_param(chemical, "ks_unsat_day")
  Q10_ks_unsat <- ct_get_param(chemical, "Q10_ks_unsat")
  ks_unsat_temp <- ct_get_param(chemical, "ks_unsat_temp")
  sol_ppm <- ct_get_param(chemical, "sol_ppm")
  dinc_m <- ct_get_param(chemical, "dinc_m")
  kd_cm3_g <- ct_get_param(chemical, "kd_cm3_g")
  ksetl_m_day <- ct_get_param(chemical, "ksetl_m_day")
  kvolat_m_day <- ct_get_param(chemical, "kvolat_m_day")
  MW <- ct_get_param(chemical, "MW")
  fet_cm <- ct_get_param(chemical, "fet_cm")

  # Derived parameters
  pos <- ct_porosity(fc = fc, wilting = wilting)
  fds <- ct_fds(pos = pos, kd_cm3_g = kd_cm3_g, bd_g_cm3 = bd_g_cm3)
  fdw <- ct_fdw(kd_cm3_g = kd_cm3_g, css_ppm = css_ppm)
  fpw <- 1 - fdw
  kdifus_m_day <- ct_kdifus_m_day(pos = pos, MW = MW)
  temperature_arrhenius <- ct_temperature_arrhenius(temperature_ave,
                                                    temperature_min,
                                                    temperature_max)

  # Hydro balance time series
  height_eod_m <- height_eod_cm / 100
  volume_eod_m3 <- height_eod_m * area_m2
  outflow_m3 <- outflow_m3_s * s_per_day()
  inflow_m3 <- inflow_m3_s * s_per_day()
  rain_cm <- precipitation_mm / 10
  rain_m3 <- (precipitation_mm / 1000) * area_m2
  etp_m3 <- (etp_mm / 1000) * area_m2
  volume_sod_m3 <- volume_eod_m3 - inflow_m3 + outflow_m3 - rain_m3 + etp_m3
  height_sod_m <- volume_sod_m3 / area_m2

  # Precomputed time series
  cover <- ct_cover(seed_day = seed_day, jgrow = jgrow, covmax = covmax)
  is_empty <- ct_is_empty(height_m = height_eod_m, thresh_m = 0)


  # Processes

  ### Settlement
  setl <- ct_setl_fac(ksetl_m_day = ksetl_m_day,
                      fpw = fpw,
                      height_sod_m = height_sod_m)

  ### Diffusion
  diff_s <- ct_diff_s(kdifus_m_day = kdifus_m_day,
                      fds = fds,
                      pos = pos,
                      dact_m = dact_m)
  diff_w <- ct_diff_w(kdifus_m_day = kdifus_m_day,
                      fdw = fdw,
                      height_sod_m = height_sod_m)

  ### Degradation (applying Arrhenius kinetic equilibrium)
  kw_day <- ct_deg_k(kw_day,
                     Q10_kw, temperature_arrhenius, kw_temp)
  ks_sat_day <- ct_deg_k(ks_sat_day,
                         Q10_ks_sat, temperature_arrhenius, ks_sat_temp)
  ks_unsat_day <- ct_deg_k(ks_unsat_day,
                           Q10_ks_unsat, temperature_arrhenius, ks_unsat_temp)
  ks_day <- (1-is_empty) * ks_sat_day + is_empty * ks_unsat_day
  deg_f <- ct_deg_fac(k = kf_day, dt = dt)
  # deg_w <- ct_deg_fac(k = kw_day, dt = dt)
  # deg_s <- ct_deg_fac(k = ks_day, dt = dt)

  ws_mat <- exp2by2(
    a = -kw_day-setl-diff_w, b = diff_s,
    c = diff_w + setl      , d = -ks_day -diff_s
  )

  ### Washout
  washout_fac <- fet_cm * rain_cm

  ### Inflow
  inflow_mw <- inflow_m3 * 0  # not implemented ATM!

  ### Outflow
  outflow_fac <- ct_outflow_fac(volume_eod_m3 = volume_eod_m3,
                                outflow_m3 = outflow_m3)

  ### Application
  mfapp <- ct_mfapp(application_kg, drift, cover)
  mwapp <- ct_mwapp(application_kg, drift, cover, SNK, is_empty)
  msapp <- ct_msapp(application_kg, drift, cover, SNK, is_empty, dinc_m, dact_m)

  k_decay_f <- kf_day + washout_fac
  log_decay_factor_f <- -cumsum(kf_day + washout_fac)
  mf <- numeric(n_time_steps)
  for (i in which(mfapp != 0)) {
    f <- log_decay_factor_f - log_decay_factor_f[i]
    fac <- exp(f)
    fac[seq_len(i-1)] <- 0
    mf <- mf + mfapp[i] * fac
  }

  # Solubility
  mw_max <- ct_mw_max(sol_ppm = sol_ppm, volume_eod_m3 = volume_eod_m3)



  res <- list(
    # Homogeneous term for linear component of evolution
    Aff = deg_f * washout_fac,
    Afw = 0,
    Afs = 0,
    Awf = (washout_fac/k_decay_f) * (1 - exp(-k_decay_f)),
    #Aww = outflow_fac * deg_w * ((1-diff_w)*(1-setl) + diff_s*setl),
    Aww = outflow_fac * ws_mat$E11,
    #Aws = outflow_fac * deg_w * diff_s,
    Aws = outflow_fac * ws_mat$E12,
    Asf = 0,
    #Asw = deg_s * ((1-setl)*diff_w + setl*(1-diff_s)),
    Asw = ws_mat$E21,
    #Ass = deg_s * (1-diff_s),
    Ass = ws_mat$E22,
    # Inhomogeneous term for linear component of evolution
    mf = mf,
    bf = mfapp,
    bw = mwapp,
    bs = msapp,

    # Threshold for mass in water compartment
    mw_max = mw_max,

    volume_sod_m3 = volume_sod_m3,
    outflow_m3 = outflow_m3,
    outflow_fac = outflow_fac
  )

  return(res)
}



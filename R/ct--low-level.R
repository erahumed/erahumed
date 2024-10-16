ct_to_cluster_wrap <- function(cluster_ca_df,
                               drift,
                               covmax,
                               jgrow,
                               SNK,
                               dact,
                               css,
                               bd,
                               qseep,
                               wilting,
                               fc
)
{
  res <- data.frame(
    cluster_id = cluster_ca_df[["cluster_id"]],
    date = cluster_ca_df[["date"]]
  )

  chemicals <- unique(erahumed::albufera_ca_schedules$chemical)
  chemicals <- names(cluster_ca_df)[names(cluster_ca_df) %in% chemicals]
  for (chemical in chemicals) {
    names <- paste(chemical, c("(F)", "(W)", "(S)"))
    masses <- ct_to_cluster(
      application_kg = cluster_ca_df[[chemical]],
      rain_mm = cluster_ca_df[["rain_mm"]],
      etp_mm = cluster_ca_df[["evapotranspiration_mm"]],
      temperature = rep(20, nrow(cluster_ca_df)),
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
      dact = dact,
      css = css,
      bd = bd,
      qseep = qseep,
      wilting = wilting,
      fc = fc
    )
    res[[ names[[1]] ]] <- masses[[1]]
    res[[ names[[2]] ]] <- masses[[2]]
    res[[ names[[3]] ]] <- masses[[3]]
  }


  return(res)
}

ct_to_cluster <- function(application_kg,
                          rain_mm,
                          etp_mm,
                          temperature,
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
                          dact,
                          css,
                          bd,
                          qseep,
                          wilting,
                          fc
)
{
  terms <- ct_compute_system_terms(application_kg,
                                   rain_mm,
                                   etp_mm,
                                   temperature,
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
                                   dact,
                                   css,
                                   bd,
                                   qseep,
                                   wilting,
                                   fc)

  Aff <- terms$Aff
  Afw <- terms$Afw
  Afs <- terms$Afs
  Awf <- terms$Awf
  Aww <- terms$Aww
  Aws <- terms$Aws
  Asf <- terms$Asf
  Asw <- terms$Asw
  Ass <- terms$Ass

  bf <- terms$bf
  bw <- terms$bw
  bs <- terms$bs

  mw_max <- terms$mw_max

  n_time_steps <- length(mw_max)
  mf <- mw <- ms <- numeric(n_time_steps)
  for (t in 2:n_time_steps) {
    mf[t] <- bf[t] + Aff[t]*mf[t-1]
    mw[t] <- bw[t] + Awf[t]*mf[t-1] + Aww[t]*mw[t-1] + Aws[t]*ms[t-1]
    ms[t] <- bs[t]                  + Asw[t]*mw[t-1] + Ass[t]*ms[t-1]

    mw_excess <- mw[t] - mw_max[t]
    if (mw_excess > 0) {
      mw[t] <- mw_max[t]
      ms[t] <- ms[t] + mw_excess
    }
  }

  return(list(mf = mf, mw = mw, ms = ms))
}

ct_compute_system_terms <- function(application_kg,
                                    rain_mm,
                                    etp_mm,
                                    temperature,
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
                                    dact,
                                    css,
                                    bd,
                                    qseep,
                                    wilting,
                                    fc
                                    )
{
  n_time_steps <- length(application_kg)
  dt <- 1

  # Chemicals parameters
  kf <- ct_get_param(chemical, "kf")
  kw <- ct_get_param(chemical, "kw")
  Q10_kw <- ct_get_param(chemical, "Q10_kw")
  kw_temp <- ct_get_param(chemical, "kw_temp")
  ks_sat <- ct_get_param(chemical, "ks_sat")
  Q10_ks_sat <- ct_get_param(chemical, "Q10_ks_sat")
  ks_sat_temp <- ct_get_param(chemical, "ks_sat_temp")
  ks_unsat <- ct_get_param(chemical, "ks_unsat")
  Q10_ks_unsat <- ct_get_param(chemical, "Q10_ks_unsat")
  ks_unsat_temp <- ct_get_param(chemical, "ks_unsat_temp")
  sol <- ct_get_param(chemical, "sol")
  dinc <- ct_get_param(chemical, "dinc")
  kd <- ct_get_param(chemical, "kd")
  ksetl <- ct_get_param(chemical, "ksetl")
  kvolat <- ct_get_param(chemical, "kvolat")
  MW <- ct_get_param(chemical, "MW")
  fet <- ct_get_param(chemical, "fet")

  # Derived parameters
  pos <- fc - wilting
  vsed <- area_m2 * dact
  fds <- pos / (pos + (kd * bd))
  fdw <- 1 / (1 + kd * css)
  fpw <- (kd * css) / (1 + kd * css)  # Porcentaje pesticida particulado en agua
  kdifus <- 69.35 / 365 - pos * ((MW)^(-2/3))  # metros / dia

  # Hydro balance time series
  height_eod_m <- height_eod_cm / 100
  volume_eod_m3 <- height_eod_m * area_m2
  outflow_m3 <- outflow_m3_s * s_per_day()
  inflow_m3 <- inflow_m3_s * s_per_day()
  rain_cm <- rain_mm / 10
  rain_m3 <- (rain_mm / 1000) * area_m2
  etp_m3 <- (etp_mm / 1000) * area_m2
  volume_sod_m3 <- volume_eod_m3 - inflow_m3 + outflow_m3 - rain_m3 + etp_m3
  height_sod_m <- volume_sod_m3 / area_m2


  # Precomputed time series
  igrow <- seed_day |> pmax2(0) |> pmin2(jgrow)
  cover <- covmax * (igrow / jgrow)
  is_empty <- height_eod_m == 0

  ### Settlement
  setl <- ksetl * fpw / pmax2(height_sod_m, ksetl * fpw)

  ### Diffusion
  diff_s <- kdifus * area_m2 * (fds / pos) / vsed
  diff_w <- kdifus * area_m2 * fdw / pmax2(volume_sod_m3, kdifus * area_m2 * fdw)

  ### Degradation (applying Arrhenius kinetic equilibrium)
  kw <- kw * (Q10_kw ^ ((temperature - kw_temp) / 10))
  ks_sat <- ks_sat * (Q10_ks_sat ^ ((temperature - ks_sat_temp) / 10))
  ks_unsat <- ks_unsat * (Q10_ks_unsat ^ ((temperature - ks_unsat_temp) / 10))
  ks <- (1-is_empty) * ks_sat + is_empty * ks_unsat
  deg_f <- exp(-kf * dt)
  deg_w <- exp(-kw * dt)
  deg_s <- exp(-ks * dt)

  ### Washout
  washout_fac <- 1 - exp(-fet * rain_cm * dt)

  ### Outflow
  outflow_fac <- ifelse(volume_eod_m3 > 0 & outflow_m3 > 0,
                        volume_eod_m3 / (outflow_m3 + volume_eod_m3),
                        1)

  ### Inflow
  inflow_mw <- inflow_m3 * 0  # not implemented ATM!

  ### Application
  m_app_kg <- application_kg * (1 - drift)
  mfapp <- m_app_kg * cover
  mwapp <- m_app_kg * (1 - cover) * (1 - SNK) * (!is_empty)
  msapp <- m_app_kg * (1 - cover) * (1 - SNK) * (dinc / dact) * is_empty

  res <- list()

  # Homogeneous term for linear component of evolution
  res$Aff <- deg_f * washout_fac
  res$Afw <- 0
  res$Afs <- 0
  res$Awf <- deg_f * (1 - washout_fac)
  res$Aww <- outflow_fac * deg_w * ((1-diff_w)*(1-setl) + diff_s*setl)
  res$Aws <- outflow_fac * deg_w * diff_s
  res$Asf <- 0
  res$Asw <- deg_s * ((1-setl)*diff_w + setl*(1-diff_s))
  res$Ass <- deg_s * (1-diff_s)

  # Inhomogeneous term for linear component of evolution
  res$bf <- mfapp
  res$bw <- mwapp
  res$bs <- msapp

  # Threshold for mass in water compartment
  res$mw_max <- volume_eod_m3 * sol

  return(res)
}

ct_get_param <- function(chemical, parameter) {
  albufera_ct_parameters [[ chemical ]] [[ parameter ]]
}

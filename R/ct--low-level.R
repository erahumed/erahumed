ct_to_cluster_wrap <- function(cluster_ca_df)
{
  res <- data.frame(
    cluster_id = cluster_ca_df[["cluster_id"]],
    date = cluster_ca_df[["date"]]
    )

  chemicals <- unique(albufera_ca_schedules$chemical)
  chemicals <- names(cluster_ca_df)[names(cluster_ca_df) %in% chemicals]
  for (chemical in chemicals)
    res <- cbind(res,
                 ct_to_cluster(
                   application_kg = cluster_ca_df[[chemical]],
                   rain_mm = cluster_ca_df[["rain_mm"]],
                   etp_mm = cluster_ca_df[["evapotranspiration_mm"]],
                   temperature = rnorm(nrow(cluster_ca_df)),
                   height_eod_cm = cluster_ca_df[["real_height_cm"]],
                   outflow_m3_s = cluster_ca_df[["real_outflow_m3_s"]],
                   inflow_m3_s = cluster_ca_df[["real_inflow_m3_s"]],
                   area_m2 = cluster_ca_df[["area_m2"]][[1]],
                   seed_day = cluster_ca_df[["seed_day"]],
                   chemical = chemical
                 )
                )

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
                          chemical)
{
  n_time_steps <- length(application_kg)

  height_eod_m <- height_eod_cm / 100
  volume_eod_m3 <- height_eod_m * area_m2
  outflow_m3 <- outflow_m3_s * s_per_day()
  inflow_m3 <- inflow_m3_s * s_per_day()
  rain_cm <- rain_mm / 10
  rain_m3 <- (rain_mm / 1000) * area_m2
  etp_m3 <- (etp_mm / 1000) * area_m2

  drift <- 0        # I    Fracción perdida por deriva
  covmax <- 0.5     # I    Cobertura máxima del cultivo
  jgrow <- 152      # I    Días totales entre emergencia y maduración
  sa <- 114881.779  # C    Superficie del arrozal en m2
  kf <- 0.24        # P    Tasa de degradación en el follaje (día-1)
  kw <- 0.046       # P    Tasa de degradación en el agua (día-1)
  Q10_kw <- 2.6     # P
  temp_kw <- 20     # P
  ks_sat <- 0.03    # P    Tasa de degradación en el sedimento (día-1)
  Q10_ks_sat <- 2.6 # P
  temp_ks_sat <- 20 # P
  ks_unsat <- 0.117 # P
  Q10_ks_unsat <-  2.6  # P
  temp_ks_unsat <- 20  # P
  sol <- 408   # P     # Solubilidad
  dt <- 1  # I        # Intervalo de tiempo (día)
  SNK <- 0   # I (seguramente lo descartemos)     # Efecto de sumidero (0 para no usar)
  dinc <- 0.05  # P  # Profundidad de incorporación (m)
  dact <- 0.1  # I     # Profundidad de la capa activa de sedimento (m)
  kd <- 15    # P      # Coeficiente de partición agua-sedimento (cm3/g)
  css <- 50 * 1e-6   # I  # Concentración de sedimento suspendido (g/cm3)
  ksetl <- 2   # P   # Velocidad de sedimentación (m/día)
  vbind <- 0.01  # I   # Coeficiente de partición directa (cm/día)
  bd <- 1.5  # I    # Densidad aparente del sedimento (g/cm3)
  kvolat <- 0.0005  # P    # Tasa de volatilización (m/día)
  qseep <- 0  # I (seguramente omitir)     # Tasa de filtración (m/día)
  wilting <- 0.24  # I    #wilting point
  fc <- 0.35   # I       #field capcity
  MW <- 1  # P        # Peso molecular del químico
  fet <- 0.2  # P         # Washoff Fraction per rain cm - Foliar Extraction T...
  kmonod <- 1e-8  # I (seguramente omitiremos) monod feedback function https://stackoverflow.com/questions/56614347/non-negative-ode-solutions-with-functools-in-r/56692927#56692927

  # Funciones de parametros
  pos <- fc - wilting
  vsed <- sa * dact
  fds <- pos / (pos + (kd * bd))
  fdw <- 1 / (1 + kd * css)
  fpw <- (kd * css) / (1 + kd * css)  # Porcentaje pesticida particulado en agua
  kdifus <- 69.35 / 365 - pos * ((MW)^(-2/3))  # metros / dia


  # TODO: We should use a threshold here, but likely not the same used in the
  # previous modeling steps.
  volume_sod_m3 <- volume_eod_m3 - inflow_m3 + outflow_m3 - rain_m3 + etp_m3
  height_sod_m <- volume_sod_m3 / area_m2


  # Precomputed time series
  igrow <- seed_day |> pmax2(0) |> pmin2(jgrow)
  cover <- covmax * (igrow / jgrow)

  is_empty <- height_eod_m == 0

  m_app_kg <- application_kg * (1 - drift)
  mfapp <- m_app_kg * cover
  mwapp <- m_app_kg * (1 - cover) * (1 - SNK) * (!is_empty)
  msapp <- m_app_kg * (1 - cover) * (1 - SNK) * (dinc / dact) * is_empty

  setl <- ksetl * fpw / pmax2(height_sod_m, ksetl * fpw)

  diff_s <- kdifus * sa * (fds / pos) / vsed
  diff_w <- kdifus * sa * fdw / pmax2(volume_sod_m3, kdifus * sa * fdw)

  # Degradation (applying Arrhenius kinetic equilibrium)
  kw <- kw * (Q10_kw ^ ((temperature - temp_kw) / 10))
  ks_sat <- ks_sat * (Q10_ks_sat ^ ((temperature - temp_ks_sat) / 10))
  ks_unsat <- ks_unsat * (Q10_ks_unsat ^ ((temperature - temp_ks_unsat) / 10))
  ks <- (1-is_empty) * ks_sat + is_empty * ks_unsat
  deg_f <- exp(-kf * dt)
  deg_w <- exp(-kw * dt)
  deg_s <- exp(-ks * dt)

  washout_fac <- 1 - exp(-fet * rain_cm * dt)

  outflow_fac <- ifelse(volume_eod_m3 > 0 & outflow_m3 > 0,
                        volume_eod_m3 / (outflow_m3 + volume_eod_m3),
                        1)

  inflow_mw <- inflow_m3 * 0  # not implemented ATM!

  Aff <- deg_f * washout_fac
  Awf <- deg_f * (1 - washout_fac)
  Aww <- outflow_fac * deg_w * ((1-diff_w)*(1-setl) + diff_s*setl)
  Asw <- deg_s * ((1-setl)*diff_w + setl*(1-diff_s))
  Aws <- outflow_fac * deg_w * diff_s
  Ass <- deg_s * (1-diff_s)

  bf <- mfapp
  bw <- mwapp
  bs <- msapp

  mw_max <- volume_eod_m3 * sol

  mf <- numeric(n_time_steps)
  mw <- numeric(n_time_steps)
  ms <- numeric(n_time_steps)

  for (t in 2:n_time_steps) {
    # # Omitted for performance improvement
    # # mf[t] <- mf[t-1]
    # # mw[t] <- mw[t-1]
    # # ms[t] <- ms[t-1]
    #
    # # Sedimentation vectorized over all time steps
    # # (first occurrences of mw and ms, taking values from t-1)
    # msetl <- setl_fac[t] * mw[t-1]
    # mw[t] <- mw[t-1] - msetl
    # ms[t] <- ms[t-1] + msetl
    #
    # # Diffusion
    # mdifus <- difus_fac_ms * ms[t] + difus_fac_mw[t] * mw[t]
    # mw[t] <- mw[t] + mdifus
    # ms[t] <- ms[t] - mdifus
    #
    # # Degradation
    # # (first occurrence of mf, taking value from t-1)
    # mf[t] <- deg_fac_mf * mf[t-1]
    # mw[t] <- deg_fac_mw[t] * mw[t]
    # ms[t] <- deg_fac_ms[t] * ms[t]
    #
    # # Washout
    # mwashout <- washout_fac[t] * mf[t]
    # mf[t] <- mf[t] - mwashout
    # mw[t] <- mw[t] + mwashout
    #
    # # Outflow
    # mw[t] <- outflow_fac[t] * mw[t]
    #
    # # Inflow
    # mw[t] <- mw[t] + inflow_mw[t]
    #

    mf[t] <- Aff[t]*mf[t-1]
    mw[t] <- Awf[t]*mf[t-1] + Aww[t]*mw[t-1] + Aws[t]*ms[t-1]
    ms[t] <-                  Asw[t]*mw[t-1] + Ass[t]*ms[t-1]

    # Application
    mf[t] <- mf[t] + bf[t]
    mw[t] <- mw[t] + bw[t]
    ms[t] <- ms[t] + bf[t]

    mw_excess <- mw[t] - mw_max[t]
    if (mw_excess > 0) {
      mw[t] <- mw_max[t]
      ms[t] <- ms[t] + mw_excess
    }


  }

  res <- data.frame(mf = mf, mw = mw, ms = ms)
  names(res) <- paste0(chemical, " (", c("F", "W", "S"), ")")
  return(res)
}

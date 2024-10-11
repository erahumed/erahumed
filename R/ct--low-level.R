ct_to_cluster_wrap <- function(cluster_ca_df)
{
  res <- data.frame(date = cluster_ca_df[["date"]])

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

  # TODO: check this smoothing is OK.
  setl_fac <- ksetl * fpw / pmax2(height_sod_m, ksetl * fpw)

  difus_fac_ms <- kdifus * sa * (fds / pos) / vsed
  difus_fac_mw <- -kdifus * sa * fdw / pmax2(volume_sod_m3, kdifus * sa * fdw)

  # Degradation (applying Arrhenius kinetic equilibrium)
  kw <- kw * (Q10_kw ^ ((temperature - temp_kw) / 10))
  ks_sat <- ks_sat * (Q10_ks_sat ^ ((temperature - temp_ks_sat) / 10))
  ks_unsat <- ks_unsat * (Q10_ks_unsat ^ ((temperature - temp_ks_unsat) / 10))
  ks <- (1-is_empty) * ks_sat + is_empty * ks_unsat

  deg_fac_mf <- exp(-kf * dt)
  deg_fac_mw <- exp(-kw * dt)
  deg_fac_ms <- exp(-ks * dt)

  washout_fac <- 1 - exp(-fet * rain_cm * dt)

  outflow_fac <- ifelse(volume_eod_m3 > 0 & outflow_m3 > 0,
                        volume_eod_m3 / (outflow_m3 + volume_eod_m3),
                        1)

  inflow_mw <- inflow_m3 * 0  # not implemented ATM!

  mw_max <- volume_eod_m3 * sol


  mf <- numeric(n_time_steps)
  mw <- numeric(n_time_steps)
  ms <- numeric(n_time_steps)

  for (t in 2:n_time_steps) {
    mf[t] <- mf[t-1]
    mw[t] <- mw[t-1]
    ms[t] <- ms[t-1]

    # Sedimentation vectorized over all time steps
    msetl <- setl_fac[t] * mw[t]
    mw[t] <- mw[t] - msetl
    ms[t] <- ms[t] + msetl

    # Diffusion
    mdifus <- difus_fac_ms * ms[t] + difus_fac_mw[t] * mw[t]
    mw[t] <- mw[t] + mdifus
    ms[t] <- ms[t] - mdifus

    # Degradation
    mf[t] <- deg_fac_mf * mf[t]
    mw[t] <- deg_fac_mw[t] * mw[t]
    ms[t] <- deg_fac_ms[t] * ms[t]

    # Washout
    mwashout <- washout_fac[t] * mf[t]
    mf[t] <- mf[t] - mwashout
    mw[t] <- mw[t] + mwashout

    # Outflow
    mw[t] <- outflow_fac[t] * mw[t]

    # Inflow
    mw[t] <- mw[t] + inflow_mw[t]

    # Application
    mf[t] <- mf[t] + mfapp[t]
    mw[t] <- mw[t] + mwapp[t]
    ms[t] <- ms[t] + msapp[t]

    mw_excess <- mw[t] - mw_max[t]
    if (mw_excess > 0) {
      mw[t] <- mw_max[t]
      ms[t] <- ms[t] + mw_excess
    }


  }

  res <- data.frame(mf, mw, ms)
  names(res) <- paste0(chemical, " (", c("F", "W", "S"), ")")
  return(res)
}

ct_evolution_model <- function(area_m2,
                               rain_mm,
                               etp_mm,
                               temperature,
                               height_cm,
                               volume_m3,
                               outflow_m3_s,
                               inflow_m3_s,
                               application_kg,
                               seed_day)
{
  height_m <- height_cm / 100
  volume_m3 <- height_m * area_m2
  outflow_m3 <- outflow_m3_s * s_per_day()
  inflow_m3 <- inflow_m3_s * s_per_day()
  rain_cm <- rain_mm / 10
  rain_m3 <- (rain_cm / 100) * area_m2
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
  kd <- 2.1    # P      # Coeficiente de partición agua-sedimento (m3/mg)
  css <- 50    # I     # Concentración de sedimento suspendido (mg/m3)
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
  eps <- 1e-10

  # Funciones de parametros
  pos <- fc - wilting
  vsed <- sa * dact
  fds <- pos / (pos + (kd * bd))
  fdw <- 1 / (1 + kd * css)
  kdifus <- 69.35 / 365 - pos * ((MW)^(-2/3))  # metros / dia
  fpw <- (kd * css) / (1 + kd * css)  # Porcentaje pesticida particulado en agua

  # Precomputed time series
  igrow <- seed_day |> pmax2(0) |> pmin2(jgrow)
  cover <- covmax * (igrow / jgrow)
  # TODO: We should use a threshold here, but likely not the same used in the
  # previous modeling steps.
  is_empty <- height_m == 0

  m_app_kg <- application_kg * (1 - drift)
  mfapp <- m_app_kg * cover
  mwapp <- m_app_kg * (1 - cover) * (1 - SNK) * (!is_empty)
  msapp <- m_app_kg * (1 - cover) * (1 - SNK) * (dinc / dact) * is_empty

  vol_t <- c(NA, volume_m3)
  vol_t_petp <- vol_t + c(NA, rain_m3)
  vol_tp1 <- vol_t_petp + c(NA, inflow_m3 - outflow_m3)

  # TODO: check this smoothing is OK.
  setl_fac <- ksetl * fpw * height_m / ((height_m + eps)^2)

  difus_fac_ms <- kdifus * sa * (fds / pos) / vsed
  difus_fac_mw <- -kdifus * sa * fdw * vol_t / ((vol_t + eps)^2)

  # Degradation (applying Arrhenius kinetic equilibrium)
  kw <- kw * (Q10_kw ^ ((temperature - temp_kw) / 10))
  ks_sat <- ks_sat * (Q10_ks_sat ^ ((temperature - temp_ks_sat) / 10))
  ks_unsat <- ks_unsat * (Q10_ks_unsat ^ ((temperature - temp_ks_unsat) / 10))
  ks <- (1-is_empty) * ks_sat + is_empty * ks_unsat

  deg_fac_mf <- exp(-kf * dt)
  deg_fac_mw <- exp(-kw * dt)
  deg_fac_ms <- exp(-ks * dt)

  washout_fac <- 1 - exp(-fet * rain_cm * dt)

  outflow_fac <- 1 - c(outflow_m3, NA) / vol_t_petp

  inflow_mw <- inflow_m3 * 0  # not implemented ATM!

  mw_max <- vol_tp1 * sol

  res <- function(t, state)
  {

    msetl <- setl_fac[[t]] * state[[2]]
    state[[2]] <- state[[2]] - msetl
    state[[3]] <- state[[3]] + msetl

    # Diffusion
    # TODO: deal with vol_t == 0
    mdifus <- difus_fac_ms * state[[3]] + difus_fac_mw[[t]] * state[[2]]
    state[[2]] <- state[[2]] + mdifus
    state[[3]] <- state[[3]] - mdifus

    # Degradation
    state[[1]] <- deg_fac_mf * state[[1]]
    state[[2]] <- deg_fac_mw[[t]] * state[[2]]
    state[[3]] <- deg_fac_ms[[t]] * state[[3]]

    # Washout
    mwashout <- washout_fac[[t]] * state[[1]]
    state[[1]] <- state[[1]] - mwashout
    state[[2]] <- state[[2]] + mwashout

    # Outflow
    state[[2]] <- outflow_fac[[t]] * state[[2]]

    # Inflow
    state[[2]] <- state[[2]] + inflow_mw[[t]]

    # Application
    state[[1]] <- state[[1]] + mfapp[[t]]
    state[[2]] <- state[[2]] + mwapp[[t]]
    state[[3]] <- state[[3]] + msapp[[t]]

    # Solubility
    mw_excess <- state[[2]] - mw_max[[t]]
    mw_excess <- if (mw_excess < 0) 0 else mw_excess
    state[[2]] <- state[[2]] - mw_excess
    state[[3]] <- state[[3]] + mw_excess

    return(state)
  }

  return(res)
}


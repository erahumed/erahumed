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
                   height_cm = cluster_ca_df[["real_height_cm"]],
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
                          height_cm,
                          outflow_m3_s,
                          inflow_m3_s,
                          area_m2,
                          seed_day,
                          chemical)
{
  ode_model <- get_ode_model(area_m2 = area_m2,
                             rain_mm = rain_mm,
                             etp_mm = etp_mm,
                             temperature = temperature,
                             height_cm = height_cm,
                             volume_m3 = volume_m3,
                             outflow_m3_s = outflow_m3_s,
                             inflow_m3_s = inflow_m3_s,
                             application_kg = application_kg,
                             seed_day = seed_day)

  initial_state <- c(mf = 0, mw = 0, ms = 0)

  n_time_steps <- length(application_kg)
  res <- ode_solve(func = ode_model, n_time_steps = n_time_steps)

  names(res) <- paste0(chemical, " (", c("F", "W", "S"), ")")
  return(res)
}

get_ode_model <- function(area_m2,
                          rain_mm,
                          etp_mm,
                          temperature,
                          height_cm,
                          volume_m3,
                          outflow_m3_s,
                          inflow_m3_s,
                          application_kg,
                          seed_day
                          )
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

  f <- function(t, mf, mw, ms)
  {

    msetl <- setl_fac[[t]] * mw
    mw <- mw - msetl
    ms <- ms + msetl

    # Diffusion
    # TODO: deal with vol_t == 0
    mdifus <- difus_fac_ms * ms + difus_fac_mw[[t]] * mw
    mw <- mw + mdifus
    ms <- ms - mdifus

    # Degradation
    mf <- deg_fac_mf * mf
    mw <- deg_fac_mw[[t]] * mw
    ms <- deg_fac_ms[[t]] * ms

    # Washout
    mwashout <- washout_fac[[t]] * mf
    mf <- mf - mwashout
    mw <- mw + mwashout

    # Outflow
    mw <- outflow_fac[[t]] * mw

    # Inflow
    mw <- mw + inflow_mw[[t]]

    # Application
    mf <- mf + mfapp[[t]]
    mw <- mw + mwapp[[t]]
    ms <- ms + msapp[[t]]

    return(c(mf, mw, ms))
  }

  f
}


euler_base <- function(func, n_time_steps = n_time_steps) {
  # Preallocate space for solution
  mf <- numeric(n_time_steps)
  mw <- numeric(n_time_steps)
  ms <- numeric(n_time_steps)

  # Euler's method loop
  for (t in 2:n_time_steps) {
    new_m <- func(t = t, mf = mf[[t-1]], mw = mw[[t-1]], ms = ms[[t-1]])
    mf[[t]] <- new_m[[1]]
    mw[[t]] <- new_m[[2]]
    ms[[t]] <- new_m[[3]]
  }

  # Return the results as a data frame of states
  return( data.frame(mf = mf, mw = mw, ms = ms) )
}

ode_solve <- euler_base

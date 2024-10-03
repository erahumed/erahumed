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
                   temperature = rnorm(nrow(cluster_ca_df)),
                   height_cm = cluster_ca_df[["real_height_cm"]],
                   outflow_m3_s = cluster_ca_df[["real_outflow_m3_s"]],
                   area_m2 = cluster_ca_df[["area_m2"]][[1]],
                   seed_day = cluster_ca_df[["seed_day"]],
                   chemical = chemical
                 )
                 )

  return(res)
}

ct_to_cluster <- function(application_kg,
                          rain_mm,
                          temperature,
                          height_cm,
                          outflow_m3_s,
                          area_m2,
                          seed_day,
                          chemical)
{
  height_m <- height_cm / 100
  volume_m3 <- height_m * area_m2
  outflow_m3 <- outflow_m3_s * s_per_day()
  rain_cm <- rain_mm / 10

  ode_model <- get_ode_model(rain_cm = rain_cm,
                             temperature = temperature,
                             height_m = height_m,
                             volume_m3 = volume_m3,
                             outflow_m3 = outflow_m3,
                             application_kg = application_kg,
                             seed_day = seed_day)

  initial_state <- c(mf = 0, mw = 0, ms = 0)
  times <- seq_along(application_kg)

  res <- ode_solve(y = initial_state, times =  times, func = ode_model)
  names(res) <- paste0(chemical, " (", c("F", "W", "S"), ")")
  return(res)
}

get_ode_model <- function(rain_cm,
                          temperature,
                          height_m,
                          volume_m3,
                          outflow_m3,
                          application_kg,
                          seed_day
                          ) {

  drift <- 0  # I     # Fracción perdida por deriva
  covmax <- 0.5  # I   # Cobertura máxima del cultivo
  jgrow <- 152    #I    Días totales entre emergencia y maduración
  sa <- 114881.779  # C    # Superficie del arrozal en m2
  kf <- 0.24     # P      Tasa de degradación en el follaje (día-1)
  kw <- 0.046     # P    Tasa de degradación en el agua (día-1)
  Q10_kw <- 2.6    # P
  temp_kw <- 20    # P
  ks_sat <- 0.03   # P            Tasa de degradación en el sedimento (día-1)
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

  cw_multiplier <-
    (1-is_empty) / pmax2(volume_m3, 1e-10) +
    is_empty * (outflow_m3 > 0) / pmax2(outflow_m3, 1e-10)

  msetl_multiplier <- (1-is_empty) * ksetl * (fpw / height_m)

  mdifus_mult_cs <- (1-is_empty) * kdifus * sa * fds  / pos
  mdifus_mult_cw <- -(1-is_empty) * kdifus * sa * fdw

  # Applying Arrhenius kinetic equilibrium

  kw <- kw * (Q10_kw ^ ((temperature - temp_kw) / 10))
  ks_sat <- ks_sat * (Q10_ks_sat ^ ((temperature - temp_ks_sat) / 10))
  ks_unsat <- ks_unsat * (Q10_ks_unsat ^ ((temperature - temp_ks_unsat) / 10))

  ks <- (1-is_empty) * ks_sat + is_empty * ks_unsat

  mwdeg_multiplier <- (1 - exp(-kw * dt))
  msdeg_multiplier <- (1 - exp(-ks * dt))
  mfdeg_multiplier <- (1 - exp(-kf * dt))

  mwash_multiplier <- (1 - exp(-fet * rain_cm * dt))



  f <- function(time, state, parms)
  {

    mf <- state[[1]]
    mw <- state[[2]]
    ms <- state[[3]]

    t <- time

    ### Solubility
    vw_t <- volume_m3[[t]]
    qout_t <- outflow_m3[[t]]

    # TODO: Critical, how should we define this density when the water level is
    # zero? ATM, this is defined taking the daily outflow as the relevant volume,
    # but we should think whether this is exactly what we want... and this should
    # likely be determined by how this density enters the equations below.
    cw <- cw_multiplier[[t]] * mw

    # TODO URGENT:
    # the following two equations introduce the only non-linearity in this
    # differential system.
    # This appears to be a special treatment due to the possibility of Volume=0,
    # which seems to be absent in the RICEWQ documentation.
    # Is there any possibility to avoid this? Without this, it should be possible
    # to solve the entire system analytically.
    sol_diff <- pmax2(cw - sol, 0) * vw_t
    cw <- pmin2(cw, sol)

    mw <- (mw - sol_diff) * (!is_empty[[t]])
    ms <- (ms + sol_diff)

    cs <- ms / vsed

    ### Outflow
    ### TODO: is rain correctly accounted here?
    ### (e.g. days in which it rains a certain amount and immediately flows
    ### away with draining)
    mout <- qout_t * cw

    ### Settlement
    msetl <- msetl_multiplier[[t]] * mw

    ### Diffusion
    mdifus <- mdifus_mult_cs[[t]] * cs + mdifus_mult_cw[[t]] * cw

    ### Degradation
    # Applying Arrhenius kinetic equilibrium
    mwdeg <- mwdeg_multiplier[[t]] * mw   # The discrete version of dx = k x dt
    msdeg <- mwdeg_multiplier[[t]] * ms
    mfdeg <- mfdeg_multiplier * mf  # A scalar!

    ### Washout
    mwash <- mwash_multiplier[[t]] * mf


    ### Final derivatives
    #fb <- function(x) x / (x + kmonod)  # Turn on in case of <0 values?
    dMF <- (mfapp[t] - mfdeg - mwash)# * fb(mf)
    dMW <- (mwapp[t] - mwdeg + mwash - msetl + mdifus - mout) #* fb(mw)
    dMS <- (msapp[t] - msdeg + msetl - mdifus) #* fb(ms)

    # TODO: 'deriv' below was originally multiplied by 'dt', but this seems to
    # be wrong (deSolve expects the derivative, not the increment)
    list(deriv = c(dMF, dMW, dMS))

  }

  f
}


euler_base <- function(y, times, func) {
  # Preallocate space for solution
  n <- length(times)
  state <- matrix(0, nrow = n, ncol = length(y)) # Handle multiple variables
  state[1, ] <- y  # Initial state

  dt <- diff(times) # Time steps

  # Euler's method loop
  for (i in 2:n) {
    derivs <- func(times[i - 1], state[i - 1, ])[[1]]  # Derivatives from model
    state[i, ] <- state[i - 1, ] + dt[i - 1] * derivs
  }

  # Return the results as a data frame of states
  return(as.data.frame(state))
}

euler_desolve <- function(y, times, func) {
  deSolve::ode(y = y, times = times, func = func,
               parms = list(), method = "euler"
               )
}

ode_solve <- euler_base

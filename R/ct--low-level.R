ct_to_cluster_wrap <- function(cluster_ca_df)
{
  res <- cluster_ca_df

  ct_to_cluster_fun <- ct_to_cluster

  chemicals <- unique(albufera_ca_schedules$chemical)
  chemicals <- names(res)[names(res) %in% chemicals]
  for (chemical in chemicals)
    res[[chemical]] <- ct_to_cluster(
      application_kg = cluster_ca_df[[chemical]],
      rain_mm = cluster_ca_df[["rain_mm"]],
      temperature = rnorm(nrow(cluster_ca_df)),
      height_cm = cluster_ca_df[["real_height_cm"]],
      outflow_m3_s = cluster_ca_df[["real_outflow_m3_s"]],
      area_m2 = cluster_ca_df[["area_m2"]][[1]],
      seed_day = cluster_ca_df[["seed_day"]],
      chemical = chemical
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

  solve <- ode_solve(y = c(mf = 0, mw = 0, ms = 0),
                     times =  seq_along(application_kg),
                     func = ode_model
                     )

  return(application_kg)
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

  ts_len <- length(application_kg)

  f <- function(time, state, parms)
  {

    mf <- state[[1]]
    mw <- state[[2]]
    ms <- state[[3]]

    t <- if (time < 1) 1 else if (time > ts_len) ts_len else time


    ### Applications
    # Interpolacion lineal entre 0 (dia de cultivo) y covmax (cobertura a maduracion).
    igrow <- seed_day[[t]] |> pmax2(0) |> pmin2(jgrow)
    cover <- covmax * (igrow / jgrow)

    # TODO: We should use a threshold here, but likely not the same used in the
    # previous modeling steps.
    is_empty <- height_m[[t]] == 0

    m_app_kg <- application_kg[[t]] * (1 - drift)
    mfapp <- m_app_kg * cover
    mwapp <- m_app_kg * (1 - cover) * (1 - SNK) * (!is_empty)
    msapp <- m_app_kg * (1 - cover) * (1 - SNK) * (dinc / dact) * is_empty


    ### Solubility
    vw_t <- volume_m3[[t]]
    qout_t <- outflow_m3[[t]]
    temp_t <- temperature[[t]]

    # TODO: Critical, how should we define this density when the water level is
    # zero? ATM, this is defined taking the daily outflow as the relevant volume,
    # but we should think whether this is exactly what we want... and this should
    # likely be determined by how this density enters the equations below.
    cw <- (mw / max(vw_t, 1e-10)) * (!is_empty) +
      (mw / max(qout_t, 1e-10)) * (is_empty & qout_t > 0)

    # TODO URGENT:
    # the following two equations introduce the only non-linearity in this
    # differential system.
    # This appears to be a special treatment due to the possibility of Volume=0,
    # which seems to be absent in the RICEWQ documentation.
    # Is there any possibility to avoid this? Without this, it should be possible
    # to solve the entire system analytically.
    sol_diff <- pmax2(cw - sol, 0) * vw_t
    cw <- pmin2(cw, sol)

    mw <- (mw - sol_diff) * (!is_empty)
    ms <- (ms + sol_diff)

    cs <- ms / vsed

    ### Outflow
    ### TODO: is rain correctly accounted here?
    ### (e.g. days in which it rains a certain amount and immediately flows
    ### away with draining)
    mout <- qout_t * cw

    ### Settlement
    msetl <- ksetl * (fpw * mw / height_m[[t]]) * (!is_empty)

    ### Diffusion
    mdifus <- kdifus * sa * (fds * cs / pos - fdw * cw) * (!is_empty)



    ### Degradation
    # Applying Arrhenius kinetic equilibrium
    kw <- kw * (Q10_kw ^ ((temp_t - temp_kw) / 10))
    ks_sat <- ks_sat * (Q10_ks_sat ^ ((temp_t - temp_ks_sat) / 10))
    ks_unsat <- ks_unsat * (Q10_ks_unsat ^ ((temp_t - temp_ks_unsat) / 10))

    ks <- if (!is_empty) ks_sat else ks_unsat

    mwdeg <- mw * (1 - exp(-kw * dt))  # The discrete version of dx = k x dt
    msdeg <- ms * (1 - exp(-ks * dt))
    mfdeg <- mf * (1 - exp(-kf * dt))

    ### Washout
    mwash <- mf * (1 - exp(-fet * rain_cm[[t]] * dt))


    ### Final derivatives
    #fb <- function(x) x / (x + kmonod)  # Turn on in case of <0 values?
    dMF <- (mfapp - mfdeg - mwash)# * fb(mf)
    dMW <- (mwapp - mwdeg + mwash - msetl + mdifus - mout) #* fb(mw)
    dMS <- (msapp - msdeg + msetl - mdifus) #* fb(ms)

    # TODO: 'deriv' below was originally multiplied by 'dt', but this seems to
    # be wrong (deSolve expects the derivative, not the increment)
    list(
      deriv = c(dMF, dMW, dMS),
      var_names = c(dw_t = height_m[[t]], vw_t = vw_t, qout_t=qout_t, mout=mout, cw=cw, mdifus=mdifus, mwapp=mwapp, msetl=msetl, mwdeg=mwdeg, mwash=mwash)
    )

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

  # Return the results as a data frame with times and states
  return(as.data.frame(cbind(time = times, state)))
}

euler_desolve <- function(y, times, func) {
  deSolve::ode(y = y, times = times, func = func,
               parms = list(), method = "euler"
               )
}

ode_solve <- euler_base

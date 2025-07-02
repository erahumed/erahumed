dss_input_defaults <- function() {
  fmls <- formals(erahumed_simulation)

  date_range <- c(eval(fmls$date_start), eval(fmls$date_end))

  list(
    seed = eval(fmls$seed),
    date_range = date_range,
    sc_intercept = eval(fmls$storage_curve_intercept_m3) / 1e6,
    sc_slope = eval(fmls$storage_curve_slope_m2) / 1e6,
    petp_surface = eval(fmls$petp_surface_m2) / 1e6,
    ideal_flow_rate_cm = eval(fmls$ideal_flow_rate_cm),
    height_thresh_cm = eval(fmls$height_thresh_cm),
    ditch_level_m = eval(fmls$ditch_level_m),
    drift = eval(fmls$drift),
    covmax = eval(fmls$covmax),
    jgrow = eval(fmls$jgrow),
    dact_m = eval(fmls$dact_m),
    css_ppm = eval(fmls$css_ppm),
    foc = eval(fmls$foc),
    bd_g_cm3 = eval(fmls$bd_g_cm3),
    qseep_m_day = eval(fmls$qseep_m_day),
    porosity = eval(fmls$porosity)
  )
}

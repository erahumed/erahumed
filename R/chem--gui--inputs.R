chem_input_display_name <- function(id) {
  inline_text_input(id, "Compound name", value =  chem_input_defaults()[["display_name"]])
}

chem_input_MW <- function(id) {
  inline_numeric_input(
    id,
    label = chem_input_label("MW"),
    value = chem_input_defaults()[["MW"]]
  )
}

chem_input_sol_ppm <- function(id) {
  inline_numeric_input(
    id,
    label = chem_input_label("sol_ppm"),
    value = chem_input_defaults()[["sol_ppm"]]
  )
}

chem_input_koc_cm3_g <- function(id) {
  inline_numeric_input(
    id,
    label = chem_input_label("koc_cm3_g"),
    value = chem_input_defaults()[["koc_cm3_g"]]
  )
}

chem_input_fet_cm <- function(id) {
  inline_numeric_input(
    id,
    label = chem_input_label("fet_cm"),
    value = chem_input_defaults()[["fet_cm"]]
  )
}

chem_input_kf_day <- function(id) {
  inline_numeric_input(
    id,
    label = chem_input_label("kf_day"),
    value = chem_input_defaults()[["kf_day"]]
  )
}

chem_input_kw_day <- function(id) {
  inline_numeric_input(
    id,
    label = chem_input_label("kw_day"),
    value = chem_input_defaults()[["kw_day"]]
  )
}

chem_input_ks_sat_day <- function(id) {
  inline_numeric_input(
    id,
    label = chem_input_label("ks_sat_day"),
    value = chem_input_defaults()[["ks_sat_day"]]
  )
}

chem_input_ks_unsat_day <- function(id) {
  inline_numeric_input(
    id,
    label = chem_input_label("ks_unsat_day"),
    value = chem_input_defaults()[["ks_unsat_day"]]
  )
}

chem_input_kw_temp <- function(id) {
  inline_numeric_input(
    id,
    label = chem_input_label("kw_temp"),
    value = chem_input_defaults()[["kw_temp"]]
  )
}

chem_input_ks_sat_temp <- function(id) {
  inline_numeric_input(
    id,
    label = chem_input_label("ks_sat_temp"),
    value = chem_input_defaults()[["ks_sat_temp"]]
  )
}

chem_input_ks_unsat_temp <- function(id) {
  inline_numeric_input(
    id,
    label = chem_input_label("ks_unsat_temp"),
    value = chem_input_defaults()[["ks_unsat_temp"]]
  )
}

chem_input_Q10_kw <- function(id) {
  inline_numeric_input(
    id,
    label = chem_input_label("Q10_kw"),
    value = chem_input_defaults()[["Q10_kw"]]
  )
}

chem_input_Q10_ks_sat <- function(id) {
  inline_numeric_input(
    id,
    label = chem_input_label("Q10_ks_sat"),
    value = chem_input_defaults()[["Q10_ks_sat"]]
  )
}

chem_input_Q10_ks_unsat <- function(id) {
  inline_numeric_input(
    id,
    label = chem_input_label("Q10_ks_unsat"),
    value = chem_input_defaults()[["Q10_ks_unsat"]]
  )
}

chem_input_tmoa_id <- function(id) {
  inline_text_input(
    id,
    label = chem_input_label("tmoa_id"),
    value = chem_input_defaults()[["tmoa_id"]]
  )
}

chem_input_ssd_acute_mu <- function(id) {
  inline_numeric_input(
    id,
    label = chem_input_label("ssd_acute_mu"),
    value = chem_input_defaults()[["ssd_acute_mu"]]
  )
}

chem_input_ssd_acute_sigma <- function(id) {
  inline_numeric_input(
    id,
    label = chem_input_label("ssd_acute_sigma"),
    value = chem_input_defaults()[["ssd_acute_sigma"]]
  )
}

chem_input_ssd_chronic_mu <- function(id) {
  inline_numeric_input(
    id,
    label = chem_input_label("ssd_chronic_mu"),
    value = chem_input_defaults()[["ssd_chronic_mu"]]
  )
}

chem_input_ssd_chronic_sigma <- function(id) {
  inline_numeric_input(
    id,
    label = chem_input_label("ssd_chronic_sigma"),
    value = chem_input_defaults()[["ssd_chronic_sigma"]]
  )
}



chem_input_defaults <- function() {
  fmls <- formals(chemical)

  list(
    display_name = "New chemical",
    tmoa_id = "New TMoA",
    MW = eval(fmls$MW),
    sol_ppm = eval(fmls$sol_ppm),
    koc_cm3_g = eval(fmls$koc_cm3_g),
    fet_cm = eval(fmls$fet_cm),
    kf_day = eval(fmls$kf_day),
    kw_day = eval(fmls$kw_day),
    ks_sat_day = eval(fmls$ks_sat_day),
    ks_unsat_day = eval(fmls$ks_unsat_day),
    kw_temp = eval(fmls$kw_temp),
    ks_sat_temp = eval(fmls$ks_sat_temp),
    ks_unsat_temp = eval(fmls$ks_unsat_temp),
    Q10_kw = eval(fmls$Q10_kw),
    Q10_ks_sat = eval(fmls$Q10_ks_sat),
    Q10_ks_unsat = eval(fmls$Q10_ks_unsat),
    ssd_acute_mu = eval(fmls$ssd_acute_mu),
    ssd_acute_sigma = eval(fmls$ssd_acute_sigma),
    ssd_chronic_mu = eval(fmls$ssd_chronic_mu),
    ssd_chronic_sigma = eval(fmls$ssd_chronic_sigma)
  )

}


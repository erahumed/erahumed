chem_input_MW <- function(id) {
  inline_numeric_input(
    id,
    label = shiny::p(shiny::HTML("Molecular Weight (g/mol)"), chem_input_tooltip("MW")),
    value = chem_input_defaults()[["MW"]]
  )
}

chem_input_sol_ppm <- function(id) {
  inline_numeric_input(
    id,
    label = shiny::p(shiny::HTML("Solubility (ppm)"), chem_input_tooltip("sol_ppm")),
    value = chem_input_defaults()[["sol_ppm"]]
  )
}

chem_input_koc_cm3_g <- function(id) {
  inline_numeric_input(
    id,
    label = shiny::p(shiny::HTML("K<sub>oc</sub> (cm<sup>3</sup>/g)"), chem_input_tooltip("koc_cm3_g")),
    value = chem_input_defaults()[["koc_cm3_g"]]
  )
}

chem_input_fet_cm <- function(id) {
  inline_numeric_input(
    id,
    label = shiny::p(shiny::HTML("Film exchange thickness (cm)"), chem_input_tooltip("fet_cm")),
    value = chem_input_defaults()[["fet_cm"]]
  )
}

chem_input_ksetl_m_day <- function(id) {
  inline_numeric_input(
    id,
    label = shiny::p(shiny::HTML("k<sub>setl</sub> (m/day)"), chem_input_tooltip("ksetl_m_day")),
    value = chem_input_defaults()[["ksetl_m_day"]]
  )
}

chem_input_kvolat_m_day <- function(id) {
  inline_numeric_input(
    id,
    label = shiny::p(shiny::HTML("k<sub>volat</sub> (m/day)"), chem_input_tooltip("kvolat_m_day")),
    value = chem_input_defaults()[["kvolat_m_day"]]
  )
}

chem_input_kf_day <- function(id) {
  inline_numeric_input(
    id,
    label = shiny::p(shiny::HTML("k<sub>f</sub> (1/day)"), chem_input_tooltip("kf_day")),
    value = chem_input_defaults()[["kf_day"]]
  )
}

chem_input_kw_day <- function(id) {
  inline_numeric_input(
    id,
    label = shiny::p(shiny::HTML("k<sub>w</sub> (1/day)"), chem_input_tooltip("kw_day")),
    value = chem_input_defaults()[["kw_day"]]
  )
}

chem_input_ks_sat_day <- function(id) {
  inline_numeric_input(
    id,
    label = shiny::p(shiny::HTML("k<sub>s,sat</sub> (1/day)"), chem_input_tooltip("ks_sat_day")),
    value = chem_input_defaults()[["ks_sat_day"]]
  )
}

chem_input_ks_unsat_day <- function(id) {
  inline_numeric_input(
    id,
    label = shiny::p(shiny::HTML("k<sub>s,unsat</sub> (1/day)"), chem_input_tooltip("ks_unsat_day")),
    value = chem_input_defaults()[["ks_unsat_day"]]
  )
}

chem_input_kw_temp <- function(id) {
  inline_numeric_input(
    id,
    label = shiny::p(shiny::HTML("T<sub>ref</sub> for k<sub>w</sub> (&deg;C)"), chem_input_tooltip("kw_temp")),
    value = chem_input_defaults()[["kw_temp"]]
  )
}

chem_input_ks_sat_temp <- function(id) {
  inline_numeric_input(
    id,
    label = shiny::p(shiny::HTML("T<sub>ref</sub> for k<sub>s,sat</sub> (&deg;C)"), chem_input_tooltip("ks_sat_temp")),
    value = chem_input_defaults()[["ks_sat_temp"]]
  )
}

chem_input_ks_unsat_temp <- function(id) {
  inline_numeric_input(
    id,
    label = shiny::p(shiny::HTML("T<sub>ref</sub> for k<sub>s,unsat</sub> (&deg;C)"), chem_input_tooltip("ks_unsat_temp")),
    value = chem_input_defaults()[["ks_unsat_temp"]]
  )
}

chem_input_Q10_kw <- function(id) {
  inline_numeric_input(
    id,
    label = shiny::p(shiny::HTML("Q<sub>10</sub> for k<sub>w</sub>"), chem_input_tooltip("Q10_kw")),
    value = chem_input_defaults()[["Q10_kw"]]
  )
}

chem_input_Q10_ks_sat <- function(id) {
  inline_numeric_input(
    id,
    label = shiny::p(shiny::HTML("Q<sub>10</sub> for k<sub>s,sat</sub>"), chem_input_tooltip("Q10_ks_sat")),
    value = chem_input_defaults()[["Q10_ks_sat"]]
  )
}

chem_input_Q10_ks_unsat <- function(id) {
  inline_numeric_input(
    id,
    label = shiny::p(shiny::HTML("Q<sub>10</sub> for k<sub>s,unsat</sub>"), chem_input_tooltip("Q10_ks_unsat")),
    value = chem_input_defaults()[["Q10_ks_unsat"]]
  )
}

chem_input_tmoa_id <- function(id) {
  inline_text_input(
    id,
    label = shiny::p("TMoA ID", chem_input_tooltip("tmoa_id")),
    value = chem_input_defaults()[["tmoa_id"]]
  )
}

chem_input_ssd_acute_mu <- function(id) {
  inline_numeric_input(
    id,
    label = shiny::p(shiny::HTML("SSD Acute &mu;"), chem_input_tooltip("ssd_acute_mu")),
    value = chem_input_defaults()[["ssd_acute_mu"]]
  )
}

chem_input_ssd_acute_sigma <- function(id) {
  inline_numeric_input(
    id,
    label = shiny::p(shiny::HTML("SSD Acute &sigma;"), chem_input_tooltip("ssd_acute_sigma")),
    value = chem_input_defaults()[["ssd_acute_sigma"]]
  )
}

chem_input_ssd_chronic_mu <- function(id) {
  inline_numeric_input(
    id,
    label = shiny::p(shiny::HTML("SSD Chronic &mu;"), chem_input_tooltip("ssd_chronic_mu")),
    value = chem_input_defaults()[["ssd_chronic_mu"]]
  )
}

chem_input_ssd_chronic_sigma <- function(id) {
  inline_numeric_input(
    id,
    label = shiny::p(shiny::HTML("SSD Chronic &sigma;"), chem_input_tooltip("ssd_chronic_sigma")),
    value = chem_input_defaults()[["ssd_chronic_sigma"]]
  )
}



chem_input_defaults <- function() NULL

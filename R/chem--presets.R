#' Predefined pesticide definitions
#'
#' These functions return predefined objects of class [chemical],
#' representing commonly used pesticides and their properties.
#'
#' @return An object of class [chemical].
#'
#' @examples
#' acetamiprid()
#'
#' @name pesticides
#' @aliases acetamiprid azoxystrobin bentazone cycloxydim cyhalofop_butyl
#'   difenoconazole mcpa penoxsulam
NULL

#' @rdname pesticides
#' @export
acetamiprid <- function() {
  chemical(display_name = "Acetamiprid",
           tmoa_id = "NicotinicAcetylcholine",
           MW = 222.677,
           ksetl_m_day = 2,
           sol_ppm = 2950,
           koc_cm3_g = 200, # PPDB
           fet_cm = 0.2,
           kf_day = 0.11,
           kw_day = 0.0154,
           ks_sat_day = 0.042,
           ks_unsat_day = 0.029,
           kw_temp = 20,
           ks_sat_temp = 20,
           ks_unsat_temp = 20,
           Q10_kw = 2.58,
           Q10_ks_sat = 2.58,
           Q10_ks_unsat = 2.58,
           ssd_acute_mu = 2.44,
           ssd_acute_sigma = 1.57,
           ssd_chronic_mu = 1.99,
           ssd_chronic_sigma = 1.58)
}

#' @rdname pesticides
#' @export
azoxystrobin <- function() {
  chemical(display_name = "Azoxystrobin",
           tmoa_id = "C_Respiration",
           MW = 403.338,
           ksetl_m_day = 2,
           sol_ppm = 6.7,
           koc_cm3_g = 589, # PPDB
           fet_cm = 0.2,
           kf_day = 0.09,
           kw_day = 0.0277,
           ks_sat_day = 0.003,
           ks_unsat_day = 0.003,
           kw_temp = 20,
           ks_sat_temp = 20,
           ks_unsat_temp = 20,
           Q10_kw = 2.58,
           Q10_ks_sat = 2.58,
           Q10_ks_unsat = 2.58,
           ssd_acute_mu = 2.75,
           ssd_acute_sigma = 0.63,
           ssd_chronic_mu = 1.89,
           ssd_chronic_sigma = 0.97)
}

#' @rdname pesticides
#' @export
bentazone <- function() {
  chemical(display_name = "Bentazone",
           tmoa_id = "Photosynthesis_PSII",
           MW = 240.28,
           ksetl_m_day = 2,
           sol_ppm = 7112,
           koc_cm3_g = 55.3, # PPDB
           fet_cm = 0.2,
           kf_day = 0.462,
           kw_day = 0.0013,
           ks_sat_day = 0.049,
           ks_unsat_day = 0.049,
           kw_temp = 20,
           ks_sat_temp = 20,
           ks_unsat_temp = 20,
           Q10_kw = 2.58,
           Q10_ks_sat = 2.58,
           Q10_ks_unsat = 2.58,
           ssd_acute_mu = 3.96,
           ssd_acute_sigma = 1.18,
           ssd_chronic_mu = 3.04,
           ssd_chronic_sigma = 0.92)
}

#' @rdname pesticides
#' @export
cycloxydim <- function() {
  chemical(display_name = "Cycloxydim",
           tmoa_id = "AcetylCoA",
           MW = 325.5,
           ksetl_m_day = 2,
           sol_ppm = 53,
           koc_cm3_g = 59, # PPDB
           fet_cm = 0.2,
           kf_day = 0.2476,
           kw_day = 0.046,
           ks_sat_day = 0.0301,
           ks_unsat_day = 0.1175,
           kw_temp = 20,
           ks_sat_temp = 20,
           ks_unsat_temp = 20,
           Q10_kw = 2.58,
           Q10_ks_sat = 2.58,
           Q10_ks_unsat = 2.58,
           ssd_acute_mu = 4.29,
           ssd_acute_sigma = 1.26,
           ssd_chronic_mu = 2.80,
           ssd_chronic_sigma = 1.22)
}

#' @rdname pesticides
#' @export
cyhalofop_butyl <- function() {
  chemical(display_name = "Cyhalofop-butyl",
           tmoa_id = "AcetylCoA",
           MW = 357.38,
           ksetl_m_day = 2,
           sol_ppm = 0.44,
           koc_cm3_g = 5247,
           fet_cm = 0.2,
           kf_day = 0.161,
           kw_day = 0.126,
           ks_sat_day = 0.976,
           ks_unsat_day = 0.976,
           kw_temp = 20,
           ks_sat_temp = 20,
           ks_unsat_temp = 20,
           Q10_kw = 2.58,
           Q10_ks_sat = 2.58,
           Q10_ks_unsat = 2.58,
           ssd_acute_mu = 3.31,
           ssd_acute_sigma = 0.70,
           ssd_chronic_mu = 1.78,
           ssd_chronic_sigma = 1.16)
}

#' @rdname pesticides
#' @export
difenoconazole <- function() {
  chemical(display_name = "Difenoconazole",
           tmoa_id = "Sterol_biosynthesis",
           MW = 406.3,
           ksetl_m_day = 2,
           sol_ppm = 15,
           koc_cm3_g = 3200,
           fet_cm = 0.2,
           kf_day = 0.094,
           kw_day = 0.231,
           ks_sat_day = 0.231,
           ks_unsat_day = 0.007,
           kw_temp = 20,
           ks_sat_temp = 20,
           ks_unsat_temp = 20,
           Q10_kw = 2.58,
           Q10_ks_sat = 2.58,
           Q10_ks_unsat = 2.58,
           ssd_acute_mu = 2.38,
           ssd_acute_sigma = 0.81,
           ssd_chronic_mu = 1.09,
           ssd_chronic_sigma = 0.97)
}

#' @rdname pesticides
#' @export
mcpa <- function() {
  chemical(display_name = "MCPA",
           tmoa_id = "Auxin_mimics",
           MW = 200.62,
           ksetl_m_day = 2,
           sol_ppm = 29390,
           koc_cm3_g = 73.88,  # PPDB
           fet_cm = 0.2,
           kf_day = 0.165,
           kw_day = 0.051,
           ks_sat_day = 0.0407,
           ks_unsat_day = 0.028,
           kw_temp = 20,
           ks_sat_temp = 20,
           ks_unsat_temp = 20,
           Q10_kw = 2.58,
           Q10_ks_sat = 2.58,
           Q10_ks_unsat = 2.58,
           ssd_acute_mu = 4.28,
           ssd_acute_sigma = 1.28,
           ssd_chronic_mu = 2.87,
           ssd_chronic_sigma = 1.30)
}

#' @rdname pesticides
#' @export
penoxsulam <- function() {
  chemical(display_name = "Penoxsulam",
           tmoa_id = "AcetoLactate",
           MW = 483.4,
           ksetl_m_day = 2,
           sol_ppm = 408,
           koc_cm3_g = 73.2,  # Differs slightly from PPDB value
           fet_cm = 0.2,
           kf_day = 0.248,
           kw_day = 0.046,
           ks_sat_day = 0.03,
           ks_unsat_day = 0.117,
           kw_temp = 20,
           ks_sat_temp = 20,
           ks_unsat_temp = 20,
           Q10_kw = 2.58,
           Q10_ks_sat = 2.58,
           Q10_ks_unsat = 2.58,
           ssd_acute_mu = 3.07,
           ssd_acute_sigma = 1.66,
           ssd_chronic_mu = 1.95,
           ssd_chronic_sigma = 1.49)
}

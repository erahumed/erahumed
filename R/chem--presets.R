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
           kvolat_m_day = 0,
           sol_ppm = 2950,
           koc_cm3_g = 200,
           dinc_m = 0,
           fet_cm = 0.2,
           kf_day = 0.110023362,
           kw_day = 0.042,
           ks_sat_day = 0.23104906,
           ks_unsat_day = 0.23104906,
           kw_temp = 20,
           ks_sat_temp = 20,
           ks_unsat_temp = 20,
           Q10_kw = 2.58,
           Q10_ks_sat = 2.58,
           Q10_ks_unsat = 2.58,
           ssd_acute_mu = 5.6240384,
           ssd_acute_sigma = 3.5474401,
           ssd_chronic_mu = 4.5749363,
           ssd_chronic_sigma = 3.5763254)
}

#' @rdname pesticides
#' @export
azoxystrobin <- function() {
  chemical(display_name = "Azoxystrobin",
           tmoa_id = "C_Respiration",
           MW = 403.338,
           ksetl_m_day = 2,
           kvolat_m_day = 0.003,
           sol_ppm = 6.7,
           koc_cm3_g = 589,
           dinc_m = 0,
           fet_cm = 0.2,
           kf_day = 0.093,
           kw_day = 0.114,
           ks_sat_day = 0.208,
           ks_unsat_day = 0.003,
           kw_temp = 20,
           ks_sat_temp = 20,
           ks_unsat_temp = 20,
           Q10_kw = 2.58,
           Q10_ks_sat = 2.58,
           Q10_ks_unsat = 2.58,
           ssd_acute_mu = 6.32789480,
           ssd_acute_sigma = 1.40725973,
           ssd_chronic_mu = 4.34895984,
           ssd_chronic_sigma = 2.15852371)
}

#' @rdname pesticides
#' @export
bentazone <- function() {
  chemical(display_name = "Bentazone",
           tmoa_id = "Photosynthesis_PSII",
           MW = 240.28,
           ksetl_m_day = 2,
           kvolat_m_day = 0.01,
           sol_ppm = 7112,
           koc_cm3_g = 55.3,
           dinc_m = 0,
           fet_cm = 0.2,
           kf_day = 0.462,
           kw_day = 0.009,
           ks_sat_day = 0.092,
           ks_unsat_day = 0.092,
           kw_temp = 20,
           ks_sat_temp = 20,
           ks_unsat_temp = 20,
           Q10_kw = 2.58,
           Q10_ks_sat = 2.58,
           Q10_ks_unsat = 2.58,
           ssd_acute_mu = 9.10616063,
           ssd_acute_sigma = 2.606912,
           ssd_chronic_mu = 6.99817816,
           ssd_chronic_sigma = 2.03936956)
}

#' @rdname pesticides
#' @export
cycloxydim <- function() {
  chemical(display_name = "Cycloxydim",
           tmoa_id = "AcetylCoA",
           MW = 325.5,
           ksetl_m_day = 2,
           kvolat_m_day = 0,
           sol_ppm = 53,
           koc_cm3_g = 59,
           dinc_m = 0,
           fet_cm = 0.2,
           kf_day = 0.2475526,
           kw_day = 0.04620981,
           ks_sat_day = 0.03013683,
           ks_unsat_day = 0.1174826,
           kw_temp = 20,
           ks_sat_temp = 20,
           ks_unsat_temp = 20,
           Q10_kw = 2.58,
           Q10_ks_sat = 2.58,
           Q10_ks_unsat = 2.58,
           ssd_acute_mu = 9.890557,
           ssd_acute_sigma = 2.720539,
           ssd_chronic_mu = 6.436792,
           ssd_chronic_sigma = 2.640179)
}

#' @rdname pesticides
#' @export
cyhalofop_butyl <- function() {
  chemical(display_name = "Cyhalofop-butyl",
           tmoa_id = "AcetylCoA",
           MW = 357.38,
           ksetl_m_day = 2,
           kvolat_m_day = 0,
           sol_ppm = 0.44,
           koc_cm3_g = 5247,
           dinc_m = 0,
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
           ssd_acute_mu = 7.605373,
           ssd_acute_sigma = 1.509798,
           ssd_chronic_mu = 4.086702,
           ssd_chronic_sigma = 2.502751)
}

#' @rdname pesticides
#' @export
difenoconazole <- function() {
  chemical(display_name = "Difenoconazole",
           tmoa_id = "Sterol_biosynthesis",
           MW = 406.3,
           ksetl_m_day = 2,
           kvolat_m_day = 0.001,
           sol_ppm = 15,
           koc_cm3_g = 3200,
           dinc_m = 0,
           fet_cm = 0.2,
           kf_day = 0.094,
           kw_day = 0.231,
           ks_sat_day = 0.231,
           ks_unsat_day = 0.008,
           kw_temp = 20,
           ks_sat_temp = 20,
           ks_unsat_temp = 20,
           Q10_kw = 2.58,
           Q10_ks_sat = 2.58,
           Q10_ks_unsat = 2.58,
           ssd_acute_mu = 5.465491,
           ssd_acute_sigma = 1.779176,
           ssd_chronic_mu = 2.506283,
           ssd_chronic_sigma = 2.15882)
}

#' @rdname pesticides
#' @export
mcpa <- function() {
  chemical(display_name = "MCPA",
           tmoa_id = "Auxin_mimics",
           MW = 200.62,
           ksetl_m_day = 2,
           kvolat_m_day = 0,
           sol_ppm = 250000,
           koc_cm3_g = 73.88,
           dinc_m = 0,
           fet_cm = 0.2,
           kf_day = 0.165,
           kw_day = 0.051,
           ks_sat_day = 0.041,
           ks_unsat_day = 0.029,
           kw_temp = 20,
           ks_sat_temp = 20,
           ks_unsat_temp = 20,
           Q10_kw = 2.58,
           Q10_ks_sat = 2.58,
           Q10_ks_unsat = 2.58,
           ssd_acute_mu = 9.863324,
           ssd_acute_sigma = 2.874493,
           ssd_chronic_mu = 6.603131,
           ssd_chronic_sigma = 2.92852)
}

#' @rdname pesticides
#' @export
penoxsulam <- function() {
  chemical(display_name = "Penoxsulam",
           tmoa_id = "AcetoLactate",
           MW = 483.4,
           ksetl_m_day = 2,
           kvolat_m_day = 0,
           sol_ppm = 408,
           koc_cm3_g = 73.2,
           dinc_m = 0,
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
           ssd_acute_mu = 7.070199,
           ssd_acute_sigma = 3.63297,
           ssd_chronic_mu = 4.503591,
           ssd_chronic_sigma = 3.243257)
}

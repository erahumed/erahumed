s_per_day <- function() {
  24 * 60 * 60
}

cm_day_to_m3_s <- function(x, area_m2) {
  x * area_m2 / s_per_day() / 100
}

m3_s_to_cm_day <- function(x, area_m2) {
  x * 100 * s_per_day() / area_m2
}

ppm_to_kg_m3 <- function(x) {
  1e-3 * x
}

ppm_to_g_cm3 <- function(x) {
  1e-6 * x
}

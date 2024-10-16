ct_deg_k <- function(k_ref, Q10, temperature, temperature_ref) {
  exp <- (temperature - temperature_ref) / 10
  fac <- Q10 ^ exp
  return(k_ref * fac)
}

ct_deg_fac <- function(k, dt) {
  exp(-k * dt)
}

albufera_ct_parameters <- readxl::read_excel(
  "data-raw/raw/ct_parameters.xlsx", sheet = 1
  ) |>
  as.data.frame() |>
  collapse::rsplit(  # This rather than 'base::split()' for simpler syntax
    by = value ~ c(chemical, parameter),
    flatten = FALSE,
    use.names = TRUE,
    simplify = TRUE
  )

.test_mod_small <- NULL
.test_mod_large <- NULL

test_mod_small <- function() {
  if (is.null(.test_mod_small))
    .test_mod_small <<- test_model("2010-01-01", "2010-01-10")

  .test_mod_small
}

test_mod_large <- function() {
  if (is.null(.test_mod_large))
    .test_mod_large <<- test_model("2010-01-01", "2011-12-31")

  .test_mod_large
}

test_model <- function(date_min, date_max, seed = 840) {
  outflows_df = albufera_outflows |>
    dplyr::filter(date_min <= date, date <= date_max)

  withr::with_seed(seed, {
    erahumed_model() |>
      compute_inp(outflows_df = outflows_df) |>
      compute_hba() |>
      compute_hbp() |>
      compute_ca()
  })
}

library(erahumed)
library(profvis)

ranges <- list(
  small = c("2010-01-01", "2010-01-31"),
  mid = c("2010-01-01", "2010-12-31"),
  large = c("2010-01-01", "2019-12-31")
)

models <- lapply(ranges, function(range) {
  outflows_df <- albufera_outflows |>
    dplyr::filter(range[[1]] <= date, date <= range[[2]])

  res <- erahumed_model() |>
    compute_inp(outflows_df = outflows_df) |>
    compute_hba() |>
    compute_hbp()

  return(res)
})


profvis({ compute_ca(models[[1]]) })

profvis({ compute_ca(models[[2]]) })

profvis({ compute_ca(models[[3]]) })

risk_from_ssds <- function(ct_output) {
  chemicals <- unique(ct_output$chemical)

  ct_output |>
    data.table::as.data.table() |>
    collapse::rsplit(by = ~ chemical,
                     flatten = TRUE,
                     use.names = FALSE,
                     simplify = FALSE,
                     keep.by = TRUE
                     ) |>
    lapply(function(df) {
      chemical <- df$chemical[[1]]
      mu <- info_chemicals()[[chemical]][["ssd_mu"]]
      sigma <- info_chemicals()[[chemical]][["ssd_sigma"]]

      df$paf <- plnorm(1e6 * df$cw, meanlog = mu, sdlog = sigma)  # TODO check units cf. #296
      df
    }) |>
    data.table::rbindlist() |>
    collapse::rsplit(by = ~ date + element_id,
                     flatten = TRUE,
                     use.names = FALSE,
                     simplify = FALSE,
                     keep.by = TRUE
                     ) |>
    lapply(function(df) {  # TODO: Use TMoA approach here
      list(element_id = df$element_id[[1]],
           date = df$date[[1]],
           paf = 1 - prod(1 - df$paf)
           )
      }) |>
    data.table::rbindlist() |>
    as.data.frame()
}

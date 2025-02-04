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
      median_concentration <- exp(info_chemicals()[[chemical]][["ssd_mu"]])

      df$HU <- 1e6 * df$cw / median_concentration  # TODO: clarify units
      df$tmoa <- info_chemicals()[[chemical]][["tmoa"]]
      df$sigma <- info_chemicals()[[chemical]][["ssd_sigma"]]

      df
    }) |>
    data.table::rbindlist() |>
    collapse::rsplit(by = ~ date + element_id + tmoa,
                     flatten = TRUE,
                     use.names = FALSE,
                     simplify = FALSE,
                     keep.by = TRUE
                     ) |>
    lapply(function(df) {

      list(element_id = df$element_id[[1]],
           date = df$date[[1]],
           paf = plnorm(log(sum(df$HU)), meanlog = 0, sdlog = mean(df$sigma))
        )
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

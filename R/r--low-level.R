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

      median_acute <- exp(info_chemicals()[[chemical]][["ssd_acute_mu"]])
      median_chronic <- exp(info_chemicals()[[chemical]][["ssd_chronic_mu"]])

      df$HU_acute <- 1e6 * df$cw / median_acute  # TODO: clarify units
      df$HU_chronic <- 1e6 * df$cw / median_chronic  # TODO: clarify units

      df$sigma_acute <- info_chemicals()[[chemical]][["ssd_acute_sigma"]]
      df$sigma_chronic <- info_chemicals()[[chemical]][["ssd_chronic_sigma"]]

      df$tmoa <- info_chemicals()[[chemical]][["tmoa"]]

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
           paf_acute = plnorm(log(sum(df$HU_acute)),
                              meanlog = 0,
                              sdlog = mean(df$sigma_acute)
                              ),
           paf_chronic = plnorm(log(sum(df$HU_chronic)),
                              meanlog = 0,
                              sdlog = mean(df$sigma_chronic)
                              )
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
           paf_acute = 1 - prod(1 - df$paf_acute),
           paf_chronic = 1 - prod(1 - df$paf_chronic)
           )
      }) |>
    data.table::rbindlist() |>
    as.data.frame()
}

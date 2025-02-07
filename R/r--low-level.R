#' @importFrom data.table let
risk_from_ssds <- function(ct_output) {
  # To avoid R CMD check note due to non-standard evaluation in {data.table}
  sigma_acute <- sigma_chronic <- median_acute <- median_chronic <-
    HU_acute <- HU_chronic <- element_id <- tmoa <- cw <- NULL

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

      df$median_acute <- exp(info_chemicals()[[chemical]][["ssd_acute_mu"]])
      df$median_chronic <- exp(info_chemicals()[[chemical]][["ssd_chronic_mu"]])

      # df$HU_acute <- 1e6 * df$cw / df$median_acute  # TODO: clarify units
      # df$HU_chronic <- 1e6 * df$cw / df$median_chronic  # TODO: clarify units

      df$sigma_acute <- info_chemicals()[[chemical]][["ssd_acute_sigma"]]
      df$sigma_chronic <- info_chemicals()[[chemical]][["ssd_chronic_sigma"]]

      df$tmoa <- info_chemicals()[[chemical]][["tmoa"]]

      df
    }) |>
    data.table::rbindlist() |>
    (\(dt) {
      dt[,
         let(
           sigma_acute = mean(sigma_acute),
           sigma_chronic = mean(sigma_chronic)
           ),
         by = "tmoa"
      ][,
        let(
          HU_acute = 1e6 * cw / median_acute,
          HU_chronic = 1e6 * rolling_average(cw, 21) / median_chronic
        ),
        by = c("element_id", "chemical")
      ][,
        list(
          HU_acute = sum(HU_acute),
          sigma_acute = sigma_acute[[1]],
          HU_chronic = sum(HU_chronic),
          sigma_chronic = sigma_chronic[[1]]
        ),
        by = c("element_id", "date", "tmoa")
      ][,
        let(
          paf_acute = pnorm(log(HU_acute), sd = sigma_acute),
          paf_chronic = pnorm(log(HU_chronic), sd = sigma_chronic)
          )
      ]
    })() |>

    as.data.frame()
}

#' @importFrom data.table let
risk_from_ssds <- function(ct_output) {
  # To avoid R CMD check note due to non-standard evaluation in {data.table}
  sd_acute <- sd_chronic <-
    median_acute <- median_chronic <- HU_acute <- HU_chronic <- element_id <-
    chemical <- tmoa <- cw_kg_m3 <- NULL

  chemicals <- unique(ct_output$chemical)

  res_prep <- ct_output |>
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

      df$sd_acute <- info_chemicals()[[chemical]][["ssd_acute_sigma"]]
      df$sd_chronic <- info_chemicals()[[chemical]][["ssd_chronic_sigma"]]

      df$tmoa <- info_chemicals()[[chemical]][["tmoa_name"]]

      df
    }) |>
    data.table::rbindlist() |>
    (\(dt) {
      dt[,
        let(cw_kg_m3 = data.table::fifelse(is.na(cw_kg_m3), 0, cw_kg_m3))
      ][,
        let(
          HU_acute = 1e6 * cw_kg_m3 / median_acute,
          HU_chronic = 1e6 * rolling_average(cw_kg_m3, 21) / median_chronic
        ),
        by = c("element_id", "chemical")
      ]
    })()


  res_tmoa <- res_prep[,
    let(sd_acute = mean(sd_acute), sd_chronic = mean(sd_chronic)), by = "tmoa"
    ][,
    list(HU_acute = sum(HU_acute),
         sd_acute = sd_acute[[1]],
         HU_chronic = sum(HU_chronic),
         sd_chronic = sd_chronic[[1]]
         ),
    by = c("element_id", "date", "tmoa")
    ][,
    let(paf_acute = stats::pnorm(log(HU_acute), sd = sd_acute),
        paf_chronic = stats::pnorm(log(HU_chronic), sd = sd_chronic),
        stressor = tmoa,
        stressor_type = "tmoa"
        )
    ][,
    c("element_id", "date", "stressor", "stressor_type", "paf_acute", "paf_chronic")
    ]

  res_chem <- res_prep[,
    let(paf_acute = stats::pnorm(log(HU_acute), sd = sd_acute),
        paf_chronic = stats::pnorm(log(HU_chronic), sd = sd_chronic),
        stressor = chemical,
        stressor_type = "chemical"
        )
    ][,
    c("element_id", "date", "stressor", "stressor_type", "paf_acute", "paf_chronic")
    ]

  rbind(res_tmoa, res_chem) |>
    as.data.frame()

}

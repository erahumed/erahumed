#' @importFrom data.table let
compute_risk_general <- function(ct_output, chemical_db) {
  # To avoid R CMD check note due to non-standard evaluation in {data.table}
  sd_acute <- sd_chronic <-
    median_acute <- median_chronic <- HU_acute <- HU_chronic <- element_id <-
    chemical_id <- chemical_name <- tmoa_id <- tmoa_name <- cw_kg_m3 <-
    hc05_acute <- hc05_chronic <- rq_acute <- rq_chronic <- NULL

  chemicals <- unique(ct_output$chemical)

  res_prep <- ct_output |>
    data.table::as.data.table() |>
    collapse::rsplit(by = ~ chemical_id, keep.by = TRUE) |>
    lapply(function(df) {
      chemical_id <- df$chemical_id[[1]]

      df$chemical_name <- ct_get_param(chemical_id, "display_name", chemical_db)
      df$tmoa_id <- ct_get_param(chemical_id, "tmoa_id", chemical_db)
      df$tmoa_name <- df$tmoa_id

      df$median_acute <- exp( ct_get_param(chemical_id, "ssd_acute_mu", chemical_db) )
      df$median_chronic <- exp( ct_get_param(chemical_id, "ssd_chronic_mu", chemical_db) )

      df$hc05_acute <- stats::qlnorm(0.05,
                                     meanlog = ct_get_param(chemical_id, "ssd_acute_mu", chemical_db),
                                     sdlog = ct_get_param(chemical_id, "ssd_acute_sigma", chemical_db)
                                     )
      df$hc05_chronic <- stats::qlnorm(0.05,
                                       meanlog = ct_get_param(chemical_id, "ssd_chronic_mu", chemical_db),
                                       sdlog = ct_get_param(chemical_id, "ssd_chronic_sigma", chemical_db)
                                       )


      df$sd_acute <- ct_get_param(chemical_id, "ssd_acute_sigma", chemical_db)
      df$sd_chronic <-  ct_get_param(chemical_id, "ssd_chronic_sigma", chemical_db)

      df
    }) |>
    data.table::rbindlist() |>
    (\(dt) {
      dt[,
        let(cw_kg_m3 = data.table::fifelse(is.na(cw_kg_m3), 0, cw_kg_m3))
      ][,
        let(
          HU_acute = 1e6 * cw_kg_m3 / median_acute,
          HU_chronic = 1e6 * rolling_average(cw_kg_m3, 21) / median_chronic,
          rq_acute = 1e6 * cw_kg_m3 / hc05_acute,
          rq_chronic = 1e6 * cw_kg_m3 / hc05_chronic
        ),
        by = c("element_id", "chemical_id")
      ]
    })()


  res_tmoa <- res_prep[,
    let(sd_acute = mean(sd_acute), sd_chronic = mean(sd_chronic)), by = "tmoa_id"
    ][,
    list(HU_acute = sum(HU_acute),
         sd_acute = sd_acute[[1]],
         HU_chronic = sum(HU_chronic),
         sd_chronic = sd_chronic[[1]],
         tmoa_name = tmoa_name[[1]]
         ),
    by = c("element_id", "date", "tmoa_id")
    ][,
    let(paf_acute = stats::pnorm(log(HU_acute), sd = sd_acute),
        paf_chronic = stats::pnorm(log(HU_chronic), sd = sd_chronic),
        rq_acute = NA,
        rq_chronic = NA,
        stressor_id = tmoa_id,
        stressor_name = tmoa_name,
        stressor_type = "tmoa"
        )
    ][,
    c("element_id", "date", "stressor_id", "stressor_name", "stressor_type", "paf_acute", "paf_chronic", "rq_acute", "rq_chronic")
    ]

  res_chem <- res_prep[,
    let(paf_acute = stats::pnorm(log(HU_acute), sd = sd_acute),
        paf_chronic = stats::pnorm(log(HU_chronic), sd = sd_chronic),
        rq_acute = rq_acute,
        rq_chronic = rq_chronic,
        stressor_id = chemical_id,
        stressor_name = chemical_name,
        stressor_type = "chemical"
        )
    ][,
    c("element_id", "date", "stressor_id", "stressor_name", "stressor_type", "paf_acute", "paf_chronic", "rq_acute", "rq_chronic")
    ]

  rbind(res_tmoa, res_chem) |>
    as.data.frame()

}

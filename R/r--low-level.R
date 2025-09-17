#' @importFrom data.table let
compute_risk_general <- function(ct_output, chemical_db) {
  # To avoid R CMD check note due to non-standard evaluation in {data.table}
  sd_acute <- sd_chronic <-
    median_acute <- median_chronic <- HU_acute <- HU_chronic <- element_id <-
    chemical_id <- chemical_name <- tmoa_id <- tmoa_name <- cw_kg_m3 <-
    pnec_acute_ug_L <- pnec_chronic_ug_L <- rq_acute <- rq_chronic <- NULL

  chemicals <- unique(ct_output$chemical)

  res_prep <- ct_output |>
    data.table::as.data.table() |>
    collapse::rsplit(by = ~ chemical_id, keep.by = TRUE) |>
    lapply(function(df) {
      chemical_id <- df$chemical_id[[1]]

      df$chemical_name <- ct_get_param(chemical_id, "display_name", chemical_db)
      df$tmoa_id <- ct_get_param(chemical_id, "tmoa_id", chemical_db)
      df$tmoa_name <- df$tmoa_id

      acute_mu_log10 <- ct_get_param(chemical_id, "ssd_acute_mu", chemical_db)
      acute_sd_log10 <- ct_get_param(chemical_id, "ssd_acute_sigma", chemical_db)
      chronic_mu_log10 <- ct_get_param(chemical_id, "ssd_chronic_mu", chemical_db)
      chronic_sd_log10 <- ct_get_param(chemical_id, "ssd_chronic_sigma", chemical_db)

      acute_mu_ln <- acute_mu_log10 * log(10)
      acute_sd_ln <- acute_sd_log10 * log(10)
      chronic_mu_ln <- chronic_mu_log10 * log(10)
      chronic_sd_ln <- chronic_sd_log10 * log(10)


      df$median_acute <- exp( acute_mu_ln )
      df$median_chronic <- exp( chronic_mu_ln )

      df$pnec_acute_ug_L <- ct_get_param(chemical_id, "pnec_acute_ug_L", chemical_db)
      df$pnec_chronic_ug_L <- ct_get_param(chemical_id, "pnec_chronic_ug_L", chemical_db)

      df$sd_acute <- acute_sd_ln
      df$sd_chronic <-  chronic_sd_ln

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
          rq_acute = 1e6 * cw_kg_m3 / pnec_acute_ug_L,
          rq_chronic = 1e6 * cw_kg_m3 / pnec_chronic_ug_L
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
    c("element_id", "date", "stressor_id", "stressor_name", "stressor_type", "rq_acute", "rq_chronic", "paf_acute", "paf_chronic")
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
    c("element_id", "date", "stressor_id", "stressor_name", "stressor_type", "rq_acute", "rq_chronic", "paf_acute", "paf_chronic")
    ]

  rbind(res_tmoa, res_chem) |>
    as.data.frame()

}

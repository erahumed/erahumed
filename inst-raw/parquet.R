library(erahumed)
df <- withr::with_seed(840,
      withr::with_envvar(c(erahumed_use_precomputed = FALSE),
        albufera_hbp()
      ))
arrow::write_parquet(df, "inst/parquet/albufera_hbp.parquet")

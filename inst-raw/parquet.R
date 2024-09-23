library(erahumed)
df <- withr::with_seed(840,
      withr::with_envvar(c(erahumed_use_precomputed = FALSE),
        hbp()
      ))
arrow::write_parquet(df, "inst/parquet/hbp.parquet")

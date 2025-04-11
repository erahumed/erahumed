test_that("dss_download creates a zip with expected CSV files", {
  zip_file <- tempfile(fileext = ".zip")
  dss_download(zip_file, test_sim_small())

  expect_true(file.exists(zip_file))

  unzip_dir <- tempfile()
  unzip(zip_file, exdir = unzip_dir)
  results_dir <- file.path(unzip_dir, "results")
  expect_true(dir.exists(results_dir))

  files <- list.files(results_dir, full.names = FALSE)
  expect_length(files, 9) # 3 components × 3 elements

  expected_names <- outer(
    c("hydrology", "exposure", "risk"),
    c("lake", "ditch", "cluster"),
    paste, sep = "-"
  )
  expected_files <- paste0(as.vector(expected_names), ".csv")

  expect_setequal(files, expected_files)

  sample_file <- file.path(results_dir, "hydrology-lake.csv")
  expect_true(file.exists(sample_file))
  df <- readr::read_csv(sample_file, show_col_types = FALSE)
  expect_s3_class(df, "data.frame")
})

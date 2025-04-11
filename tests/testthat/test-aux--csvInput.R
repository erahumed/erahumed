test_that("UI succeeds", {
  expect_no_error(csvInputUI("ui"))
})

args <- list(initial_df = albufera_weather)
shiny::testServer(csvInputServer, args = args, {

  # Test that server returns with default inputs
  expect_no_error(session$returned())
  expect_s3_class(session$returned(), class(data.frame()))

  # Test upload file feature
  weather_df_csv <- test_path("data", "weather-df.csv")
  weather_df_xlsx <- test_path("data", "weather-df.xlsx")
  weather_df_invalid_xlsx <- test_path("data", "weather-df-invalid.xlsx")

  session$setInputs(file = list(datapath = weather_df_csv))
  expect_no_error(df())
  expect_s3_class(df(), "data.frame")
  expect_equal(df(), readr::read_csv(weather_df_csv), ignore_attr = TRUE)

  session$setInputs(file = list(datapath = weather_df_xlsx))
  expect_no_error(df())
  expect_s3_class(df(), "data.frame")
  expect_equal(df(), readxl::read_xlsx(weather_df_xlsx), ignore_attr = TRUE)

  session$setInputs(file = list(datapath = weather_df_invalid_xlsx))
  expect_no_error(df())
  expect_s3_class(df(), "data.frame")
  expect_equal(df(), readxl::read_xlsx(weather_df_xlsx), ignore_attr = TRUE)

  # Test that download button works correctly
  expect_no_error(output$downloadDataCSV)
  expect_no_error(output$downloadDataExcel)
}) |> suppressMessages()

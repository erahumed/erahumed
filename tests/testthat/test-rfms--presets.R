test_that("Preset wrappers succeed", {
  expect_no_error(jsendra_rfms())
  expect_no_error(bomba_rfms())
  expect_no_error(clearfield_rfms())
})

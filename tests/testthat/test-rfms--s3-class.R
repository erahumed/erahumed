test_that("new_rfms() constructor succeeds", {
  expect_no_error(new_rfms())
})

test_that("new_rfms() creates an object of the correct class", {
  system <- new_rfms()
  expect_s3_class(system, "erahumed_rfms")
})

test_that("Validator succeeds", {
  system <- new_rfms()
  expect_no_error(assert_rfms(system))
})

test_that("add_application() succeeds", {
  system <- new_rfms()
  chemical <- acetamiprid()
  expect_no_error(schedule_application(system = system,
                                       chemical = chemical,
                                       seed_day = 50,
                                       amount_kg_ha = 1,
                                       type = "ground")
  )
})

test_that("print() method suceeds", {
  expect_no_error( print(jsendra()) ) |>
    capture.output()
})

test_that("summary() method suceeds", {
  expect_no_error( summary(jsendra()) ) |>
    capture.output()
})

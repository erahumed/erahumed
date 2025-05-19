test_that("new_rfms() constructor succeeds", {
  expect_no_error(new_rfms())
})

test_that("new_rfms() creates an object of the correct class", {
  rfms <- new_rfms()
  expect_s3_class(rfms, "erahumed_rfms")
})

test_that("Validator succeeds", {
  rfms <- new_rfms()
  expect_no_error(assert_rfms(rfms))
})

test_that("add_application() succeeds", {
  rfms <- new_rfms()
  chemical <- acetamiprid()
  expect_no_error(add_application(rfms = rfms,
                                  chemical = chemical,
                                  seed_day = 50,
                                  amount_kg_ha = 1,
                                  type = "ground"))
})

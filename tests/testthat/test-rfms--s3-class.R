test_that("new_management_system() constructor succeeds", {
  expect_no_error(new_management_system())
})

test_that("new_management_system() creates an object of the correct class", {
  system <- new_management_system()
  expect_s3_class(system, "erahumed_management_system")
})

test_that("Validator succeeds", {
  system <- new_management_system()
  expect_no_error(assert_management_system(system))
})

test_that("add_application() succeeds", {
  system <- new_management_system()
  chemical <- acetamiprid()
  expect_no_error(schedule_application(system = system,
                                       chemical = chemical,
                                       seed_day = 50,
                                       amount_kg_ha = 1,
                                       type = "ground")
  )
})

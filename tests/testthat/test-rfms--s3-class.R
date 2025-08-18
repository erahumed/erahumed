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

test_that("print.erahumed_rfms() produces correctly formatted output", {
  system <- new_rfms(display_name = "Test System")
  
  # Capture print output
  output <- capture.output(print(system))
  
  # Verify the correct structure and formatting
  expect_match(output[1], "^<Rice Field Management System>$")
  expect_match(output[2], "^  Name               : Test System$")
  expect_match(output[3], "^  Sowing period")
  expect_match(output[4], "^  Perellonà period")
  expect_match(output[5], "^  Flow height")
  expect_match(output[6], "^  Perellonà height")
  expect_match(output[7], "^  Applications")
  
  # Verify invisible return
  expect_identical(print(system), system)
})

test_that("print.erahumed_rfms() handles special characters in display name", {
  system <- new_rfms(display_name = "Test System & More")
  
  # Capture print output
  output <- capture.output(print(system))
  
  # Verify display name is printed correctly
  expect_match(output[2], "^  Name               : Test System & More$")
})

test_that("summary.erahumed_rfms() produces correct output for empty applications", {
  system <- new_rfms(display_name = "Test System")
  
  # Capture summary output
  output <- capture.output(summary(system))
  
  # Should include the print output plus no applications message
  expect_match(output[1], "^<Rice Field Management System>$")
  expect_match(output[2], "^  Name               : Test System$")
  expect_match(output[8], "^  No chemical applications defined\\.$")
  
  # Verify invisible return
  expect_identical(summary(system), system)
})

test_that("summary.erahumed_rfms() produces correct output with applications", {
  system <- new_rfms(display_name = "Test System")
  chemical <- acetamiprid()
  system_with_app <- schedule_application(system = system,
                                         chemical = chemical,
                                         seed_day = 50,
                                         amount_kg_ha = 1.5,
                                         type = "ground")
  
  # Capture summary output
  output <- capture.output(summary(system_with_app))
  
  # Should include the print output plus applications table
  expect_match(output[1], "^<Rice Field Management System>$")
  expect_match(output[2], "^  Name               : Test System$")
  
  # Check that applications table is printed (should contain headers and data)
  # Find the line with chemical name
  chemical_line <- grep("acetamiprid", output, value = TRUE)
  expect_length(chemical_line, 1)
  expect_match(chemical_line, "acetamiprid")
  expect_match(chemical_line, "ground")
  expect_match(chemical_line, "50")
  expect_match(chemical_line, "1.5")
  
  # Verify invisible return
  expect_identical(summary(system_with_app), system_with_app)
})

test_that("print and summary methods are consistent", {
  system <- new_rfms(display_name = "Consistency Test")
  
  # Capture outputs
  print_output <- capture.output(print(system))
  summary_output <- capture.output(summary(system))
  
  # The first 7 lines should be identical (the print portion)
  expect_identical(print_output[1:7], summary_output[1:7])
  
  # Summary should have the additional "No chemical applications" message
  expect_match(summary_output[8], "No chemical applications defined")
})

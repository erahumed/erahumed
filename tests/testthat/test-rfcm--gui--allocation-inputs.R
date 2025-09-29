test_that("Input functions succeed", {
  funs <- c(
    # allocation_input_system,  # <- requires extra 'choices' argument
    allocation_input_target_fraction,
    allocation_input_field_type,
    allocation_input_ditches
  )

  for (fun in funs) {
    expect_no_error(fun("id"))
    expect_s3_class(fun("id"), "shiny.tag")
  }


  # Treat special cases
  expect_no_error(allocation_input_system("id", c("a", "b", "c")))
  expect_s3_class(allocation_input_system("id", c("a", "b", "c")), "shiny.tag")

})

test_that("allocation_input_defaults() succeeds", {
  expect_no_error(allocation_input_defaults())
})

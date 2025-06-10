library(shiny, quietly = TRUE)

test_that("UI succeeds", {
  expect_no_error(list_manager_ui("ui"))
})

# Server tests effectively covered in test-chem--gui.R

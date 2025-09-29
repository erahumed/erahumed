test_that("Input functions succeed", {
  funs <- c(
    # rfms_input_crop_calendar,  # <-- shiny.tag.list
    rfms_input_sowing_season,
    rfms_input_perellona_start_yday,
    rfms_input_perellona_end_yday,
    rfms_input_harvesting_yday,
    rfms_input_flow_height_cm,
    rfms_input_perellona_height_cm,
    # rfms_input_chemical_id,  # <-- requires 'choices' arg
    # rfms_input_seed_day,  # <-- requires 'max' arg
    rfms_input_amount_kg_ha,
    rfms_input_type,
    rfms_input_emptying_days
    )

  for (fun in funs) {
    expect_no_error(fun("id"))
    expect_s3_class(fun("id"), "shiny.tag")
  }


  # Treat special cases
  expect_no_error(rfms_input_crop_calendar("id"))
  expect_s3_class(rfms_input_crop_calendar("id"), "shiny.tag.list")

  expect_no_error(rfms_input_chemical_id("id", choices = 1:3))
  expect_s3_class(rfms_input_chemical_id("id", choices = 1:3), "shiny.tag")

  expect_no_error(rfms_input_seed_day("id", max = 150))
  expect_s3_class(rfms_input_seed_day("id", max = 150), "shiny.tag")

})

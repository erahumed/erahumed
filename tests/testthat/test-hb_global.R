test_that("hb_global() succeeds with valid inputs", {
  obj <-  hb_global(level = rep(1, 10),
                    rain_mm = rep(10, 10),
                    evapotranspiration_mm = rep(1, 10),
                    outflows = list(a = rep(1, 10), b = rep(2, 10))
                    )
  expect_s3_class(obj, "hb_global")
})

test_that("hb_global() succeeds with ellipsis args of correct length", {
  expect_no_error(hb_global(level = rep(1, 10),
                            rain_mm = 1:10,
                            evapotranspiration_mm = rep(1, 10),
                            outflows = list(a = rep(1, 10), b = rep(2, 10)),
                            correct_length_vec = 1:10,
                            another_correct_length_vec = letters[1:10]
                            )
                  )
})

test_that("hb_global() output has no NA values", {
  obj <-  hb_global(level = rep(1, 10),
                    rain_mm = rep(10, 10),
                    evapotranspiration_mm = rep(1, 10),
                    outflows = list(a = rep(1, 10), b = rep(2, 10))
                    )

  expect_no_error(na.fail(obj))
})

test_that("hb_global() raises an error if inputs have wrong lengths", {
  expect_error(hb_global(level = rep(1, 10),
                         rain_mm = rep(10, 5),
                         evapotranspiration_mm = rep(1, 10),
                         outflows = list(a = rep(1, 10), b = rep(2, 10)))
               )
})

test_that("hb_global() raises an error if inputs have wrong type", {
  expect_error(hb_global(level = rep(1, 10),
                         rain_mm = letters[1:10],
                         evapotranspiration_mm = rep(1, 10),
                         outflows = list(a = rep(1, 10), b = rep(2, 10))),
               class = "hb_global_argcheck_error"
  )
})

test_that("hb_global() error if storage_curve() has invalid signature", {
  expect_error(hb_global(level = rep(1, 10),
                         rain_mm = 1:10,
                         evapotranspiration_mm = rep(1, 10),
                         outflows = list(a = rep(1, 10), b = rep(2, 10)),
                         storage_curve = function() 1
                         ),
               class = "hb_global_argcheck_error"
  )
})

test_that("hb_global() error if petp_surface() has invalid signature", {
  expect_error(hb_global(level = rep(1, 10),
                         rain_mm = 1:10,
                         evapotranspiration_mm = rep(1, 10),
                         outflows = list(a = rep(1, 10), b = rep(2, 10)),
                         petp_surface = function(x) x
  ),
  class = "hb_global_argcheck_error"
  )
})

test_that("hb_global() error if an ellipsis arg has the wrong length", {
  expect_error(hb_global(level = rep(1, 10),
                         rain_mm = 1:10,
                         evapotranspiration_mm = rep(1, 10),
                         outflows = list(a = rep(1, 10), b = rep(2, 10)),
                         wrong_length_vec = 1:9
                         ),
  class = "hb_global_argcheck_error"
  )
})

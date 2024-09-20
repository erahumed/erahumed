test_that("lhb() succeeds with valid inputs", {
  obj <-  lhb(level = rep(1, 10),
                    rain_mm = rep(10, 10),
                    evapotranspiration_mm = rep(1, 10),
                    outflows = list(a = rep(1, 10), b = rep(2, 10))
                    )
  expect_s3_class(obj, "erahumed_lhb")
})

test_that("lhb() succeeds with ellipsis args of correct length", {
  expect_no_error(lhb(level = rep(1, 10),
                            rain_mm = 1:10,
                            evapotranspiration_mm = rep(1, 10),
                            outflows = list(a = rep(1, 10), b = rep(2, 10)),
                            correct_length_vec = 1:10,
                            another_correct_length_vec = letters[1:10]
                            )
                  )
})

test_that("lhb() output has no NA values", {
  obj <-  lhb(level = rep(1, 10),
                    rain_mm = rep(10, 10),
                    evapotranspiration_mm = rep(1, 10),
                    outflows = list(a = rep(1, 10), b = rep(2, 10))
                    )

  expect_no_error(na.fail(obj))
})

test_that("lhb() raises an error if inputs have wrong lengths", {
  expect_error(lhb(level = rep(1, 10),
                         rain_mm = rep(10, 5),
                         evapotranspiration_mm = rep(1, 10),
                         outflows = list(a = rep(1, 10), b = rep(2, 10)))
               )
})

test_that("lhb() raises an error if inputs have wrong type", {
  expect_error(lhb(level = rep(1, 10),
                         rain_mm = letters[1:10],
                         evapotranspiration_mm = rep(1, 10),
                         outflows = list(a = rep(1, 10), b = rep(2, 10))),
               class = "lhb_argcheck_error"
  )
})

test_that("lhb() error if storage_curve() has invalid signature", {
  expect_error(lhb(level = rep(1, 10),
                         rain_mm = 1:10,
                         evapotranspiration_mm = rep(1, 10),
                         outflows = list(a = rep(1, 10), b = rep(2, 10)),
                         storage_curve = function() 1
                         ),
               class = "lhb_argcheck_error"
  )
})

test_that("lhb() error if petp_surface() has invalid signature", {
  expect_error(lhb(level = rep(1, 10),
                         rain_mm = 1:10,
                         evapotranspiration_mm = rep(1, 10),
                         outflows = list(a = rep(1, 10), b = rep(2, 10)),
                         petp_surface = function(x) x
  ),
  class = "lhb_argcheck_error"
  )
})

test_that("lhb() error if an ellipsis arg has the wrong length", {
  expect_error(lhb(level = rep(1, 10),
                         rain_mm = 1:10,
                         evapotranspiration_mm = rep(1, 10),
                         outflows = list(a = rep(1, 10), b = rep(2, 10)),
                         wrong_length_vec = 1:9
                         ),
  class = "lhb_argcheck_error"
  )
})

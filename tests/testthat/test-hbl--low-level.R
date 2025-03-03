test_that(".hbl() succeeds with valid inputs", {
  expect_no_error({
    .hbl(level = rep(1, 10),
         precipitation_mm = rep(10, 10),
         evapotranspiration_mm = rep(1, 10),
         outflows = list(a = rep(1, 10), b = rep(2, 10))
         )
    })
})

test_that(".hbl() succeeds with ellipsis args of correct length", {
  expect_no_error(.hbl(level = rep(1, 10),
                            precipitation_mm = 1:10,
                            evapotranspiration_mm = rep(1, 10),
                            outflows = list(a = rep(1, 10), b = rep(2, 10)),
                            correct_length_vec = 1:10,
                            another_correct_length_vec = letters[1:10]
                            )
                  )
})

test_that(".hbl() output has no NA values", {
  obj <-  .hbl(level = rep(1, 10),
                    precipitation_mm = rep(10, 10),
                    evapotranspiration_mm = rep(1, 10),
                    outflows = list(a = rep(1, 10), b = rep(2, 10))
                    )

  expect_no_error(na.fail(obj))
})

test_that(".hbl() raises an error if inputs have wrong lengths", {
  expect_error(.hbl(level = rep(1, 10),
                    precipitation_mm = rep(10, 5),
                    evapotranspiration_mm = rep(1, 10),
                    outflows = list(a = rep(1, 10), b = rep(2, 10))),
               class = ".hbl_argcheck_error"
               )
})

test_that(".hbl() raises an error if inputs have wrong type", {
  expect_error(.hbl(level = rep(1, 10),
                         precipitation_mm = letters[1:10],
                         evapotranspiration_mm = rep(1, 10),
                         outflows = list(a = rep(1, 10), b = rep(2, 10))),
               class = ".hbl_argcheck_error"
  )
})

test_that(".hbl() error if an ellipsis arg has the wrong length", {
  expect_error(.hbl(level = rep(1, 10),
                         precipitation_mm = 1:10,
                         evapotranspiration_mm = rep(1, 10),
                         outflows = list(a = rep(1, 10), b = rep(2, 10)),
                         wrong_length_vec = 1:9
                         ),
  class = ".hbl_argcheck_error"
  )
})

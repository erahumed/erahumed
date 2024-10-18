test_that(".hba() succeeds with valid inputs", {
  expect_no_error({
    .hba(level = rep(1, 10),
         precipitation_mm = rep(10, 10),
         evapotranspiration_mm = rep(1, 10),
         outflows = list(a = rep(1, 10), b = rep(2, 10))
         )
    })
})

test_that(".hba() succeeds with ellipsis args of correct length", {
  expect_no_error(.hba(level = rep(1, 10),
                            precipitation_mm = 1:10,
                            evapotranspiration_mm = rep(1, 10),
                            outflows = list(a = rep(1, 10), b = rep(2, 10)),
                            correct_length_vec = 1:10,
                            another_correct_length_vec = letters[1:10]
                            )
                  )
})

test_that(".hba() output has no NA values", {
  obj <-  .hba(level = rep(1, 10),
                    precipitation_mm = rep(10, 10),
                    evapotranspiration_mm = rep(1, 10),
                    outflows = list(a = rep(1, 10), b = rep(2, 10))
                    )

  expect_no_error(na.fail(obj))
})

test_that(".hba() raises an error if inputs have wrong lengths", {
  expect_error(.hba(level = rep(1, 10),
                    precipitation_mm = rep(10, 5),
                    evapotranspiration_mm = rep(1, 10),
                    outflows = list(a = rep(1, 10), b = rep(2, 10))),
               class = ".hba_argcheck_error"
               )
})

test_that(".hba() raises an error if inputs have wrong type", {
  expect_error(.hba(level = rep(1, 10),
                         precipitation_mm = letters[1:10],
                         evapotranspiration_mm = rep(1, 10),
                         outflows = list(a = rep(1, 10), b = rep(2, 10))),
               class = ".hba_argcheck_error"
  )
})

test_that(".hba() error if storage_curve() has invalid signature", {
  expect_error(.hba(level = rep(1, 10),
                         precipitation_mm = 1:10,
                         evapotranspiration_mm = rep(1, 10),
                         outflows = list(a = rep(1, 10), b = rep(2, 10)),
                         storage_curve = function() 1
                         ),
               class = ".hba_argcheck_error"
  )
})

test_that(".hba() error if petp_function() has invalid signature", {
  expect_error(.hba(level = rep(1, 10),
                         precipitation_mm = 1:10,
                         evapotranspiration_mm = rep(1, 10),
                         outflows = list(a = rep(1, 10), b = rep(2, 10)),
                         petp_function = function(x) x
  ),
  class = ".hba_argcheck_error"
  )
})

test_that(".hba() error if an ellipsis arg has the wrong length", {
  expect_error(.hba(level = rep(1, 10),
                         precipitation_mm = 1:10,
                         evapotranspiration_mm = rep(1, 10),
                         outflows = list(a = rep(1, 10), b = rep(2, 10)),
                         wrong_length_vec = 1:9
                         ),
  class = ".hba_argcheck_error"
  )
})

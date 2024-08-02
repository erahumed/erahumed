test_that("hb_global() succeeds with valid inputs", {
  obj <-  hb_global(level = rep(1, 10),
                    P = rep(10, 10),
                    ETP = rep(1, 10),
                    outflows = list(a = rep(1, 10), b = rep(2, 10))
                    )
  expect_s3_class(obj, "hb_global")
})

test_that("hb_global() has no NA values", {
  obj <-  hb_global(level = rep(1, 10),
                    P = rep(10, 10),
                    ETP = rep(1, 10),
                    outflows = list(a = rep(1, 10), b = rep(2, 10))
                    )

  expect_no_error(na.fail(obj))
})

test_that("hb_global() raises an error if inputs have wrong lengths", {
  expect_error(hb_global(level = rep(1, 10),
                         P = rep(10, 5),
                         ETP = rep(1, 10),
                         outflows = list(a = rep(1, 10), b = rep(2, 10)))
               )
})

test_that("hb_global() raises an error if inputs have wrong type", {
  expect_error(hb_global(level = rep(1, 10),
                         P = rep(10, 5),
                         ETP = rep(1, 10),
                         outflows = list(a = rep(1, 10), b = rep(2, 10))),
               class = "hb_global_argcheck_error"
  )
})

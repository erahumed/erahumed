test_that("hb_global() succeeds with valid inputs", {
  obj <-  hb_global(level = rep(1, 10),
                    P = rep(10, 10),
                    ETP = rep(1, 10),
                    outflows = list(a = rep(1, 10), b = rep(2, 10))
                    )
  expect_s3_class(obj, "hb_global")
})

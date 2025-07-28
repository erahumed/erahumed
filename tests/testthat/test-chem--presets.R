test_that("chemical presets succeed", {
  funs <- list(acetamiprid, azoxystrobin, bentazone, cycloxydim,
               cyhalofop_butyl, difenoconazole, mcpa, penoxsulam)

  for (fun in funs) {
    expect_no_error(fun())
  }
})

test_that("chemical presets create objects of the correct class", {
  funs <- list(acetamiprid, azoxystrobin, bentazone, cycloxydim,
               cyhalofop_butyl, difenoconazole, mcpa, penoxsulam)

  for (fun in funs) {
    expect_s3_class(fun(), "erahumed_chemical")
  }
})

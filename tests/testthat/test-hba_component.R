test_that("Ensure constructor raises errors of the correct class", {

  expect_error(new_hba_component(output = "not a df", params = list()),
               class = "new_model_component_argcheck_error")

  expect_error(new_hba_component(output = data.frame(x = 1:10, y = 10:1),
                                 params = list(storage_curve = identity,
                                               petp_surface = function(x,y) x - y)
                                 ),
               class = "new_model_component_argcheck_error")

})

test_that("info_clusters() succeeds", {
  expect_no_error(info_clusters())
})

test_that("info_clusters(include_geometry = TRUE) succeeds", {
  expect_no_error(info_clusters(include_geometry = TRUE))
})

test_that("info_ditches() succeeds", {
  expect_no_error(info_ditches())
})

test_that("info_ditches(include_geometry = TRUE) succeeds", {
  expect_no_error(info_ditches(include_geometry = TRUE))
})




test_that("plot_albufera_clusters(): no error or warning with default args", {
  expect_no_error(plot_albufera_clusters())
  expect_no_warning(plot_albufera_clusters())
})

test_that("plot_albufera_clusters(): no error with default_rfms_map()", {
  expect_no_error(plot_albufera_clusters(default_rfms_map()))
})

test_that("plot_albufera_clusters(): warning but no error with wrong arg", {
  plot_albufera_clusters(rfms_map = "not a map") |>
    expect_no_error() |>
    expect_warning()
})



test_that("plot_albufera_pie(): succeeds", {
  pdf(NULL)    # open a null pdf device
  on.exit(dev.off(), add = TRUE)

  expect_no_error( plot_albufera_pie(rfms_map = default_rfms_map()) )
})




test_that(".rfms_palette(): suceeds", {
  expect_no_error(.rfms_palette(default_rfms_map()))
})

test_that(".rfms_palette(): returns a list of two named components", {
  res <- .rfms_palette(default_rfms_map())
  expect_vector(res, ptype = list(), size = 2)
})

test_that(".rfms_palette(): palette has the correct length", {
  res <- .rfms_palette(default_rfms_map())
  len <- length( default_rfms_map()$rfms_list )
  expect_vector(res$domain, ptype = character(), size = len)
  expect_vector(res$palette, ptype = character(), size = len)
})




test_that(".rfms_stats(): suceeds", {
  map <- default_rfms_map()
  domain <- .rfms_palette(map)$domain

  expect_no_error(.rfms_stats(rfms_map = map, domain = domain))
})



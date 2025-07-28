test_that("Allocation of a rfms succeeds", {
  map <- new_cluster_map()
  ms <- new_management_system()

  expect_no_error(allocate_surface(map = map,
                                   system = ms,
                                   target_fraction = 0.5)
                  )
})

test_that("allocate_surface() returns an object of the correct type", {
  res <-     allocate_surface(map = new_cluster_map(),
                              system = new_management_system(),
                              target_fraction = 0.5)

  expect_no_error(assert_cluster_map(res))
})

test_that("allocate_surface() increases the number of rfmss only upon adding new RFMS", {
  map <- new_cluster_map()
  expect_length(map$rfms_list, 1)
  map <- allocate_surface(map = map, system = jsendra(), target_fraction = .1)
  expect_length(map$rfms_list, 2)
  map <- allocate_surface(map = map, system = jsendra(), target_fraction = .1)
  expect_length(map$rfms_list, 2)
  map <- allocate_surface(map = map, system = clearfield(), target_fraction = .1)
  expect_length(map$rfms_list, 3)
  })

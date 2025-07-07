test_that("map_candidates(): succeeds", {
  map <- new_cluster_map()
  ditches <- paste0("d", 1:26)
  field_type <- "both"

  expect_no_error(map_candidates(map = map, ditches = ditches, field_type = field_type))
})

test_that("map_candidates(): returns a character vector", {
  res <- map_candidates(map = new_cluster_map(),
                        ditches = paste0("d", 1:26),
                        field_type = "both")

  expect_vector(res, ptype = character())
})

test_that("map_candidates(): gives the correct output in simple case 1", {
  res <- map_candidates(map = new_cluster_map(),
                        ditches = paste0("d", 1:26),
                        field_type = "both")
  n_clusters <- nrow(info_clusters())

  expect_length(res, n_clusters)
})

test_that("map_candidates(): gives the correct output in simple case 2", {
  res <- map_candidates(map = new_cluster_map(),
                        ditches = paste0("d", 1:26),
                        field_type = "tancat")
  n_tancats <- info_clusters() |> (\(.) .[.$tancat, ])() |> nrow()

  expect_length(res, n_tancats)
})

test_that("map_candidates(): gives the correct output in simple case 3", {
  ditches <- paste0("d", 1:8)

  res <- map_candidates(map = new_cluster_map(),
                        ditches = ditches,
                        field_type = "both")
  n_clusters <- info_clusters() |>
    (\(.) .[.$ditch_element_id %in% ditches, ])() |>
    nrow()

  expect_length(res, n_clusters)
})

test_that("map_assign(): succeeds", {
  map <- new_cluster_map()
  cluster_id <- info_clusters()$element_id[[1]]

  expect_no_error(map_assign(map, cluster_id = cluster_id, ms_id = 1))
})

test_that("map_assign() returns an object of class map", {
  res <- map_assign(map = new_cluster_map(),
                    cluster_id = info_clusters()$element_id[[1]],
                    ms_id = 1)

  expect_no_error(assert_cluster_map(res))
})

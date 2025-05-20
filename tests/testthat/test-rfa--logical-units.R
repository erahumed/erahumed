test_that("rfa_candidates(): succeeds", {
  rfa <- new_rfa()
  ditches <- paste0("d", 1:26)
  field_type <- "both"

  expect_no_error(rfa_candidates(rfa = rfa, ditches = ditches, field_type = field_type))
})

test_that("rfa_candidates(): returns a character vector", {
  res <- rfa_candidates(rfa = new_rfa(),
                        ditches = paste0("d", 1:26),
                        field_type = "both")

  expect_vector(res, ptype = character())
})

test_that("rfa_candidates(): gives the correct output in simple case 1", {
  res <- rfa_candidates(rfa = new_rfa(),
                        ditches = paste0("d", 1:26),
                        field_type = "both")
  n_clusters <- nrow(info_clusters())

  expect_length(res, n_clusters)
})

test_that("rfa_candidates(): gives the correct output in simple case 2", {
  res <- rfa_candidates(rfa = new_rfa(),
                        ditches = paste0("d", 1:26),
                        field_type = "tancat")
  n_tancats <- info_clusters() |> (\(.) .[.$tancat, ])() |> nrow()

  expect_length(res, n_tancats)
})

test_that("rfa_candidates(): gives the correct output in simple case 3", {
  ditches <- paste0("d", 1:8)

  res <- rfa_candidates(rfa = new_rfa(),
                        ditches = ditches,
                        field_type = "both")
  n_clusters <- info_clusters() |>
    (\(.) .[.$ditch_element_id %in% ditches, ])() |>
    nrow()

  expect_length(res, n_clusters)
})

test_that("rfa_assign(): succeeds", {
  rfa <- new_rfa()
  cluster_id <- info_clusters()$element_id[[1]]

  expect_no_error(rfa_assign(rfa, cluster_id = cluster_id, rfms_id = 1))
})

test_that("rfa_assign() returns an object of class rfa", {
  res <- rfa_assign(rfa = new_rfa(),
                    cluster_id = info_clusters()$element_id[[1]],
                    rfms_id = 1)

  expect_no_error(assert_rfa(res))
})

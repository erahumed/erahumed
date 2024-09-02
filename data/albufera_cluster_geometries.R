delayedAssign("albufera_cluster_geometries", local({
  # Used to test that errors for this assignment still allow normal execution
  # of other functions in the package
  testing_env_path <- "erahumed_test_cluster_geometries_error"
  is_testing_env <- Sys.getenv(testing_env_path) == "TRUE"
  stopifnot(!is_testing_env)

  requireNamespace("sf", quietly = TRUE)
  try(erahumed:::albufera_cluster_geometries, silent = TRUE)
}))

delayedAssign("albufera_cluster_geometries", local({
  try({
    # Simulate error, to test what would happen if the evaluation of
    # erahumed:::albufera_cluster_geometries failed.
    testing_env_path <- "erahumed_test_cluster_geometries_error"
    is_testing_env <- Sys.getenv(testing_env_path) == "TRUE"
    stopifnot(!is_testing_env)

    requireNamespace("sf", quietly = TRUE)
    erahumed:::albufera_cluster_geometries
  }, silent = TRUE)
}))

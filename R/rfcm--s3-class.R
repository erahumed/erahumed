#' Initialize Cluster-to-Management Map
#'
#' Creates a new cluster map object that assigns rice field management systems
#' to individual clusters in the simulation domain.
#'
#' @param default_management_system `[`[erahumed_management_system][management_system]`]` \cr
#'   Management system used for all clusters by default. If not provided,
#'   defaults to a system initialized with [new_management_system()].
#'
#' @return An object of class `erahumed_cluster_map`.
#'
#' @details The cluster map is used to associate different management strategies
#' with spatial clusters in the simulation.
#'
#' @seealso [management_system]
#'
#' @export
new_cluster_map <- function(default_management_system = new_management_system())
{
  map_df <- data.frame(cluster_id = info_clusters()$element_id, ms_id = NA)

  res <- list(map_df = map_df,
              ms_list = list(),
              default_ms = default_management_system
              )

  class(res) <- "erahumed_cluster_map"

  return(res)
}

is_cluster_map <- function(x) {
  inherits(x, "erahumed_cluster_map")
}

#' @export
print.erahumed_cluster_map <- function(x, ...) {
  n_clusters <- nrow(x$map_df)
  n_custom_ms <- length(x$ms_list)

  cat("<Cluster-to-Management Map>\n")
  cat("  Clusters         :", n_clusters, "\n")
  cat("  Management systems   :", n_custom_ms + 1, "\n")
  invisible(x)
}

#' @export
summary.erahumed_cluster_map <- function(object, ...) {
  n_clusters <- nrow(object$map_df)
  n_custom_ms <- length(object$ms_list)


  cat("<Cluster-to-Management Map Summary>\n")
  cat("  Total clusters        :", n_clusters, "\n")
  cat("  Management systems:", n_custom_ms + 1, "\n")

  freq <- table(object$map_df$ms_id)
  df <- data.frame(ms_id = names(freq), assigned_clusters = as.integer(freq), row.names = NULL)
  print(df)

  invisible(object)
}


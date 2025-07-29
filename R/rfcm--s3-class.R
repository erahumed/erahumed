#' Cluster-Based Assignment of Rice-Field Management Systems
#'
#' These functions define how different rice-field management systems are
#' assigned to spatial clusters within the simulation. Each cluster can be
#' associated with a specific rice field management system, allowing for
#' heterogeneous management across the simulation domain.
#'
#' @param default_management_system `[`[erahumed_management_system][management_system]`]` \cr
#'   A default management system assigned to all clusters initially. Typically
#'   created with [new_management_system()] or a helper like [jsendra()].
#' @param seed `[numeric(1)]` \cr
#'  Seed for random number generation in the assignation of clusters to
#'  management systems.
#'
#' @return An object of class `erahumed_rfms_map`.
#'
#' @details
#'
#' * `new_rfms_map()` initializes a cluster map with all clusters assigned
#'   the same default management system.
#'
#' * `default_rfms_map()` provides a predefined map inspired by current
#'   practices in the Albufera Natural Park. Specifically, it uses the *J. Sendra*
#'   system as the default, and allocates small proportions of *Bomba* (10% in
#'   tancats) and *Clearfield* (10% in ditches 1–19). This map is a convenient
#'   starting point for scenario simulation or customization.
#'
#' Cluster assignments can be modified using [allocate_surface()].
#'
#' @seealso [new_management_system()], [allocate_surface()]
#'
#' @name rfms_map
#'
#' @export
new_rfms_map <- function(default_management_system = new_management_system())
{
  map_df <- data.frame(
    cluster_id = info_clusters()$element_id,
    rfms_id = 1,
    rfms_name = default_management_system[["display_name"]]
    )

  res <- list(map_df = map_df, rfms_list = list(default_management_system))

  class(res) <- "erahumed_rfms_map"

  return(res)
}

is_rfms_map <- function(x) {
  inherits(x, "erahumed_rfms_map")
}

#' @export
print.erahumed_rfms_map <- function(x, ...) {
  cat("<Rice-Field Management System Map>\n")
  cat("  Clusters         :", nrow(x$map_df), "\n")
  cat("  Management systems   :", length(x$rfms_list), "\n")
  invisible(x)
}

#' @export
summary.erahumed_rfms_map <- function(object, ...) {
  cat("<Rice-Field Management System Map Summary>\n")
  cat("  Total clusters        :", nrow(object$map_df), "\n")
  cat("  Management systems:", length(object$rfms_list), "\n")

  df <- object$map_df

  df$readable_id <- paste(df$rfms_id, df$rfms_name, sep = ": ")

  freq <- table(df$readable_id)
  df <- data.frame(rfms_id = names(freq), assigned_clusters = as.integer(freq), row.names = NULL)
  print(df)

  invisible(object)
}


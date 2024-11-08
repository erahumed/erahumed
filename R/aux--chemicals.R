#' Information on supported chemicals
#'
#' @description
#' These helpers provide information on the set of chemicals supported by
#' ERAHUMED: `chemicals()` lists the short names of supported chemicals,
#' whereas `chemical_properties()` returns the properties of a given chemical.
#'
#' @param name A string. Short name of the chemical for which properties are
#' required, one of the entries of the vector returned by `chemicals()`.
#'
#' @return
#' A character vector for `chemicals()`, a list for `chemical_properties()`.
#'
#' @export
chemicals <- function() {
  names(albufera_ct_parameters)
}

#' @rdname chemicals
#' @export
chemical_properties <- function(name) {
  name <- match.arg(name, chemicals())
  albufera_ct_parameters[[name]]
}



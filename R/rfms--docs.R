#' Rice Field Management System
#'
#' Functions to define and initialize rice field management systems.
#'
#' @return An object of class `erahumed_rfms`.
#'
#' @details These functions are used to define and initialize rice field
#' management system. Specifically:
#'
#' * `new_rfms()` creates an empty management system, with no
#'   scheduled chemical applications.
#' * `jsendra()`, `bomba()`, and `clearfield()` provide
#'   predefined systems inspired by the *J. Sendra*, *Bomba*, and *Clearfield*
#'   rice varieties, respectively.
#'
#' Additional chemical applications can be scheduled using the helper function
#' [schedule_application()]. The default values of the arguments of
#' `new_rfms()` coincide with those used internally by `jsendra()`,
#' `bomba()`, and `clearfield()`.
#'
#' @seealso [schedule_application]
#'
#' @name rfms
NULL

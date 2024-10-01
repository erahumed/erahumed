get_mm <- function(date) {
  stopifnot(class(date)[1] == "POSIXlt")
  date$mon + 1
}

get_dd <- function(date) {
  stopifnot(class(date)[1] == "POSIXlt")
  date$mday
}

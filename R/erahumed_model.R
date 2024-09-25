#' @export
erahumed_model <- function()
  new_erahumed_model()

new_erahumed_model <- function()
  structure(list(), class = "erahumed_model")

is_erahumed_model <- function(obj) {
  if (!is.list(obj))
    return(FALSE)

  if ( !inherits(obj, class(new_erahumed_model())) )
    return(FALSE)

  return(TRUE)
}



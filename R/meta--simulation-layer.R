new_simulation_layer <- function(
    output = NULL,
    params = list(),
    validate_output = function(output) TRUE,
    validate_params = is.list
    )
{
  new_simulation_layer_argcheck(output, params, validate_output, validate_params)

  res <- list(output = output, params = params)
  class(res) <- "erahumed_simulation_layer"

  return(res)
}

new_simulation_layer_argcheck <- function(
    output,
    params,
    validate_output,
    validate_params
    )
{
  tryCatch({
    if(!is.null(output))
      assert_data.frame(output)
    assert_list(params)
    if(!validate_output(output))
      stop("Invalid 'output' passed to new_simulation_layer().")
    if(!validate_params(params))
      stop("Invalid 'params' passed to new_simulation_layer().")
  },
  error = function(e) {
    class(e) <- c("new_simulation_layer_argcheck_error", class(e))
    stop(e)
  })

}

#' @export
print.erahumed_simulation_layer <- function(x, ...) {
  layer_name <- class(x)[[1]]
  layer_name <- gsub("_", " ", layer_name, fixed = TRUE)
  layer_name <- toupper(layer_name)
  title <- paste("A", layer_name, "simulation layer.")

  cat(title)


  output <- layer_output(x)
  cat("\n\nOutput columns:", paste(names(output), collapse = ", "))

  # cat("\n\nExample of output:\n")
  # print(head(output))

  return(invisible(x))
}

#' @export
summary.erahumed_simulation_layer <- function(object, ...) {
  print(object, max = max)
}

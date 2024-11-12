new_simulation_layer <- function(
    output = NULL,
    params = list(),
    layer_name = ""
    )
{
  new_simulation_layer_argcheck(output, params, layer_name)

  res <- list(output = output, params = params)

  base_class <- paste0("erahumed_", layer_name)
  class(res) <- c(base_class, "erahumed_simulation_layer")

  return(res)
}

new_simulation_layer_argcheck <- function(output, params, layer_name)
{
  tryCatch({
    if(!is.null(output))
      assert_data.frame(output)
    assert_list(params)
    assert_string(layer_name)
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

  output <- get_layer_output(x)
  cat("\n\nOutput columns:", paste(names(output), collapse = ", "))

  return(invisible(x))
}

#' @export
summary.erahumed_simulation_layer <- function(object, ...) {
  print(object)
}

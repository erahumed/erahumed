#' Parameters documentation
#'
#' @description
#' Extracts the documentation for a specific ERAHUMED parameter as an R string.
#' For internal use only.
#'
#' @param param string. Parameter name.
#'
#' @return
#' A list.
#'
#' @export
erahumed_docs <- function(...)
{
  args <- list(...)

  sapply(args, assert_string)

  tags <- as.character(args)

  yml_path <- system.file("docs", "docs.yml", package = "erahumed")
  docs <- yaml::read_yaml(yml_path)

  res <- docs
  for (tag in tags) {
    res <- res[[tag]]
  }

  if (is.null(res)) {
    obj <- paste(tags, collapse = "/")
    warning( paste0("No entry found for '", obj, "' found in ", yml_path, ".") )
  }

  return(res)
}

erahumed_param_roxy <- function(param, layer) {
  docs <- erahumed_docs(layer, "parameters", param)

  if (is.null(docs[["description"]])) {
    docs[["description"]] <- paste(
      "No description available.",
      "If you think this is a bug, please reach out to us by filing an issue",
      "[on Github](https://github.com/erahumed/erahumed/issues)."
    )
  }

  if (is.null(docs[["type"]])) {
    docs[["type"]] <- "N.D."
  }

  res <- paste0("`[", docs[["type"]], "]` \\cr ", docs[["description"]])

  res <- gsub("\\\\link(?:\\[[^]]+\\])?\\{([^}]+)\\}", "`\\1`", res)

  return(res)
}

erahumed_dataset_format <- function(dataset, layer){
  docs <- erahumed_docs(layer, "parameters", dataset)

  if (is.null(docs)) {
    res <- paste(
      "No format description available.",
      "If you think this is a bug, please reach out to us by filing an issue",
      "[on Github](https://github.com/erahumed/erahumed/issues)."
    )

    return(res)
  }

  res <- paste0(
    "The cardinality of data is given by: ", docs[["cardinality"]],
    "The dataset features the following columns:",
    "\\describe{"
  )

  cols <- docs[["columns"]]
  for (i in seq_along(cols)) {
    res <- paste0(res,
                  "\\item{", names(cols)[[i]], "}{", cols[[i]], "}")
  }

  res <- paste0(res, "}")

  return(res)
}

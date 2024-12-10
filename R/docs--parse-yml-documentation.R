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
erahumed_param_docs <- function(param)
{
  assert_string(param)

  yml_path <- system.file("docs", "params.yml", package = "erahumed")
  docs <- yaml::read_yaml(yml_path)

  res <- docs[[param]]

  if (is.null(res))
    warning(paste0(
      "No YML entry for parameter '", param, "' found at ", yml_path, "."
      ))

  return(res)
}

erahumed_param_roxy <- function(param) {
  docs <- erahumed_param_docs(param)

  if (is.null(docs))
    return(paste(
      "No description available.",
      "If you think this is a bug, please reach out to us by filing an issue",
      "[on Github](https://github.com/erahumed/erahumed/issues)."
      ))

  res <- paste0("`[", docs[["type"]], "]` \\cr ", docs[["description"]])

  res <- gsub("\\\\link(?:\\[[^]]+\\])?\\{([^}]+)\\}", "`\\1`", res)

  return(res)
}

erahumed_dataset_format <- function(dataset){
  yml_path <- system.file("docs", "params.yml", package = "erahumed")
  docs <- yaml::read_yaml(yml_path)

  docs <- docs[[dataset]]

  if (is.null(docs)) {
    msg <- paste0(
      "No YML entry for dataset '", dataset, "' found at ", yml_path, "."
    )
    warning(msg)
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

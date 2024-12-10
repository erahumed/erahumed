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


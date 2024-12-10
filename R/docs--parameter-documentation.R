erahumed_param_docs <- function(param, field = "description", use_link = TRUE) {
  yml_path <- system.file("docs", "params.yml", package = "erahumed")
  docs <- yaml::read_yaml(yml_path)

  res <- docs[[param]][[field]]

  if (is.null(res)) {
    msg <- paste0(
      "No YML entry for parameter '", param, "' found at ", yml_path, "."
      )
    warning(msg)
    res <- paste(
      "No description available.",
      "If you think this is a bug, please reach out to us by filing an issue",
      "[on Github](https://github.com/erahumed/erahumed/issues)."
      )
  }

  if (!use_link) {
    res <- gsub("\\\\link(?:\\[[^]]+\\])?\\{([^}]+)\\}", "`\\1`", res)
  }

  res
}


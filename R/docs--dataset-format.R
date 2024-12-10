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

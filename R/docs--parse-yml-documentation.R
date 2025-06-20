get_roxy_param_entry <- function(docs) {
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

  return(res)
}

erahumed_input_docs <- function(...)
{
  args <- list(...)

  sapply(args, assert_string)

  tags <- as.character(args)

  yml_path <- system.file("docs", "inputs.yml", package = "erahumed")
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



input_roxy <- function(name) {
  get_roxy_param_entry(erahumed_input_docs(name))
}

erahumed_input_desc <- function(name, strip_roxy = TRUE) {
  res <- erahumed_input_docs(name, "description")

  if (strip_roxy)
    res <- strip_roxy_macros(res)

  return(res)
}

erahumed_dataset_format <- function(name){
  docs <- erahumed_input_docs(name)

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

chemical_prop_docs <- function(...)
{
  args <- list(...)

  sapply(args, assert_string)

  tags <- as.character(args)

  yml_path <- system.file("docs", "chemical_properties.yml", package = "erahumed")
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

chemical_prop_roxy <- function(name) {
  get_roxy_param_entry(chemical_prop_docs(name))
}

chemical_prop_desc <- function(name, strip_roxy = TRUE) {
  res <- chemical_prop_docs(name, "description")

  if (strip_roxy)
    res <- strip_roxy_macros(res)

  return(res)
}

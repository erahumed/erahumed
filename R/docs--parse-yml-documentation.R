parse_docs_yml <- function(fun = c("simulation", "chemical", "rfms", "application", "allocation")) {
  fun <- match.arg(fun)
  filename <- paste0(fun, ".yml")
  yml_path <- system.file("docs", filename, package = "erahumed")

  yaml::read_yaml(yml_path)
}

get_param_docs <- function(param, fun)
{
  assert_string(param)

  docs <- parse_docs_yml(fun)

  res <- docs[[param]]

  if (is.null(res))
    warning( paste0("No entry found for ", fun, " parameter '", param, "'.") )

  return(res)
}

get_param_roxy <- function(param, fun)
{
  docs <- get_param_docs(param = param, fun = fun)

  if (is.null(docs[["description"]])) {
    docs[["description"]] <- param_desc_placeholder()
  }

  if (is.null(docs[["type"]])) {
    docs[["type"]] <- "N.D."
  }

  res <- paste0("`[", docs[["type"]], "]` \\cr ", docs[["description"]])

  return(res)
}

get_param_desc <- function(param, fun, strip_roxy = TRUE) {
  docs <- get_param_docs(param = param, fun = fun)

  res <- docs[["description"]]

  if (strip_roxy)
    res <- strip_roxy_macros(res)

  return(res)
}

get_dataset_format_roxy <- function(param, fun){
  docs <- get_param_docs(param, fun)

  docs_df <- docs[["df"]]

  if (is.null(docs_df)) {
    res <- paste(
      "No dataset format description available.",
      "If you think this is a bug, please reach out to us by filing an issue",
      "[on Github](https://github.com/erahumed/erahumed/issues)."
    )

    return(res)
  }

  res <- paste0(
    "The cardinality of data is given by: ", docs_df[["cardinality"]],
    "The dataset features the following columns:",
    "\\describe{"
  )

  cols <- docs_df[["columns"]]
  for (i in seq_along(cols)) {
    res <- paste0(res,
                  "\\item{", names(cols)[[i]], "}{", cols[[i]], "}")
  }

  res <- paste0(res, "}")

  return(res)
}

get_param_docs_df <- function(fun, fmt_markdown = identity) {
  docs <- parse_docs_yml(fun)

  res <- data.frame(
    Parameter = names(docs) |> wrap_bcktcks() |> fmt_markdown(),
    Name = NA,
    Unit = NA,
    Group = NA,
    Type = NA,
    Description = NA
  )

  for (i in seq_along(docs)) {
    res[i, "Name"] <- (docs[[i]]$name %||% res[i, "Parameter"])
    res[i, "Unit"] <- (docs[[i]]$unit %||% "N/A")
    res[i, "Type"] <- docs[[i]]$type |>
      wrap_bcktcks() |> fmt_markdown()
    res[i, "Group"] <- docs[[i]]$group
    res[i, "Description"] <- docs[[i]]$description |>
      strip_roxy_macros() |> fmt_markdown()

  }

  return(res)
}



param_desc_placeholder <- function() {
  gh_issue_link <- "https://github.com/erahumed/erahumed/issues"

  paste(
    "No description available.",
    "If you think this is a bug, please reach out to us by filing an issue",
    "[on Github](", gh_issue_link, ")."
  )
}

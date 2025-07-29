erahumed_input_docs_df <- function(fmt_markdown = identity) {
  docs <- erahumed:::erahumed_input_docs()

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

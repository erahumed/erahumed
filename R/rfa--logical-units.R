rfa_candidates <- function(rfa,
                           ditches,
                           field_type = c("both", "regular", "tancat")
                           )
{
  assert_rfa(rfa)
  assert_character(ditches)
  field_type <- match.arg(field_type)

  df <- rfa$allocations |>
    merge(info_clusters(), by.x = "cluster_id", by.y = "element_id")

  df <- df[is.na(df$rfms_id), ]

  df <- df[df$ditch_element_id %in% ditches, ]

  if (field_type == "tancat") {
    df <- df[df$tancat, ]
  } else if (field_type == "regular") {
    df <- df[!df$tancat, ]
  }

  return(df$cluster_id)
}

rfa_assign <- function(rfa, cluster_id, rfms_id) {
  df <- rfa[["allocations"]]
  row <- which(df$cluster_id == cluster_id)
  df[row, "rfms_id"] <- rfms_id
  rfa[["allocations"]] <- df
  return(rfa)
}

rfa_cluster_fraction <- function(cluster_id) {
  df <- info_clusters()
  tot_area <- sum(df$area)
  clus_area <- df[df$element_id == cluster_id, ]$area

  clus_area / tot_area
}

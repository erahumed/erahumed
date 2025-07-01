map_candidates <- function(map,
                           ditches,
                           field_type = c("both", "regular", "tancat")
                           )
{
  assert_cluster_map(map)
  assert_character(ditches)
  field_type <- match.arg(field_type)

  df <- map$map_df |>
    merge(info_clusters(), by.x = "cluster_id", by.y = "element_id")

  df <- df[df$ms_id == 1, ]

  df <- df[df$ditch_element_id %in% ditches, ]

  if (field_type == "tancat") {
    df <- df[df$tancat, ]
  } else if (field_type == "regular") {
    df <- df[!df$tancat, ]
  }

  return(df$cluster_id)
}

map_assign <- function(map, cluster_id, ms_id) {
  df <- map[["map_df"]]
  row <- which(df$cluster_id == cluster_id)
  df[row, "ms_id"] <- ms_id
  df[row, "ms_name"] <- map[["ms_list"]][[ms_id]][["display_name"]]
  map[["map_df"]] <- df
  return(map)
}

map_surface_fraction <- function(cluster_id) {
  df <- info_clusters()
  tot_area <- sum(df$area)
  clus_area <- df[df$element_id == cluster_id, ]$area

  clus_area / tot_area
}

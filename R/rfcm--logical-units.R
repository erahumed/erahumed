rfcm_get_map_candidates <- function(map,
                           ditches,
                           field_type = c("both", "regular", "tancat")
                           )
{
  assert_cluster_map(map)
  assert_character(ditches)
  field_type <- match.arg(field_type)

  df <- rfcm_filter_cluster_df(ditches = ditches, field_type = field_type) |>
    merge(map$map_df, by.x = "element_id", by.y = "cluster_id") |>
    (\(.) .[.$ms_id == 1, ])()

  return(df$element_id)
}

rfcm_map_assign <- function(map, cluster_id, ms_id) {
  df <- map[["map_df"]]
  row <- which(df$cluster_id == cluster_id)
  df[row, "ms_id"] <- ms_id
  df[row, "ms_name"] <- map[["ms_list"]][[ms_id]][["display_name"]]
  map[["map_df"]] <- df
  return(map)
}

rfcm_filter_cluster_df <- function(ditches, field_type) {
  res <- info_clusters()

  res <- res[res$ditch_element_id %in% ditches, ]

  if (field_type == "tancat") {
    res <- res[res$tancat, ]
  } else if (field_type == "regular") {
    res <- res[!res$tancat, ]
  }

  return(res)
}

rfcm_get_filtered_surface <- function(ditches, field_type) {
  df <- rfcm_filter_cluster_df(ditches = ditches, field_type = field_type)
  sum(df$area)
}

rfcm_cluster_surface <- function(cluster_id) {
  info_clusters() |>
    (\(.) .[.$element_id == cluster_id, ])() |>
    (\(.) .$area)()
}


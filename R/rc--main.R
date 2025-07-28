compute_rc <- function(simulation)
{
  output <- compute_risk_general(ct_output = get_output(simulation, "ctc"),
                           chemical_db = get_etc(simulation, "chemical_db")
  )

  clus_props_df <-
    get_input(simulation, "cluster_map")[["map_df"]][, c("cluster_id", "rfms_id", "rfms_name")]
  output <- output |>
    merge(clus_props_df, by.x = "element_id", by.y = "cluster_id")

  validate_rc_output(output)

  simulation [["outputs"]] [["rc"]] <- output

  return(simulation)
}



validate_rc_output <- assert_data.frame

#' @export
materialize_output.erahumed_output_hbl <- function(output) {
  area_m2 <- output[["volume_m3"]][[1]] / output[["depth_m"]][[1]]

  data.frame(
    date = output[["date"]],
    element_id = "lake",
    area_m2 = area_m2,
    is_tancat = NA,

    outflow_m3 = output[["outflow_total_m3"]],
    outflow_cm = 100 * output[["outflow_total_m3"]] / area_m2,

    inflow_m3 = output[["inflow_total_m3"]],
    inflow_cm = 100 * output[["inflow_total_m3"]] / area_m2,

    volume_m3 = output[["volume_m3"]],
    depth_cm = 100 * output[["depth_m"]],

    petp_cm = 0.1 * (output[["precipitation_mm"]] - output[["evapotranspiration_mm"]])
  )
}

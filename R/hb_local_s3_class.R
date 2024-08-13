make_hb_local <- function(df)
{
  make_hb_local_argcheck(df)

  class(df) <- c("hb_local", class(df))
  attr(class(df), "package") <- "erahumed"
  return(df)
}


make_hb_local_argcheck <- function(df) {
  tryCatch(
    {
      assert_data.frame(
        df,
        template = data.frame(
          ideal_height_cm = numeric(),
          petp_cm = numeric(),
          irrigation = logical(),
          draining = logical(),
          area_m2 = numeric(),
          capacity_m3_s = numeric(),
          date = as.Date(character()),
          cluster_id = character(),
          ditch = character(),
          ideal_inflow_cm = numeric(),
          ideal_outflow_cm = numeric(),
          real_inflow_cm = numeric(),
          real_outflow_cm = numeric(),
          real_inflow_m3_s = numeric(),
          real_outflow_m3_s = numeric(),
          plan_delay = numeric(),
          real_height_cm = numeric()
          ),
        extends = TRUE
        )
    },
    error = function(e) {
      class(e) <- c("make_hb_local_argcheck_error", class(e))
      stop(e)
    })
}

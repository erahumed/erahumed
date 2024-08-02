make_hb_global <- function(df)
{
  make_hb_global_argcheck(df)

  class(df) <- c("hb_global", class(df))
  attr(class(df), "package") <- "erahumed"
  return(df)
}

make_hb_global_argcheck <- function(df) {
  tryCatch(
    {
      assert_data.frame(
        df,
        template = data.frame(level = numeric(),
                              rain_mm = numeric(),
                              evapotranspiration_mm = numeric(),
                              volume = numeric(),
                              inflow_total = numeric(),
                              outflow_total = numeric(),
                              outflow_extra = numeric(),
                              residence_time_days = numeric()
                              ),
        extends = TRUE
        )
    },
    error = function(e) {
      class(e) <- c("make_hb_global_argcheck_error", class(e))
      stop(e)
    })
}

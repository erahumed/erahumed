make_hb_global <- function(df) {
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

#' @export
print.hb_global <- function(x, ..., max = 100) {
  cat(bold("An object of class 'hb_global'."))

  min_date <- format(as.Date(min(x$date)))
  max_date <- format(as.Date(max(x$date)))
  cat("\nData from:", min_date, "to:", max_date, "\n\n")

  print.data.frame(x, max = max)
}

#' @export
summary.hb_global <- function(object, ..., max = 100) {
  print(object, max = max)
}

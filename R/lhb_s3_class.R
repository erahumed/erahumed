make_lhb <- function(df) {
  make_lhb_argcheck(df)

  class(df) <- c("erahumed_lhb", class(df))
  attr(class(df), "package") <- "erahumed"
  return(df)
}

make_lhb_argcheck <- function(df) {
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
      class(e) <- c("make_lhb_argcheck_error", class(e))
      stop(e)
    })
}

#' @export
print.erahumed_lhb <- function(x, ..., max = 100) {
  cat(bold("An object of class 'lhb'."))

  min_date <- format(as.Date(min(x$date)))
  max_date <- format(as.Date(max(x$date)))
  cat("\nData from:", min_date, "to:", max_date, "\n\n")

  print.data.frame(x, max = max)
}

#' @export
summary.erahumed_lhb <- function(object, ..., max = 100) {
  print(object, max = max)
}

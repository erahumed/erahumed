make_hba <- function(df) {
  make_hba_argcheck(df)

  class(df) <- c("erahumed_hba", class(df))

  return(df)
}

make_hba_argcheck <- function(df) {
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
      class(e) <- c("make_hba_argcheck_error", class(e))
      stop(e)
    })
}

#' @export
print.erahumed_hba <- function(x, ..., max = 100) {
  cat(bold("An object of class 'hba'."))

  min_date <- format(as.Date(min(x$date)))
  max_date <- format(as.Date(max(x$date)))
  cat("\nData from:", min_date, "to:", max_date, "\n\n")

  print.data.frame(x, max = max)
}

#' @export
summary.erahumed_hba <- function(object, ..., max = 100) {
  print(object, max = max)
}

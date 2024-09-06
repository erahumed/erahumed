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
      assert_positive_vector(df$ideal_height_cm)
      assert_positive_vector(df$area_m2)
      assert_positive_vector(df$capacity_m3_s)
      assert_date(df$date)
      assert_character(df$cluster_id)
      assert_character(df$ditch)
      assert_positive_vector(df$ideal_outflow_cm)
      assert_positive_vector(df$plan_delay)
      assert_integer_vector(df$plan_delay)
      assert_positive_vector(df$real_height_cm)
    },
    error = function(e) {
      class(e) <- c("make_hb_local_argcheck_error", class(e))
      stop(e)
    })
}

#' @export
print.hb_local <- function(x, ..., max = 100) {
  cat(bold("An object of class 'hb_local'."))

  min_date <- format(as.Date(min(x$date)))
  max_date <- format(as.Date(max(x$date)))
  cat("\nData from:", min_date, "to:", max_date, "\n\n")

  print.data.frame(x, max = max)
}

#' @export
summary.hb_local <- function(object, ..., max = 100) {
  print(object, max = max)
}

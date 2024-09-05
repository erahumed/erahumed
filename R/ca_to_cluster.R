ca_to_cluster <- function(date,
                          real_height_cm,
                          irrigation,
                          draining,
                          plan_delay,
                          application_day,
                          amount,
                          application_type = c("ground", "aerial"),
                          height_thresh_cm,
                          previous_applications,
                          sowing_mmdd,
                          sowing_yyyy)
{
  sowing_date <- paste0(sowing_yyyy, "-", sowing_mmdd)

  seed_day <- as.numeric( as.Date(date) - as.Date(sowing_date) )

  application_type = match.arg(application_type)

  return(previous_applications)  # TODO
}

ca_to_cluster_argcheck <- function(date,
                                   real_height_cm,
                                   irrigation,
                                   draining,
                                   plan_delay,
                                   application_day,
                                   amount,
                                   height_thresh_cm,
                                   previous_applications,
                                   sowing_mmdd,
                                   sowing_yyyy)
{
  tryCatch(
    {
      assert_date(date)
      assert_numeric_vector(real_height_cm)
      assert_logical(irrigation)
      assert_logical(draining)
      assert_positive_vector(plan_delay)
      assert_integer_vector(plan_delay)
      assert_positive_vector(previous_applications)

      n_lengths <- sapply(
        list(date, real_height_cm, irrigation, draining, plan_delay, previous_applications),
        length
        ) |>
        unique() |>
        length()
      if (n_lengths > 1)
        stop("Inputs have mismatched lengths.")

      assert_positive_integer(application_day)
      assert_positive_number(amount)

      assert_positive_number(height_thresh_cm)

      assert_string(sowing_mmdd)
      sowing_mmdd <- paste0("2000-", sowing_mmdd)
      assert_date(sowing_mmdd)

      assert_string(sowing_yyyy)
      sowing_yyyy <- paste0(sowing_yyyy, "-01-01")
      assert_date(sowing_yyyy)
    },
    error = function(e) {
      class(e) <- c("ca_to_cluster_argcheck_error", class(e))
      stop(e)
    }
  )
}

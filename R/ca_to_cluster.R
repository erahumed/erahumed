ca_to_cluster <- function(real_height_cm,
                          seed_day,
                          irrigation,
                          draining,
                          plan_delay,
                          application_day,
                          amount_kg,
                          application_type,
                          height_thresh_cm,
                          previous_applications)

{
  # Argument checking here is too costly and not really indispensable, as
  # all arguments are internally provided, and pass numerous checks before
  # arriving here. Leaving the code below just for potential debugging purposes.
  #
  # ca_to_cluster_argcheck(date,
  #                        real_height_cm,
  #                        irrigation,
  #                        draining,
  #                        plan_delay,
  #                        application_day,
  #                        amount,
  #                        height_thresh_cm,
  #                        previous_applications,
  #                        sowing_mmdd,
  #                        sowing_yyyy)

  potential_day_index <- TRUE &
    ca_filter_by_state(irrigation, draining, plan_delay, application_type) &
    ca_filter_by_water_level(real_height_cm, application_type) &  # TODO: use threshold argument
    ca_filter_by_previous_applications(previous_applications)

  res <- previous_applications
  idx <- ca_choose_application_day_index(application_day,
                                         potential_day_index,
                                         seed_day,
                                         plan_delay)
  res[idx] <- amount_kg

  return(res)
}


# Not used
ca_to_cluster_argcheck <- function(date,
                                   real_height_cm,
                                   irrigation,
                                   draining,
                                   plan_delay,
                                   application_day,
                                   amount_kg,
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
      assert_positive_number(amount_kg)

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

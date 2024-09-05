ca <- function(hbl,
               ca_schedules_df = erahumed::albufera_ca_schedules,
               height_thresh_cm = 2.5,
               sowing_day = "04-20")
{
  stopifnot(inherits(hbl, "hb_local"))

  hbl |>
    collapse::rsplit(
      by = ~ cluster_id,
      use.names = FALSE,
      simplify = FALSE,
      keep.by = TRUE
    ) |>
    lapply(ca_to_cluster_wrap,
           ca_schedules_df = ca_schedules_df,
           height_thresh_cm = height_thresh_cm,
           sowing_day = sowing_day) |>
    data.table::rbindlist() |>
    as.data.frame()
}

ca_to_cluster_wrap <- function(
    cluster_hbl_df,
    ca_schedules_df,
    height_thresh_cm,
    sowing_day
    )
{
  ca_to_cluster_fun <- function(
    application_day, amount, application_type, previous_applications
    )
  {
    ca_to_cluster(date = cluster_hbl_df$date,
                  real_height_cm = cluster_hbl_df$real_height_cm,
                  irrigation = cluster_hbl_df$irrigation,
                  draining = cluster_hbl_df$draining,
                  plan_delay = cluster_hbl_df$plan_delay,
                  application_day = application_day,
                  amount = amount,
                  application_type = application_type,
                  height_thresh_cm = height_thresh_cm,
                  previous_applications = previous_applications,
                  sowing_day = sowing_day
                  )
  }

  variety <- cluster_hbl_df$variety[[1]]
  applications_df <- ca_schedules_df |> (\(.) .[.$rice_variety == variety, ])()

  res <- cluster_hbl_df
  for (chemical in unique(ca_schedules_df$chemical))
    res[[chemical]] <- 0

  for (i in 1:nrow(applications_df)) {
    res[[ applications_df$chemical[[i]] ]] <-
      ca_to_cluster_fun(
        application_day = applications_df$day[[i]],
        amount = applications_df$amount[[i]],
        application_type = applications_df$application_type[[i]],
        previous_applications = res[[ applications_df$chemical[[i]] ]]
      )
  }



  return(res)
}

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
                          sowing_day)
{
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
                                   sowing_day)
{
  tryCatch(
    {
      assert_date(date)
      assert_positive_vector(real_height_cm)
      assert_logical(irrigation)
      assert_logical(draining)
      assert_positive_vector(plan_delay)
      assert_integer_vector(plan_delay)
      assert_positive_vector(previous_applications)

      n_lengths <- sapply(
        list(date, real_height_cm, irrigation, draining, plan_delay, previous_applications),
        length
        ) |> unique()
      if (n_lengths > 1)
        stop("Inputs have mismatched lengths.")

      assert_positive_integer(application_day)
      assert_positive_number(amount)

      assert_positive_number(height_thresh_cm)

      assert_string(sowing_day)
      sowing_day <- paste0("2000-", sowing_day)
      assert_date(sowing_day)
    },
    error = function(e) {
      class(e) <- c("ca_to_cluster_argcheck_error", class(e))
      stop(e)
    }
  )
}





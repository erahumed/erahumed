#' Chemicals Applications
#'
#' @description
#' Simulates the application of chemicals to rice paddies, given a previous
#' calculation of local hydrological balance as input.
#'
#' @param hbl an object of class `"hb_local"` - see \link{hb_local}.
#' @param ca_schedules_df a `data.frame` following the template of
#' \link{albufera_ca_schedules}.
#' @param height_thresh_cm a positive number. Upper limit of paddy water levels
#' required for ground applications of chemicals. Expressed in centimeters.
#'
#' @return an object of class `"erahumed_ca"`. This is essentially a
#' `data.frame` similar to the output of \link{hb_local}, with additional
#' columns named as the chemicals listed in `ca_schedules_df`. Each of these
#' additional columns provides the (daily) time series of chemicals
#' applications.
#'
#' @details
#' TODO: Document columns of returned df #72
#'
#' @export
ca <- function(hbl,
               ca_schedules_df = erahumed::albufera_ca_schedules,
               height_thresh_cm = 2)
{
  ca_argcheck(hbl, ca_schedules_df, height_thresh_cm)

  hbl$year <- format(hbl$date, "%Y") |> as.numeric()


  hbl |>
    collapse::rsplit(
      by = ~ cluster_id + year,
      flatten = TRUE,
      use.names = FALSE,
      simplify = FALSE,
      keep.by = TRUE
    ) |>
    lapply(ca_to_cluster_wrap,
           ca_schedules_df = ca_schedules_df,
           height_thresh_cm = height_thresh_cm) |>
    data.table::rbindlist() |>
    as.data.frame() |>
    make_erahumed_ca()
}



ca_argcheck <- function(hbl, ca_schedules_df, height_thresh_cm)
{
  tryCatch({
    stopifnot(inherits(hbl, "hb_local"))
    assert_data.frame(ca_schedules_df,
                      template = erahumed::albufera_ca_schedules)
    assert_positive_number(height_thresh_cm)
  },
  error = function(e) {
    class(e) <- c("ca_argcheck_error", class(e))
    stop(e)
  })
}


ca_to_cluster_wrap <- function(cluster_hbl_df,
                               ca_schedules_df,
                               height_thresh_cm)
{
  ca_to_cluster_fun <- function(
    application_day,
    amount,
    application_type,
    previous_applications
    )
  {
    ca_to_cluster(real_height_cm = cluster_hbl_df$real_height_cm,
                  seed_day = cluster_hbl_df$seed_day,
                  irrigation = cluster_hbl_df$irrigation,
                  draining = cluster_hbl_df$draining,
                  plan_delay = cluster_hbl_df$plan_delay,
                  application_day = application_day,
                  amount = amount,
                  application_type = application_type,
                  height_thresh_cm = height_thresh_cm,
                  previous_applications = previous_applications)

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

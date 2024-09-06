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
#' @param sowing_mmdd a string formatted as `"%m-%d"`, where `%m` and `%d`
#' represent month and day, respectively. Day of the year at which sowing
#' occurs.
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
               height_thresh_cm = 2.5,
               sowing_mmdd = "04-20")
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
           sowing_mmdd = sowing_mmdd) |>
    data.table::rbindlist() |>
    as.data.frame() |>
    make_erahumed_ca()
}



ca_to_cluster_wrap <- function(
    cluster_hbl_df,
    ca_schedules_df,
    height_thresh_cm,
    sowing_mmdd
    )
{
  ca_to_cluster_fun <- function(
    sowing_yyyy,
    application_day,
    amount,
    application_type,
    previous_applications
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
                  sowing_mmdd = sowing_mmdd,
                  sowing_yyyy = sowing_yyyy
                  )
  }

  variety <- cluster_hbl_df$variety[[1]]
  applications_df <- ca_schedules_df |> (\(.) .[.$rice_variety == variety, ])()

  years <- format(cluster_hbl_df$date, "%Y") |> unique()

  res <- cluster_hbl_df
  for (chemical in unique(ca_schedules_df$chemical))
    res[[chemical]] <- 0

  for (year in years)
    for (i in 1:nrow(applications_df)) {
      res[[ applications_df$chemical[[i]] ]] <-
        ca_to_cluster_fun(
          sowing_yyyy = year,
          application_day = applications_df$day[[i]],
          amount = applications_df$amount[[i]],
          application_type = applications_df$application_type[[i]],
          previous_applications = res[[ applications_df$chemical[[i]] ]]
        )
    }

  return(res)
}

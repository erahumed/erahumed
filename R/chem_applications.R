chem_apply_cluster <- function(date,
                               real_height_cm,
                               irrigation,
                               draining,
                               plan_delay,
                               application_days,
                               amounts,
                               sowing_day = "04-20"
)
{
  return(numeric(length(date)))  # TODO
}

chem_apply <- function(
    hbl,
    management_chemicals_df = erahumed::albufera_management_chemicals
    )
{
  stopifnot(inherits(hbl, "hb_local"))

  hbl |>
    collapse::rsplit(
      by = ~ cluster_id,
      use.names = FALSE,
      simplify = FALSE,
      keep.by = TRUE
    ) |>
    lapply(\(df){

      variety <- df$variety[[1]]
      chemicals <- unique(df$chemicals)

      FUN <- function(application_days, amounts) {
        chem_apply_cluster(date = df$date,
                           real_height_cm = df$real_height_cm,
                           irrigation = df$irrigation,
                           draining = df$draining,
                           plan_delay = df$plan_delay,
                           application_days = application_days,
                           amounts = amounts)
      }

      for (chemical in chemicals) {
        df1 <- df[df$variety == variety & df$chemical == chemical, ]
        df[applications$chemical] <- FUN(application_days = df1$day,
                                         amounts = df1$amount)
      }

      df
    }) |>
    data.table::rbindlist() |>
    as.data.frame()
}

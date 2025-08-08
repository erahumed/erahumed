get_application_kg_vector <- function(
    chemical, rfms_id, applications_df, seed_day, plan_delay, area_m2
) {
  res <- numeric(length(seed_day))

  # Find start indices of each year (first seed_day of each year)
  year_starts <- which(c(TRUE, diff(seed_day < 0) == 1))

  for (k in seq_len(nrow(applications_df))) {
    if (applications_df$chemical_id[[k]] != chemical)
      next
    if (applications_df$rfms_id[[k]] != rfms_id)
      next

    app_seed_day <- applications_df$seed_day[[k]]

    for (y in seq_along(year_starts)) {
      start <- year_starts[y]
      end <- if (y < length(year_starts)) year_starts[y + 1] - 1 else length(seed_day)

      # Adjusted seed day vector for this year
      year_seed_days <- seed_day[start:end]
      year_plan_delays <- plan_delay[start:end]

      idx <- which.max(year_seed_days - year_plan_delays == app_seed_day)
      if (idx > 0) {
        res[start + idx - 1] <- applications_df$amount_kg_ha[[k]] * area_m2 * 1e-4
      }
    }
  }

  return(res)
}

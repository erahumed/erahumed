get_application_kg_vector <- function(
    chemical, rfms_id, applications_df, seed_day, plan_delay, area_m2
)
{
  res <- numeric(length(seed_day))

  for (k in seq_len(nrow(applications_df))) {
    if (applications_df$chemical_id[[k]] != chemical)
      next
    if (applications_df$rfms_id[[k]] != rfms_id)
      next
    application_seed_day <- applications_df$seed_day[[k]]
    idx <- which.max(seed_day - plan_delay == application_seed_day)
    res[idx] <- applications_df$amount_kg_ha[[k]] * area_m2 * 1e-4
  }
  return(res)
}

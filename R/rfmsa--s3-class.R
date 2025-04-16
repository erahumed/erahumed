rice_field_management_scheme <- function() {
  allocations_df <-
    info_clusters()[, c("element_id", "ditch_element_id", "tancat")]

  allocations_df$variety <- NA
  res <- list(varieties = list(), allocations = allocations_df)

  class(res) <- "erahumed_rfms"

  return(res)
}

allocate_rice_variety <- function(
  rfms,
  variety,
  target_fraction,
  ditches = paste0("d", 1:26),  # TODO Make this more transparent
  field_type = c("both", "regular", "tancat")
  )
{
  # TODO: arg validation

  max_target_fraction <- rfmc_max_target_fraction(ditches, field_type)
  if (target_fraction > max_target_fraction) {
    msg <- paste0(
      "Target fraction of crop surface is greater",
      "than the unallocated surface for the specified 'field_type' and",
      "'ditches'. Allocation has been reduced to ",
      format(100 * max_target_fraction, digits = 1), "%."
    )
    warning(msg)
    target_fraction <- max_target_fraction
  }
}

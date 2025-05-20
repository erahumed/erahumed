allocate_rfms <- function(rfa,
                          rfms,
                          target_fraction,
                          ditches = paste0("d", 1:26),
                          field_type = c("both", "regular", "tancat")
)
{
  tryCatch(
    {
      assert_rfa(rfa)
      assert_rfms(rfms)
      assert_positive_number(target_fraction)
      stopifnot(target_fraction <= 1)
      assert_character(ditches)
      field_type <- match.arg(field_type)
    },
    error = function(e) {
      class(e) <- c("erahumed_allocate_rfms_error", class(e))
      stop(e)
    })

  rfa[["rfms"]] <- c(rfa[["rfms"]], list(rfms))
  rfms_id <- length(rfa[["rfms"]])

  allocated_fraction <- 0
  while (allocated_fraction < target_fraction) {
    candidates <- rfa_candidates(rfa, ditches, field_type)
    if (length(candidates) == 0)
      break
    cluster_id <- sample(candidates, 1)
    rfa <- rfa_assign(rfa, cluster_id = cluster_id, rfms_id = rfms_id)

    allocated_fraction <- allocated_fraction + rfa_cluster_fraction(cluster_id)
  }

  return(rfa)
}

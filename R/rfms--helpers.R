get_applications_df0 <- function(system) {
  lapply(seq_along(system$applications), function(i) {
    a <- system$applications[[i]]
    data.frame(chemical_id = i,
               amount_kg_ha = a$amount_kg_ha,
               seed_day = a$seed_day,
               type = a$type)
    }) |>
    Reduce(rbind, x = _, init = data.frame())
}

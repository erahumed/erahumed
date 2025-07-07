get_applications_df0 <- function(system, chemical_db) {
  lapply(seq_along(system$applications), function(i) {
    a <- system$applications[[i]]
    data.frame(chemical_id = match_chemical(a$chemical, db = chemical_db),
               amount_kg_ha = a$amount_kg_ha,
               seed_day = a$seed_day,
               type = a$type)
    }) |>
    Reduce(rbind, x = _, init = data.frame())
}

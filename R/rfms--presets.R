#' @rdname rfms
#' @export
jsendra_rfms <- function() {
  new_rfms() |>
    add_application(chemical = acetamiprid(),
                    seed_day = 52,
                    amount_kg_ha = 0.03,
                    type = "ground",
                    emptying_days = 6) |>
    add_application(chemical = bentazone(),
                    seed_day = 52,
                    amount_kg_ha = 1,
                    type = "ground",
                    emptying_days = 6) |>
    add_application(chemical = mcpa(),
                    seed_day = 52,
                    amount_kg_ha = 0.50,
                    type = "ground",
                    emptying_days = 6) |>
    add_application(chemical = penoxsulam(),
                    seed_day = 18,
                    amount_kg_ha = 0.04,
                    type = "ground",
                    emptying_days = 2) |>
    add_application(chemical = cyhalofop_butyl(),
                    seed_day = 5,
                    amount_kg_ha = 0.30,
                    type = "ground",
                    emptying_days = 2) |>
    add_application(chemical = cyhalofop_butyl(),
                    seed_day = 18,
                    amount_kg_ha = 0.30,
                    type = "ground",
                    emptying_days = 2) |>
    add_application(chemical = azoxystrobin(),
                    seed_day = 76,
                    amount_kg_ha = 0.20,
                    type = "aerial") |>
    add_application(chemical = azoxystrobin(),
                    seed_day = 90,
                    amount_kg_ha = 0.20,
                    type = "aerial") |>
    add_application(chemical = difenoconazole(),
                    seed_day = 76,
                    amount_kg_ha = 0.13,
                    type = "aerial") |>
    add_application(chemical = difenoconazole(),
                    seed_day = 90,
                    amount_kg_ha = 0.13,
                    type = "aerial")
}

#' @rdname rfms
#' @export
bomba_rfms <- function() {
  jsendra_rfms() |>
    add_application(chemical = azoxystrobin(),
                    seed_day = 104,
                    amount_kg_ha = 0.20,
                    type = "aerial") |>
    add_application(chemical = difenoconazole(),
                    seed_day = 104,
                    amount_kg_ha = 0.13,
                    type = "aerial")
}

#' @rdname rfms
#' @export
clearfield_rfms <- function() {
  new_rfms() |>
    add_application(chemical = acetamiprid(),
                    seed_day = 52,
                    amount_kg_ha = 0.03,
                    type = "ground",
                    emptying_days = 6) |>
    add_application(chemical = cycloxydim(),
                    seed_day = 18,
                    amount_kg_ha = 0.30,
                    type = "ground",
                    emptying_days = 2) |>
    add_application(chemical = cycloxydim(),
                    seed_day = 52,
                    amount_kg_ha = 0.30,
                    type = "ground",
                    emptying_days = 6) |>
    add_application(chemical = azoxystrobin(),
                    seed_day = 76,
                    amount_kg_ha = 0.20,
                    type = "aerial") |>
    add_application(chemical = azoxystrobin(),
                    seed_day = 90,
                    amount_kg_ha = 0.20,
                    type = "aerial") |>
    add_application(chemical = difenoconazole(),
                    seed_day = 76,
                    amount_kg_ha = 0.13,
                    type = "aerial") |>
    add_application(chemical = difenoconazole(),
                    seed_day = 90,
                    amount_kg_ha = 0.13,
                    type = "aerial")
}

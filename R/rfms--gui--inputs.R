rfms_input_sowing_yday <- function(id) {
  inline_numeric_input(
    id,
    label = rfms_input_label("sowing_yday"),
    value = rfms_input_defaults()$sowing_yday,
    min = 1, max = 366
  )
}

rfms_input_perellona_start_yday <- function(id) {
  inline_numeric_input(
    id,
    label = rfms_input_label("perellona_start_yday"),
    value = rfms_input_defaults()$perellona_start_yday,
    min = 1, max = 366
  )
}

rfms_input_perellona_end_yday <- function(id) {
  inline_numeric_input(
    id,
    label = rfms_input_label("perellona_end_yday"),
    value = rfms_input_defaults()$perellona_end_yday,
    min = 1, max = 366
  )
}

rfms_input_harvesting_yday <- function(id) {
  inline_numeric_input(
    id,
    label = rfms_input_label("harvesting_yday"),
    value = rfms_input_defaults()$harvesting_yday,
    min = 1, max = 366
  )
}

rfms_input_flow_height_cm <- function(id) {
  inline_numeric_input(
    id,
    label = rfms_input_label("flow_height_cm"),
    value = rfms_input_defaults()$flow_height_cm,
    min = 0
  )
}

rfms_input_perellona_height_cm <- function(id) {
  inline_numeric_input(
    id,
    label = rfms_input_label("perellona_height_cm"),
    value = rfms_input_defaults()$perellona_height_cm,
    min = 0
  )
}

rfms_input_chemical_id <- function(id, choices) {
  inline_select_input(id, "Select chemical", choices = choices)
}

rfms_input_seed_day <- function(id, max) {
  inline_numeric_input(
    id,
    label = application_input_label("seed_day"),
    value = NA,
    min = 1,
    max = max
  )
}

rfms_input_amount_kg_ha <- function(id) {
  inline_numeric_input(
    id,
    label = application_input_label("amount_kg_ha"),
    value = NA,
    min = 0
  )
}

rfms_input_type <- function(id) {
  inline_select_input(
    id,
    label = application_input_label("type"),
    choices = c("ground", "aerial"),
    selected = NA
  )
}

rfms_input_emptying_days <- function(id) {
  inline_numeric_input(
    id,
    label = application_input_label("emptying_days"),
    value = NA,
    min = 1,
    step = 1
  )
}


rfms_input_defaults <- function() {
  fmls <- formals(new_rfms)
  list(
    sowing_yday = eval(fmls$sowing_yday),
    harvesting_yday = eval(fmls$harvesting_yday),
    perellona_end_yday = eval(fmls$perellona_end_yday),
    perellona_start_yday = eval(fmls$perellona_start_yday),
    flow_height_cm = eval(fmls$flow_height_cm),
    perellona_height_cm = eval(fmls$perellona_height_cm)
  )
}

get_proto_applications <- function(rfms, chemical_db) {
  lapply(rfms$applications, function(app) {
    list(
      chemical_name = app$chemical$display_name,
      chemical_id = chemical_db$ids[match_chemical(app$chemical, chemical_db$items)],
      amount_kg_ha = app$amount_kg_ha,
      seed_day = app$seed_day,
      type = app$type,
      emptying_days = app$emptying_days
    )
  } )
}

rfms_input_tooltip <- function(param) {
  bslib::tooltip(
    trigger = shiny_icon("question-circle"),
    get_param_desc(param, fun = "rfms", strip_roxy = TRUE),
    placement = "right"
  )
}

application_input_tooltip <- function(param) {
  bslib::tooltip(
    trigger = shiny_icon("question-circle"),
    get_param_desc(param, fun = "application", strip_roxy = TRUE),
    placement = "right"
  )
}

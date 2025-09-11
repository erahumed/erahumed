dss_input_tooltip <- function(param) {
  bslib::tooltip(
    trigger = shiny_icon("question-circle"),
    get_param_desc(param, fun = "simulation", strip_roxy = TRUE),
    placement = "right"
    )
}

dss_input_label <- function(param) {
  name <- get_param_name(param, fun = "simulation")
  unit <- get_param_unit(param, fun = "simulation")
  lbl <- paste0(name, " [", unit, "]")

  shiny::p(lbl, dss_input_tooltip(param))
}

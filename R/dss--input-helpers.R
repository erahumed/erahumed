dss_input_tooltip <- function(param) {
  bslib::tooltip(
    trigger = shiny_icon("question-circle"),
    get_param_desc(param, fun = "simulation", strip_roxy = TRUE),
    placement = "right"
    )
}

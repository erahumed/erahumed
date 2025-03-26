dss_input_tooltip <- function(param) {
  bslib::tooltip(
    trigger = shiny_icon("question-circle"),
    erahumed_input_desc(param, strip_roxy = TRUE),
    placement = "right"
    )
}

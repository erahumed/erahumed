default_chemical_db <- function() {
  list(
    acetamiprid(),
    azoxystrobin(),
    bentazone(),
    cycloxydim(),
    cyhalofop_butyl(),
    difenoconazole(),
    mcpa(),
    penoxsulam()
  )
}

chem_input_tooltip <- function(param) {
  bslib::tooltip(
    trigger = shiny_icon("question-circle"),
    get_param_desc(param, fun = "chemical", strip_roxy = TRUE),
    placement = "right"
  )
}


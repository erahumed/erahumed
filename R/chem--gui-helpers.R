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
    chemical_prop_desc(param, strip_roxy = TRUE),
    placement = "right"
  )
}


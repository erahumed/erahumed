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
  input_tooltip(param, fun = "chemical")
}

chem_input_label <- function(param) {
  input_label(param, fun = "chemical")
}

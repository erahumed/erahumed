allocation_input_tooltip <- function(name) {
  bslib::tooltip(
    trigger = shiny_icon("question-circle"),
    allocation_param_desc(name, strip_roxy = TRUE),
    placement = "right"
  )
}

allocation_input_defaults <- function() {
  list(
    target_fraction = 0.1,
    field_type = "both",
    ditches = c(1, 26)
  )
}

allocation_input_system <- function(id, choices) {
  inline_select_input(
    inputId = id,
    label = shiny::p("System to allocate"), # No tooltip since this isn't in YAML
    choices = choices
  )
}

allocation_input_target_fraction <- function(id) {
  inline_numeric_input(
    id,
    label = shiny::p(
      shiny::HTML("Target fraction"),
      allocation_input_tooltip("target_fraction")
    ),
    value = allocation_input_defaults()[["target_fraction"]],
    min = 0, max = 1, step = 0.01
  )
}

allocation_input_field_type <- function(id) {
  inline_select_input(
    inputId = id,
    label = shiny::p(
      shiny::HTML("Field type"),
      allocation_input_tooltip("field_type")
    ),
    choices = c("both", "regular", "tancat"),
    selected = allocation_input_defaults()[["field_type"]]
  )
}

allocation_input_ditches <- function(id) {
  shiny::sliderInput(
    inputId = id,
    label = shiny::p(
      shiny::HTML("Ditches"),
      allocation_input_tooltip("ditches")
    ),
    min = 1, max = 26,
    value = allocation_input_defaults()[["ditches"]],
    step = 1
  )
}

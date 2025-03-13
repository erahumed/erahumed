hbd_input_ui <- function(id) {
  ns <- shiny::NS(id)

  tltp <- function(param) param_tooltip(layer = "hbd", param = param)

  shiny::tagList(
    shiny::numericInput(ns("ditch_level_m"),
                        shiny::p("Ditch Level [m]", tltp("ditch_level_m")),
                        value = eval(formals(erahumed_simulation)$ditch_level_m),
                        min = 0,
                        max = 2,
                        step = 0.01
    )
  )
}

hbd_input_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::reactive({ list(ditch_level_m = input$ditch_level_m) })

  })
}

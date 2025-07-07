library(erahumed)

shiny::shinyApp(
  ui = function() {
    bslib::page_fillable(
      erahumed:::chemical_db_ui("mock") |> shiny::tags$style(display = "none"),
      erahumed:::rfms_ui("test")
    )
  },

  server = function(input, output, session) {
    chemical_db <- erahumed:::chemical_db_server("mock")
    erahumed:::rfms_server("test", chemical_db = chemical_db, initial_rfms = jsendra())
  }
)

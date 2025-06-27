library(erahumed)

shiny::shinyApp(
  ui = function() {
    bslib::page_fillable(
      erahumed:::chemical_db_ui("mock") |> shiny::tags$style(display = "none"),
      erahumed:::rfms_db_ui("db")
    )
  },

  server = function(input, output, session) {
    chemical_db <- erahumed:::chemical_db_server("mock")
    erahumed:::rfms_db_server("db", chemical_db = chemical_db)
  }
)

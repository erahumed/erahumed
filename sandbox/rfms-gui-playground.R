shiny::shinyApp(
  ui = function()
    erahumed:::rfms_ui("test"),
  server = function(input, output, session) {
    chemical_db <- erahumed:::chemical_db_server("mock")
    erahumed:::rfms_server("test", chemical_db = chemical_db)
  }

)

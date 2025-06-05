
shiny::shinyApp(
  ui = function()
    erahumed:::chemical_db_ui("test"),
  server = function(input, output, session)
    erahumed:::chemical_db_server("test")
)

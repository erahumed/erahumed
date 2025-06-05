shiny::shinyApp(
  ui = function()
    erahumed:::rfms_ui("test"),
  server = function(input, output, session)
    erahumed:::rfms_server("test")
)

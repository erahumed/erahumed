library(erahumed)

shiny::shinyApp(
  ui = function() {
    bslib::page_fillable(
      erahumed:::rfcm_ui("test")
    )
  },

  server = function(input, output, session) {
    erahumed:::rfcm_server("test")
  }
)

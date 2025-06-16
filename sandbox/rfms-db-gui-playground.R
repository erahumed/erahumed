library(erahumed)

shiny::shinyApp(
  ui = function() {
    bslib::page_fillable(
      erahumed:::rfcm_ui("test"),
      erahumed:::rfms_db_ui("db")
    )
  },

  server = function(input, output, session) {
    rfms_db <- erahumed:::rfms_db_server("db")
    erahumed:::rfcm_server("test", rfms_db)
  }
)

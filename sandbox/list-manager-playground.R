shiny::shinyApp(
  ui = function()
    erahumed:::list_manager_ui("test"),
  server = function(input, output, session) {
    item_module_ui <- function(id, item = NULL) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shiny::textInput(ns("name"), "Name", value = item$name),
        shiny::numericInput(ns("value"), "Value", value = item$value)
        )
    }

    item_module_server <- function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        shiny::reactive(list(name = input$name, value = input$value))
      })
    }

    erahumed:::list_manager_server("test",
                               item_module_ui,
                               item_module_server,
                               display_function = function(item) item$name,
                               default_items = list())
  }

)

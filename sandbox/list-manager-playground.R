library(shiny)

item_editor_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("name"), "Name", value = NA),
    numericInput(ns("value"), "Value", value = NA)
  )
}

item_editor_server <- function(id, item = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(item())

      updateTextInput(inputId = "name", value = item()$name)
      updateNumericInput(inputId = "value", value = item()$value)

    })

    return( reactive(list(name = input$name, value = input$value)) )
  })
}

shinyApp(
  ui = function()
    erahumed:::list_manager_ui("test"),
  server =
    function(input, output, session)
      erahumed:::list_manager_server("test",
                                     item_editor_ui,
                                     item_editor_server,
                                     item_display_function = function(item) item$name,
                                     default_items = list())
)

library(shiny, quietly = TRUE)

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

args <- list(
  item_editor_ui = item_editor_ui,
  item_editor_server = item_editor_server,
  item_display_function = function(item) item$name,
  default_items = list(list(name = "a", value = 1), list(name = "b", value = 2))
  )

test_that("UI succeeds", {
  expect_no_error(list_manager_ui("ui"))
})

library(shiny, quietly = TRUE)

test_that("Server runs without error", {
  expect_no_error( shiny::testServer(list_manager_server, {}, args = args) )
})

test_that("Server returns without error", {
  shiny::testServer(list_manager_server, {
    expect_no_error(session$returned)
  }, args = args)
})

test_that("Server returns a reactive expression", {
  shiny::testServer(list_manager_server, {
    expect_s3_class(session$returned, class(reactiveVal()))
  }, args = args)
})

test_that("Return contains by default the list of predefined chemicals", {
  shiny::testServer(list_manager_server, {
    res <- session$returned
    expect_identical(res()[["items"]], args$default_items)
  }, args = args)
})

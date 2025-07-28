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
    for (i in seq_along(args$default_items)) {
      expect_identical(res()[["items"]][[i]], args$default_items[[i]])
    }
  }, args = args)
})

test_that("Can add an item", {
  shiny::testServer(list_manager_server, {
    session$setInputs(add_item = 1)
    session$flushReact()

    session$setInputs("editor-name" = "new", "editor-value" = 10)
    session$setInputs(save_item_btn = 1)
    session$flushReact()

    expect_equal(length(session$returned()$items), 3)
    expect_identical(session$returned()$items[[3]]$name, "new")
  }, args = args)
})

test_that("Can edit an item", {
  shiny::testServer(list_manager_server, {
    session$setInputs(edit_idx = 1, edit_trigger = 1)
    session$flushReact()

    session$setInputs("editor-name" = "edited", "editor-value" = 99)
    session$setInputs(save_item_btn = 1)
    session$flushReact()

    expect_identical(session$returned()$items[[1]]$name, "edited")
    expect_equal(session$returned()$items[[1]]$value, 99)
  }, args = args)
})

test_that("Can duplicate an item", {
  shiny::testServer(list_manager_server, {
    # Initial items
    expect_equal(length(session$returned()$items), 2)

    # Trigger duplication of the first item
    session$setInputs(duplicate_idx = 1)
    session$setInputs(duplicate_trigger = 1)
    session$flushReact()

    # Should now have 3 items
    items <- session$returned()$items
    expect_equal(length(items), 3)

    # The last item should be identical to the first one
    expect_equal(items[[3]], items[[1]])
  }, args = args)
})


test_that("Can delete an item", {
  shiny::testServer(list_manager_server, {
    session$setInputs(delete_idx = 1, delete_trigger = 1)
    session$flushReact()

    session$setInputs(confirm_delete = 1)
    session$flushReact()

    expect_equal(length(session$returned()$items), 1)
    expect_identical(session$returned()$items[[1]]$name, "b")
  }, args = args)
})


test_that("Can delete an item", {
  shiny::testServer(list_manager_server, {
    session$setInputs(delete_idx = 1, delete_trigger = 1)
    session$flushReact()

    session$setInputs(confirm_delete = 1)
    session$flushReact()

    expect_equal(length(session$returned()$items), 1)
    expect_identical(session$returned()$items[[1]]$name, "b")
  }, args = args)
})

test_that("Canceling delete does not delete the item", {
  shiny::testServer(list_manager_server, {
    # Initially 2 items
    expect_equal(length(session$returned()$items), 2)

    # Trigger delete on item 1
    session$setInputs(delete_idx = 1)
    session$setInputs(delete_trigger = 1)
    session$flushReact()

    # If user does not click on the confirm button nothing should be deleted
    expect_equal(length(session$returned()$items), 2)
  }, args = args)
})


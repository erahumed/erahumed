list_manager_ui <- function(
    id,
    object_name = "Item",
    plural_name = paste0(object_name, "s"),
    list_description = "list"
    )
{
  ns <- shiny::NS(id)

  # Used to trigger list edits and deletes from dynamically created button,
  # avoiding anti-patterns such as nested observers.
  counters_js <- shiny::tags$script(shiny::HTML(
    "window.edit_counter = 0; window.delete_counter = 0; window.duplicate_counter = 0;"))

  add_item_btn <- shiny::actionButton(ns("add_item"), "Add new", icon = shiny::icon("plus"))

  list_output <- shiny::uiOutput(ns("list_output"))

  shiny::tagList(
    shinyjs::useShinyjs(),
    counters_js,
    list_output,
    add_item_btn
  )
}

list_manager_server <- function(id,
                                item_editor_ui,
                                item_editor_server,
                                item_display_function,
                                default_items = list()
                                )
{
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    db <- shiny::reactiveVal(list(items = default_items,
                                  ids = seq_along(default_items)
                                  ))
    item_counter <- shiny::reactiveVal(length(default_items))
    editing_item <- shiny::reactiveVal(NULL)

    onclick_js <- function(idx_id, trigger_id, counter_id) {
      function(i) {
        paste(
          sprintf("%s++;", counter_id),
          sprintf("Shiny.setInputValue('%s', %s);", ns(idx_id), i),
          sprintf("Shiny.setInputValue('%s', %s);", ns(trigger_id), counter_id)
        )
      }
    }

    edit_onclick_js <- onclick_js("edit_idx", "edit_trigger", "window.edit_counter")
    duplicate_onclick_js <- onclick_js("duplicate_idx", "duplicate_trigger", "window.duplicate_counter")
    delete_onclick_js <- onclick_js("delete_idx", "delete_trigger", "window.delete_counter")

    output$list_output <- shiny::renderUI({
      if (length(db()$items) == 0) {
        shiny::tags$em("No items yet.")
      } else {
        shiny::tags$ul(
          class = "list-unstyled d-flex flex-wrap gap-2",
          style = "padding-left: 0;",
          Map(function(item, id) {
            shiny::tags$li(
              shiny::tags$div(
                class = "d-inline-flex align-items-center border rounded-pill px-3 py-1 bg-light",
                style = "gap: 0.5rem;",
                shiny::span(item_display_function(item)),
                shiny::div(
                  class = "btn-group btn-group-sm",
                  shiny::actionLink(
                    ns(paste0("edit_", id)),
                    label = NULL,
                    icon = shiny::icon("pen"),
                    class = "btn btn-outline-primary btn-sm",
                    onclick = edit_onclick_js(id),
                    title = "Edit"
                  ),
                  shiny::actionLink(
                    ns(paste0("duplicate_", id)),
                    label = NULL,
                    icon = shiny::icon("copy"),
                    class = "btn btn-outline-secondary btn-sm",
                    onclick = duplicate_onclick_js(id),
                    title = "Duplicate"
                  ),
                  shiny::actionLink(
                    ns(paste0("delete_", id)),
                    label = NULL,
                    icon = shiny::icon("trash"),
                    class = "btn btn-outline-danger btn-sm",
                    onclick = delete_onclick_js(id),
                    title = "Delete"
                  )
                )
              )
            )
          }, db()$items, db()$ids)
        )
      }
    })


    open_editor_modal <- function(title) {
      shiny::showModal(
        session = session,
        shiny::modalDialog(
          size = "xl",
          title = title,
          item_editor_ui(ns("editor")),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(ns("save_item"), "Save")
          ),
        ))
      }

    shiny::observe({
      editing_item(NULL)
      shinyjs::runjs(sprintf("Shiny.setInputValue('%s', null)", ns("edit_idx")))

      open_editor_modal(title = "Add New Item")
    }) |>
      shiny::bindEvent(input$add_item)

    shiny::observe({
      i <- match(input$edit_idx, db()$ids)
      editing_item(db()$items[[i]])
      open_editor_modal(title = "Edit Item")
      }) |>
      shiny::bindEvent(input$edit_trigger, ignoreNULL = TRUE)

    edited_item <- item_editor_server("editor", item = editing_item)

    shiny::observe({

      replacement <- tryCatch(
        edited_item(),
        error = function(e) {
          shiny::showNotification(paste("Definition error:", e$message), type = "error")
          cat(e$message)
          shiny::req(FALSE)
        }
      )

      db_copy <- db()

      if (is.null(input$edit_idx)) {  # i.e. we are adding a new item
        item_counter(item_counter() + 1)
        db_copy$ids <- c(db_copy$ids, item_counter())
        i <- length(db_copy$items) + 1
      } else {
        i <- match(input$edit_idx, db_copy$ids)
      }

      db_copy$items[[i]] <- replacement

      db(db_copy)

      shiny::removeModal(session = session)
    }) |>
      shiny::bindEvent(input$save_item, ignoreInit = TRUE, ignoreNULL = TRUE)

    shiny::observe({
      i <- match(input$duplicate_idx, db()$ids)
      item_to_copy <- db()$items[[i]]

      db_copy <- db()
      item_counter(item_counter() + 1)
      db_copy$ids <- c(db_copy$ids, item_counter())
      db_copy$items <- c(db_copy$items, list(item_to_copy))
      db(db_copy)
    }) |> shiny::bindEvent(input$duplicate_trigger, ignoreNULL = TRUE)

    shiny::observe({
      shiny::showModal(shiny::modalDialog(
        title = "Confirm Deletion",
        "Are you sure you want to delete this item?",
        size = "s",
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("confirm_delete"), "Delete", class = "btn-danger")
        )
      ))
    }) |>
      shiny::bindEvent(input$delete_trigger, ignoreNULL = TRUE, ignoreInit = TRUE)

    shiny::observe({
      shiny::removeModal(session = session)

      db_copy <- db()

      i <- match(input$delete_idx, db_copy$ids)

      db_copy$items[[i]] <- NULL
      db_copy$ids <- db_copy$ids[-i]
      db(db_copy)

      shiny::removeModal(session = session)
    }) |>
      shiny::bindEvent(input$confirm_delete, ignoreNULL = TRUE, ignoreInit = TRUE)

    return(db)
  })
}

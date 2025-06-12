list_manager_ui <- function(
    id,
    object_name = "Item",
    plural_name = paste0(object_name, "s"),
    list_description = "list"
    )
{
  ns <- shiny::NS(id)

  title <- paste(object_name, list_description)
  list_output_header <- shiny::tags$h4(plural_name)

  # Used to trigger list edits and deletes from dynamically created button,
  # avoiding anti-patterns such as nested observers.
  counters_js <- shiny::tags$script(shiny::HTML(
    "window.edit_counter = 0; window.delete_counter = 0;"))

  add_item_btn <- shiny::actionButton(ns("add_item"), paste("Add", object_name))

  bslib::page_fillable(
    counters_js,
    shinyjs::useShinyjs(),

    # UI elements
    title = title,
    add_item_btn,
    list_output_header,
    shiny::uiOutput(ns("list_output"))
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

    db <- shiny::reactiveVal(list(
      items = default_items,
      ids = seq_along(default_items)
      ))
    item_counter <- shiny::reactiveVal(length(default_items))

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
    delete_onclick_js <- onclick_js("delete_idx", "delete_trigger", "window.delete_counter")

    output$list_output <- shiny::renderUI({
      if (length(db()$items) == 0) {
        shiny::tags$em("No items yet.")
      } else {
        shiny::tags$ul(
          lapply(seq_along(db()$items), function(i) {
            shiny::tags$li(
              item_display_function(db()$items[[i]]),
              shiny::actionLink(ns(paste0("edit_", i)), "Edit", onclick = edit_onclick_js(i)),
              " | ",
              shiny::actionLink(ns(paste0("delete_", i)), "Delete", onclick = delete_onclick_js(i))
            )
          })
        )
      }
    })

    open_editor_modal <- function(title, item = NULL) {
      shiny::showModal(
        session = session,
        shiny::modalDialog(
          title = title,
          item_editor_ui(ns("editor"), item = item),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(ns("save_item"), "Save")
          ),
        ))
      }

    shiny::observe({
      shinyjs::runjs(
        sprintf("Shiny.setInputValue('%s', null)", ns("edit_idx"))
        )
      item_counter(item_counter() + 1)

      open_editor_modal(title = "Add New Item")
    }) |> shiny::bindEvent(input$add_item)

    shiny::observe(
      open_editor_modal(title = "Edit Item",
                        item = db()$items[[input$edit_idx]])
      ) |>
      shiny::bindEvent(input$edit_trigger, ignoreNULL = TRUE)

    # Mount the editor module
    edited_item <- item_editor_server("editor")

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

      i <- input$edit_idx
      if (is.null(i)) {
        item_counter(item_counter() + 1)
        i <- length(db_copy$items) + 1
        db_copy$ids <- c(db_copy$ids, item_counter())
      }
      db_copy$items[[i]] <- replacement

      db(db_copy)

      shiny::removeModal(session = session)
    }) |>
      shiny::bindEvent(input$save_item)

    shiny::observe({
      db_copy <- db()

      i <- input$delete_idx
      if (!is.null(i) && i <= length(db_copy$items)) {
        db_copy$items[[i]] <- NULL
        db_copy$ids <- db_copy$ids[-i]
      }

      db(db_copy)
    }) |>
      shiny::bindEvent(input$delete_trigger, ignoreNULL = TRUE)

    return(db)
  })
}

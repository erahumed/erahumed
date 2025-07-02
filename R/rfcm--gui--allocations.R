allocations_db_ui <- function(id) {
  ns <- shiny::NS(id)
  list_manager_ui(
    ns("allocations"),
    object_name = "Allocation",
    list_description = "list"
  )
}

allocations_db_server <- function(id, rfms_db) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    list_manager_server(
      "allocations",
      item_editor_ui = make_allocations_editor_ui(rfms_db = rfms_db),
      item_editor_server = make_allocations_editor_server(rfms_db = rfms_db),
      item_display_function = function(item) {
        item_idx <- item$allocate_ms
        rfms <- shiny::reactiveValuesToList(rfms_db)[[item_idx]]()
        name <- rfms[["display_name"]]
        pct <- paste0(round(item$target_fraction * 100), "%")
        paste0(name, " (", pct, ")")
      },
      default_items = list()  # TODO
    )
  })
}

make_allocations_editor_ui <- function(rfms_db) {
  function(id) {
    ns <- shiny::NS(id)

    choices <- names(shiny::reactiveValuesToList(rfms_db))
    names(choices) <- sapply(shiny::reactiveValuesToList(rfms_db), function(x) {
      x()$display_name
    })

    shiny::tagList(
      shiny::selectInput(ns("allocate_ms"), "System to allocate", choices = choices),
      shiny::numericInput(ns("target_fraction"), "Target fraction", value = 0.1, min = 0, max = 1, step = 0.01),
      shiny::selectInput(ns("field_type"), "Field type", choices = c("both", "regular", "tancat")),
      shiny::sliderInput(ns("ditches"), "Ditches", min = 1, max = 26, value = c(1, 26), step = 1)
    )
  }
}

make_allocations_editor_server <- function(rfms_db) {
  function(id, item = shiny::reactive(NULL)) {
    shiny::moduleServer(id, function(input, output, session) {
      shiny::observe({
        shiny::req(item())

        shiny::updateSelectInput(inputId = "allocate_ms", selected = item()$allocate_ms)
        shiny::updateNumericInput(inputId = "target_fraction", value = item()$target_fraction)
        shiny::updateSelectInput(inputId = "field_type", selected = item()$field_type)
        shiny::updateSliderInput(inputId = "ditches", value = item()$ditches)
      })

      shiny::reactive({

        list(allocate_ms = input$allocate_ms,
             target_fraction = input$target_fraction,
             field_type = input$field_type,
             ditches = input$ditches
             )
        })
    })
  }
}


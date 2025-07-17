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
        rfms_list <- shiny::reactiveValuesToList(rfms_db)
        item_idx <- item$allocate_ms

        name <- tryCatch({
          if (!item_idx %in% names(rfms_list)) {
            stop("RFMS no longer exists")
          }
          rfms <- rfms_list[[item_idx]]()
          rfms[["display_name"]]
        }, error = function(e) {
          "<deleted>"
        })

        pct <- paste0(round(item$target_fraction * 100), "%")
        paste0(name, " (", pct, ")")
      },

      default_items = list(
        list(
          allocate_ms = "clearfield",
          target_fraction = 0.8,
          field_type = "both",         # or "regular" if clearer
          ditches = c(1, 9)
        ),
        list(
          allocate_ms = "clearfield",
          target_fraction = 0.20,
          field_type = "both",
          ditches = c(10, 26)  # full range or customize
        )
      )
    )
  })
}

make_allocations_editor_ui <- function(rfms_db) {
  function(id) {
    ns <- shiny::NS(id)

    rfms_list <- shiny::reactiveValuesToList(rfms_db)

    choices <- names(rfms_list)
    labels <- sapply(rfms_list, function(x) {
      tryCatch(
        x()$display_name,
        error = function(e) "<deleted>"
      )
    })

    # Optionally filter out deleted ones
    valid <- labels != "<deleted>"
    choices <- choices[valid]
    labels <- labels[valid]

    names(choices) <- labels

    shiny::tagList(
      allocation_input_system(ns("allocate_ms"), choices = choices),
      allocation_input_target_fraction(ns("target_fraction")),
      allocation_input_field_type(ns("field_type")),
      allocation_input_ditches(ns("ditches"))
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


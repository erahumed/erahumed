rfcm_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    h3("Cluster-Based Management Assignment"),

    selectInput(ns("default_ms"), "Default Management System",
                choices = NULL),
    actionButton(ns("create_map"), "Create New Cluster Map"),

    hr(),
    selectInput(ns("allocate_ms"), "Allocate Management System",
                choices = NULL),
    numericInput(ns("target_fraction"), "Target Fraction", value = 0.1, min = 0, max = 1, step = 0.01),
    selectInput(ns("field_type"), "Field Type", choices = c("both", "regular", "tancat")),
    sliderInput(ns("ditches"), "Ditches", min = 1, max = 26, value = c(1, 26), step = 1),
    actionButton(ns("allocate"), "Allocate"),

    hr(),
    verbatimTextOutput(ns("summary"))
  )
}







rfcm_server <- function(id, rfms_db) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    map <- reactiveVal(NULL)

    # Show available rfms
    observe({
      rfms_names <- names(reactiveValuesToList(rfms_db))
      updateSelectInput(session, "default_ms", choices = rfms_names)
      updateSelectInput(session, "allocate_ms", choices = rfms_names)

      cat("[rfcm] Available RFMS:\n")
      print(rfms_names)
    })

    # Create new map
    observeEvent(input$create_map, {
      req(input$default_ms)
      default_system <- rfms_db[[input$default_ms]]
      new_map <- new_cluster_map(default_system)
      map(new_map)

      cat("[rfcm] New cluster map created with default system:", input$default_ms, "\n")
    })

    # Allocate surface
    observeEvent(input$allocate, {
      req(map(), input$allocate_ms, input$target_fraction)

      rfms <- shiny::reactiveValuesToList(rfms_db)[[input$allocate_ms]]
      ditches <- seq(input$ditches[1], input$ditches[2])
      field_type <- input$field_type

      cat("[rfcm] Allocating system:", input$allocate_ms,
          "\n  Fraction:", input$target_fraction,
          "\n  Ditches:", paste(ditches, collapse = ", "),
          "\n  Field type:", field_type, "\n")

      updated <- allocate_surface(
        map = map(),
        system = rfms(),
        target_fraction = input$target_fraction,
        ditches = ditches,
        field_type = field_type
      )
      map(updated)

      cat("[rfcm] Allocation complete. Updated map has",
          length(updated$ms_list), "management systems.\n")
    })

    output$summary <- renderPrint({
      req(map())
      summary(map())
    })

    return(map)
  })
}




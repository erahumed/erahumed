rfcm_ui <- function(id) {
  ns <- shiny::NS(id)

  sidebar <- bslib::sidebar(
    width = "30%",
    bslib::card(
      bslib::card_header("Initialization"),
      bslib::card_body(
        shiny::selectInput(ns("default_ms"), "Default Management System", choices = NULL),
        shiny::actionButton(ns("create_map"), "Create New Cluster Map", icon = icon("map"))
      )
    ),
    bslib::card(
      bslib::card_header("Spatial Allocation"),
      bslib::card_body(
        shiny::selectInput(ns("allocate_ms"), "System to allocate", choices = NULL),
        shiny::numericInput(ns("target_fraction"), "Target fraction", value = 0.1, min = 0, max = 1, step = 0.01),
        shiny::selectInput(ns("field_type"), "Field type", choices = c("both", "regular", "tancat")),
        shiny::sliderInput(ns("ditches"), "Ditches", min = 1, max = 26, value = c(1, 26), step = 1),
        shiny::actionButton(ns("allocate"), "Allocate", icon = icon("fill-drip"))
      )
    )
  )

  bslib::layout_sidebar(
    shiny::h3("Spatial mapping of management systems"),
    sidebar = sidebar,
    bslib::card(
      bslib::card_header("Map Summary"),
      shiny::verbatimTextOutput(ns("summary"))
    )
  )
}







rfcm_server <- function(id, rfms_db) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    map <- shiny::reactiveVal(NULL)

    # Show available rfms
    shiny::observe({
      choices <- names(shiny::reactiveValuesToList(rfms_db))
      names(choices) <- sapply(shiny::reactiveValuesToList(rfms_db), function(x) {
        x()$display_name
      })
      shiny::updateSelectInput(session, "default_ms", choices = choices)
      shiny::updateSelectInput(session, "allocate_ms", choices = choices)
    })

    # Create new map
    shiny::observeEvent(input$create_map, {
      shiny::req(input$default_ms)
      default_system <- shiny::reactiveValuesToList(rfms_db)[[input$default_ms]]
      new_map <- new_cluster_map(default_system())
      map(new_map)
    })

    # Allocate surface
    shiny::observeEvent(input$allocate, {
      shiny::req(map(), input$allocate_ms, input$target_fraction)

      rfms <- shiny::reactiveValuesToList(rfms_db)[[input$allocate_ms]]
      ditches <- seq(input$ditches[1], input$ditches[2])
      field_type <- input$field_type

      updated <- allocate_surface(
        map = map(),
        system = rfms(),
        target_fraction = input$target_fraction,
        ditches = ditches,
        field_type = field_type
      )
      map(updated)
    })

    output$summary <- shiny::renderPrint( summary(map()) )

    return(map)
  })
}




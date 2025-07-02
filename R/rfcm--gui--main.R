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
    allocations_db_ui(ns("allocations_db"))
  )

  bslib::layout_sidebar(
    shiny::h3("Spatial mapping of management systems"),
    sidebar = sidebar,
    bslib::card(
      bslib::card_header("Map Summary"),
      shiny::verbatimTextOutput(ns("summary")),
      shiny::plotOutput(ns("pie"))
    )
  )
}







rfcm_server <- function(id, rfms_db) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Show available rfms
    shiny::observe({
      choices <- names(shiny::reactiveValuesToList(rfms_db))
      names(choices) <- sapply(shiny::reactiveValuesToList(rfms_db), function(x) {
        x()$display_name
      })
      shiny::updateSelectInput(session, "default_ms", choices = choices)
      shiny::updateSelectInput(session, "allocate_ms", choices = choices)
    })

    allocations_db <- allocations_db_server("allocations_db", rfms_db = rfms_db)

    map <- shiny::reactive({
      shiny::req(input$default_ms)

      default_ms <- shiny::reactiveValuesToList(rfms_db)[[input$default_ms]]
      res <- new_cluster_map(default_management_system = default_ms())

      for (allocation in allocations_db()$items) {
        rfms <- shiny::reactiveValuesToList(rfms_db)[[allocation$allocate_ms]]

        res <- res |> allocate_surface(
          system = rfms(),
          target_fraction = allocation$target_fraction,
          ditches = seq(allocation$ditches[1], allocation$ditches[2]),
          field_type = allocation$field_type
        )
      }

      res
      })

    output$summary <- shiny::renderPrint( summary(map()) )

    output$pie <- shiny::renderPlot({
      shiny::req(map())

      ids <- paste(map()$map_df$ms_id, map()$map_df$ms_name, sep = " - ")
      ms_areas <- table(ids)
      graphics::pie(
        ms_areas,
        col = grDevices::rainbow(length(ms_areas)),
        main = "Surface breakdown by management system"
        )
    })


    return(map)
  })
}






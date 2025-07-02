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
      rfms_list <- shiny::reactiveValuesToList(rfms_db)

      labels <- sapply(rfms_list, function(x) {
        tryCatch(x()$display_name, error = function(e) NA)
      })

      valid <- !is.na(labels)
      choices <- names(rfms_list)[valid]
      names(choices) <- labels[valid]

      shiny::updateSelectInput(session, "default_ms", choices = choices)
      shiny::updateSelectInput(session, "allocate_ms", choices = choices)
    })


    allocations_db <- allocations_db_server("allocations_db", rfms_db = rfms_db)

    map <- shiny::reactive({
      shiny::req(input$default_ms)

      rfms_list <- shiny::reactiveValuesToList(rfms_db)

      # Defensive: check if selected default_ms still exists
      if (!input$default_ms %in% names(rfms_list)) {
        shiny::showNotification("Selected default management system has been deleted.", type = "error")
        shiny::req(FALSE)
      }

      default_rfms_fun <- rfms_list[[input$default_ms]]

      default_ms <- tryCatch(
        default_rfms_fun(),
        error = function(e) {
          shiny::showNotification("Default RFMS is no longer available.", type = "error")
          shiny::req(FALSE)
        }
      )

      res <- new_cluster_map(default_management_system = default_ms)

      for (allocation in allocations_db()$items) {
        rfms_fun <- rfms_list[[allocation$allocate_ms]]
        rfms <- tryCatch(
          rfms_fun(),
          error = function(e) {
            shiny::showNotification(
              paste("Skipping allocation due to deleted RFMS:", allocation$allocate_ms),
              type = "warning"
            )
            NULL
          }
        )
        if (!is.null(rfms)) {
          res <- res |> allocate_surface(
            system = rfms,
            target_fraction = allocation$target_fraction,
            ditches = seq(allocation$ditches[1], allocation$ditches[2]),
            field_type = allocation$field_type
          )
        }
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






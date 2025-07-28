rfcm_ui <- function(id) {
  ns <- shiny::NS(id)

  sidebar <- bslib::sidebar(
    width = "30%",
    shiny::selectInput(ns("default_ms"), "Default Management System", choices = NULL),
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







rfcm_server <- function(id, rfms_db, seed) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    has_initialized <- shiny::reactiveVal(FALSE)

    previous_choices <- shiny::reactiveVal(NULL)
    previous_selected <- shiny::reactiveVal(NULL)

    shiny::observe({
      rfms_list <- shiny::reactiveValuesToList(rfms_db)

      # Names are values, labels are display names
      values <- names(rfms_list)
      labels <- sapply(rfms_list, function(x) {
        tryCatch(x()$display_name, error = function(e) NA)
      })

      valid <- !is.na(labels)
      values <- values[valid]
      labels <- labels[valid]
      choices <- values
      names(choices) <- labels

      if (!identical(values, previous_choices())) {
        current_selected <- input$default_ms

        selected <- NULL
        if (!has_initialized()) {
          if ("jsendra" %in% values) {
            selected <- "jsendra"
          } else if (length(values) > 0) {
            selected <- values[1]
          }
          has_initialized(TRUE)
        } else {
          if (!is.null(current_selected) && current_selected %in% values) {
            selected <- current_selected
          } else if (length(values) > 0) {
            selected <- values[1]
          }
        }

        shiny::updateSelectInput(
          session, "default_ms",
          choices = choices,
          selected = selected
        )

        previous_choices(values)
        previous_selected(selected)
      }
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

      withr::with_seed(seed, {
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
      })

      res
    })

    output$summary <- shiny::renderPrint( summary(map()) )

    output$pie <- shiny::renderPlot({
      shiny::req(map())

      ids <- paste(map()$map_df$rfms_id, map()$map_df$rfms_name, sep = " - ")
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






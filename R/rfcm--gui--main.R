rfcm_ui <- function(id) {
  ns <- shiny::NS(id)

  allocation_rules_card <- bslib::card(
    bslib::card_header(shiny::h4("Allocation rules")),
    shiny::p("Assign:"),
    allocations_db_ui(ns("allocations_db")),
    shiny::selectInput(ns("default_ms"), "Unallocated surface is assigned to:", choices = NULL)
    )

  summary_card <- bslib::card(
    bslib::card_header(shiny::h4("Map Summary")),
    shiny::plotOutput(ns("pie"))
    )

  info_popover <- bslib::popover(
    shiny::actionLink(ns("rfcm_help"), label = NULL, icon = shiny::icon("circle-info")),
    title = "What is this?",
    placement = "right",
    shiny::div(
      "Assign rice field management systems across the Albufera Natural Park area. ",
      "Choose a default system and add allocation rules, optionally specifying a subset of ditches and field types. "
    )
  )

  bslib::page_fillable(
    shiny::div(class = "d-flex align-items-center gap-2 mb-4",
               shiny::h3("Spatial mapping of management systems"),
               info_popover),
    bslib::layout_column_wrap(allocation_rules_card, summary_card)
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

      res <- new_rfms_map(default_rfms = default_ms)

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






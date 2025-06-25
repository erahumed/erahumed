applications_db_ui <- function(id) {
  ns <- shiny::NS(id)
  list_manager_ui(
    ns("apps"),
    object_name = "Application",
    list_description = "list"
  )
}

applications_db_server <- function(id, chemical_db, harvesting_yday, sowing_yday, default_items = list()) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    list_manager_server(
      "apps",
      item_editor_ui =
        make_application_editor_ui(chemical_db = chemical_db,
                                   harvesting_yday = harvesting_yday,
                                   sowing_yday = sowing_yday),
      item_editor_server =
        make_application_editor_server(chemical_db = chemical_db),
      item_display_function =
        \(x) paste0(x$chemical_name, " - Seed day: ", x$seed_day),
      default_items = default_items
    )
  })
}

make_application_editor_ui <- function(chemical_db, harvesting_yday, sowing_yday) {
  function(id) {
    ns <- shiny::NS(id)

    choices <- chemical_db()$ids
    names(choices) <- sapply(chemical_db()$items, \(.) .$display_name)

    shiny::tagList(
      shiny::selectInput(ns("chemical_id"), "Select chemical", choices = choices),

      shiny::numericInput(ns("seed_day"),
                          "Application day (since sowing)",
                          value = NA,
                          min = 1,
                          max = harvesting_yday() - sowing_yday()),

      shiny::numericInput(ns("amount_kg_ha"),
                          "Amount (kg/ha)",
                          value = NA,
                          min = 0),

      shiny::selectInput(ns("type"),
                         label = "Type",
                         choices = c("ground", "aerial"),
                         selected = NA),

      shiny::numericInput(ns("emptying_days"),
                          "Emptying days",
                          value = NA,
                          min = 1,
                          step = 1)
    )
  }
}

make_application_editor_server <- function(chemical_db) {
  function(id, item = shiny::reactive(NULL)) {
    shiny::moduleServer(id, function(input, output, session) {
      shiny::observe({
        shiny::req(item())

        shiny::updateSelectInput(inputId = "chemical_id", selected = item()$chemical_id)
        shiny::updateNumericInput(inputId = "seed_day", value = item()$seed_day)
        shiny::updateNumericInput(inputId = "amount_kg_ha", value = item()$amount_kg_ha)
        shiny::updateSelectInput(inputId = "type", selected = item()$type)
        shiny::updateNumericInput(inputId = "emptying_days", value = item()$emptying_days)
      })

      shiny::reactive({
        chemical_idx <- match(input$chemical_id, chemical_db()$ids)
        chemical_name <- chemical_db()$items[[chemical_idx]]$display_name

        list(
          chemical_name = chemical_name,
          chemical_id = input$chemical_id,
          amount_kg_ha = input$amount_kg_ha,
          seed_day = input$seed_day,
          type = input$type,
          emptying_days = input$emptying_days
        )
      })
    })
  }
}


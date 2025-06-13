rfcm_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    chemical_db_ui(ns("chem_db")),
    shiny::hr(),
    shiny::actionButton(ns("addButton"), "", icon = shiny::icon("plus"))
  )
}

rfcm_server <- function(id, chemical_db, initial_rfms_list = list()) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    chemical_db <- chemical_db_server("chem_db")

    shiny::observeEvent(input$addButton, {
      id <- paste0("rfms-", input$addButton)
      shiny::insertUI(
        selector = paste0("#", ns("addButton")),
        where = "beforeBegin",
        ui = rfms_ui(ns(id)),
        session = session
      )
      rfms_server(id = id, chemical_db = chemical_db, initial_rfms = jsendra())

    })
  })
}

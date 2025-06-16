rfcm_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    chemical_db_ui(ns("chem_db")),
    shiny::hr(),
    shiny::actionButton(ns("addButton"), "Add cluster", icon = shiny::icon("plus")),
    shiny::tabsetPanel(
      id = ns("rfms_tabs"),
      type = "tabs"
      # Initially empty; tabs will be inserted dynamically
    )
  )
}






rfcm_server <- function(id, chemical_db, initial_rfms_list = list()) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    chemical_db <- chemical_db_server("chem_db")

    counter <- shiny::reactiveVal(0)

    shiny::observeEvent(input$addButton, {
      i <- counter() + 1
      counter(i)

      tab_id <- paste0("rfms_", i)
      tab_title <- paste("Cluster", i)

      # Create the tab panel UI with namespaced ID
      shiny::insertTab(
        inputId = "rfms_tabs",
        shiny::tabPanel(
          title = tab_title,
          value = tab_id,
          rfms_ui(ns(tab_id))
        ),
        target = NULL,  # append at end
        position = "after",
        select = TRUE  # automatically switch to new tab
      )

      # Call the server logic for this rfms instance
      rfms_server(id = tab_id, chemical_db = chemical_db, initial_rfms = jsendra())
    })
  })
}

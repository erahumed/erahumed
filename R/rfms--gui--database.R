rfms_db_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::page_fillable(
    title = "Rice Field Management Systems",
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    shiny::tabsetPanel(id = ns("rfms_tabs"), type = "tabs",
      header = shiny::tagList(
        shiny::actionButton(ns("addButton"), "Add new", icon = shiny::icon("plus")),
        shiny::actionButton(ns("remove_tab"), "Remove current"),
      )
      # Initially empty; tabs will be inserted dynamically
    )
  )
}






rfms_db_server <- function(id, chemical_db) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    counter <- shiny::reactiveVal(0)

    rfms_modules <- shiny::reactiveValues()  # holds system() reactive for each tab
    open_tabs <- shiny::reactiveVal(character())  # list of currently open tab IDs

    # -- Add new RFMS tab
    shiny::observeEvent(input$addButton, {
      i <- counter() + 1
      counter(i)

      tab_id <- paste0("rfms_", i)
      tab_title <- paste("System", i)

      # Insert tab
      shiny::insertTab(
        inputId = "rfms_tabs",
        shiny::tabPanel(
          title = tab_title,
          value = tab_id,
          rfms_ui(ns(tab_id))
        ),
        position = "after",
        select = TRUE
      )

      rfms_modules[[tab_id]] <- rfms_server(id = tab_id, chemical_db = chemical_db, initial_rfms = new_management_system())

      # Track as open
      open_tabs(c(open_tabs(), tab_id))
    })

    # -- Expose closing mechanism via external event or UI
    shiny::observeEvent(input$remove_tab, {
      tab_id <- input$rfms_tabs
      if (!is.null(tab_id) && tab_id %in% open_tabs()) {
        shiny::removeTab("rfms_tabs", target = tab_id)
        open_tabs(setdiff(open_tabs(), tab_id))
        rfms_modules[[tab_id]] <- NULL  # optional: clean memory
      }
    })

    output$rfms_summary <- shiny::renderPrint({
      tabs <- open_tabs()
      if (length(tabs) == 0) {
        return()
      }

      summary_list <- lapply(tabs, function(tab_id) {
        sys <- rfms_modules[[tab_id]]
        if (is.null(sys)) return(NULL)
        val <- tryCatch(sys(), error = function(e) NULL)
        if (is.null(val)) return(NULL)
        list(tab = tab_id)
      })
    })

    # Optionally return rfms_list from module
    return(rfms_modules)
  })
}


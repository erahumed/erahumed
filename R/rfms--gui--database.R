rfms_db_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::page_fillable(
    title = "Rice Field Management Systems",
    shiny::tagList(
      shiny::h3("Rice Field Management Systems"),
      shiny::tagList(
        shiny::actionButton(ns("addButton"), "Add new", icon = shiny::icon("plus")),
        shiny::actionButton(ns("remove_tab"), "Remove current"),
      ),
      shiny::tabsetPanel(
        id = ns("rfms_tabs"),
        type = "tabs",
        shiny::tabPanel(title = "J.Sendra", value = "jsendra", rfms_ui(ns("jsendra"))),
        shiny::tabPanel(title = "Clearfield", value = "clearfield", rfms_ui(ns("clearfield"))),
        shiny::tabPanel(title = "Bomba", value = "bomba", rfms_ui(ns("bomba")))
        )
      )
    )
}






rfms_db_server <- function(id, chemical_db) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    default_rfms <- c("jsendra", "bomba", "clearfield")

    counter <- shiny::reactiveVal(0)

    rfms_modules <- shiny::reactiveValues()  # holds system() reactive for each tab

    rfms_modules[["jsendra"]] <- rfms_server(id = "jsendra", chemical_db = chemical_db, initial_rfms = jsendra())
    rfms_modules[["bomba"]] <- rfms_server(id = "bomba", chemical_db = chemical_db, initial_rfms = bomba())
    rfms_modules[["clearfield"]] <- rfms_server(id = "clearfield", chemical_db = chemical_db, initial_rfms = clearfield())

    open_tabs <- shiny::reactiveVal(default_rfms)
    tab_titles <- shiny::reactiveValues(jsendra = "J.Sendra",
                                        bomba = "Bomba",
                                        clearfield = "Clearfield")



    shiny::observeEvent(input$addButton, {
      shiny::showModal(
        session = session,
        shiny::modalDialog(
          title = "Name your new RFMS",
          shiny::textInput(ns("new_rfms_name"), "System name", ""),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(ns("confirm_add"), "Add")
          )
        )
      )
    })


    # -- Add new RFMS tab
    shiny::observeEvent(input$confirm_add, {
      shiny::req(input$new_rfms_name)

      shiny::removeModal(session = session)

      i <- counter() + 1
      counter(i)

      tab_id <- paste0("rfms_", i)
      tab_label <- input$new_rfms_name

      tab_titles[[tab_id]] <- tab_label

      shiny::insertTab(
        inputId = "rfms_tabs",
        shiny::tabPanel(
          title = tab_label,
          value = tab_id,
          rfms_ui(ns(tab_id))
        ),
        position = "after",
        select = TRUE
      )

      rfms_modules[[tab_id]] <- rfms_server(
        id = tab_id,
        chemical_db = chemical_db,
        initial_rfms = new_rfms(display_name = tab_label)
        )

      open_tabs(c(open_tabs(), tab_id))
    })

    # -- Expose closing mechanism via external event or UI
    shiny::observeEvent(input$remove_tab, {
      tab_id <- input$rfms_tabs
      if (tab_id %in% default_rfms) {
        shiny::showModal(shiny::modalDialog(
          title = "Cannot remove default system",
          paste("The system", tab_titles[[tab_id]], "is a default and cannot be deleted."),
          easyClose = TRUE
        ))
      } else if (!is.null(tab_id) && tab_id %in% open_tabs()) {
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


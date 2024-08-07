dataUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabsetPanel(
    shiny::tabPanel("Lake Levels and Outflows",
                    dataInputUI(ns("outflows"))),
    shiny::tabPanel("Precipitation and Evapotranspiration",
                    dataInputUI(ns("petp"))),
    shiny::tabPanel("Paddy Management",
                    dataInputUI(ns("management"))))

}

dataServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    o_df <-
      dataInputServer("outflows", initial_df = erahumed::albufera_outflows)
    p_df <-
      dataInputServer("petp", initial_df = erahumed::albufera_petp)
    m_df <-
      dataInputServer("management", initial_df = erahumed::albufera_management)

    res <- shiny::reactive({
      list(outflows_df = o_df(), petp_df = p_df(), management_df = m_df())
      })

    return(res)
  })
}


# Define the UI part of the module
dataInputUI <- function(id) {
  ns <- shiny::NS(id)
  file_accept <- c("text/csv", "text/comma-separated-values,text/plain", ".csv")

  shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput(ns("file"), "Upload CSV File", accept = file_accept),
        shiny::numericInput(ns("rows"), "Rows per page", 10, min = 1),
        shiny::downloadButton(ns("downloadData"), "Download Data")
      ),
      shiny::mainPanel(DT::DTOutput(ns("contents")))
    )
  )

}

# Define the server part of the module
dataInputServer <- function(id, initial_df) {
  shiny::moduleServer(id, function(input, output, session) {
    df <- shiny::reactiveVal(initial_df)

    # Observe file input and update the data frame
    shiny::observe({
      shiny::req(input$file)
      file <- input$file$datapath
      new_data <- readr::read_csv(file)

      tryCatch(assert_data.frame(new_data, template = initial_df),
               error = \(cnd) shiny::req(FALSE))

      df(new_data)
    })

    # Output the data frame as a paginated table
    output$contents <- DT::renderDT({
      DT::datatable(df(), options = list(pageLength = input$rows))
    })

    # Downloadable csv of the data frame
    output$downloadData <- shiny::downloadHandler(
      filename = function() paste0("data-", Sys.Date(), ".csv"),
      content = function(file) readr::write_csv(df(), file)
    )

    return(df)
  })
}

csvInputUI <- function(id) {
  ns <- shiny::NS(id)
  file_accept <- c("text/csv", "text/comma-separated-values,text/plain", ".csv")

  shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput(ns("file"), "Upload CSV File", accept = file_accept),
        shiny::numericInput(ns("rows"), "Rows per page", 5, min = 1),
        shiny::downloadButton(ns("downloadData"), "Download Data")
      ),
      shiny::mainPanel(DT::DTOutput(ns("contents")))
    )
  )

}

csvInputServer <- function(id, initial_df, sig_digits = 4) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
      DT::datatable(df(), options = list(pageLength = input$rows)) |>
        DT::formatSignif(which(sapply(df(), is.numeric)), digits = sig_digits)
    })

    # Downloadable csv of the data frame
    output$downloadData <- shiny::downloadHandler(
      filename = function() paste0("data-", Sys.Date(), ".csv"),
      content = function(file) readr::write_csv(df(), file)
    )

    return(df)
  })
}

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

      new_data <- csv_try_read(file)
      csv_try_assert_df(new_data, template = initial_df)

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

csv_try_read <- function(file) {
  err_title <- shiny::p(shiny::icon("ban"), "Error reading the uploaded file")
  err_msg <-   shiny::markdown(
    "The file could not be read as a valid CSV. Please ensure that the file:

    * Is in CSV format (e.g., .csv extension).
    * Is not empty or corrupted.
    * Uses a supported encoding (e.g., UTF-8).

    Try re-uploading the file or using a different one."
  )
  err_modal <- shiny::modalDialog(title = err_title, err_msg, easyClose = TRUE)
  err_fn <- function(cnd) {
    shiny::showModal(err_modal)
    shiny::req(FALSE)
  }

  tryCatch(readr::read_csv(file), error = err_fn)
}

csv_try_assert_df <- function(df, template) {
  err_title <- shiny::p(shiny::icon("ban"), "Invalid data format")

  cols <- colnames(template)
  types <- sapply(template, typeof)
  cols_list <- paste(cols, collapse = ", ")
  types_msg <- paste0("\n   * '", cols, "' should be of type '", types, "'") |>
    paste0(collapse = "")
  err_msg <- shiny::markdown(paste(
    "The uploaded file does not match the expected structure.",
    "Please ensure that:\n\n",
    "* The file contains the required columns:", cols_list, "\n",
    "* Each column has the correct data type:", types_msg, "\n\n",
    "Check the file and upload it again."
    ))

  err_modal <- shiny::modalDialog(title = err_title, err_msg, easyClose = TRUE)
  err_fn <- function(cnd) {
    shiny::showModal(err_modal)
    shiny::req(FALSE)
  }

  tryCatch(assert_data.frame(df, template = template), error = err_fn)
}




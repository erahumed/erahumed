csvInputUI <- function(id, columns = NULL) {
  ns <- shiny::NS(id)
  file_accept <- c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv",
                   ".xls", ".xlsx")
  upload_label <- "Upload dataset in CSV or Excel format"

  page_header <- if (is.null(columns)) NULL else csv_input_header(columns)

  bslib::page_fluid(
    page_header,
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        shiny::fileInput(ns("file"), upload_label, accept = file_accept),
        shiny::downloadButton(ns("downloadDataCSV"),
                              "Download as .csv",
                              icon = shiny::icon("file-csv")
                              ),
        shiny::downloadButton(ns("downloadDataExcel"),
                              "Download as .xlsx",
                              icon = shiny::icon("file-excel")
                              )
        ),
      DT::DTOutput(ns("contents"))
    )
  )

}

csvInputServer <- function(id, initial_df, sig_digits = 4, initial_rows = 5) {
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
      DT::datatable(df(), options = list(pageLength = initial_rows)) |>
        DT::formatSignif(which(sapply(df(), is.numeric)), digits = sig_digits)
    })

    # Downloadable csv of the data frame
    output$downloadDataCSV <- shiny::downloadHandler(
      filename = function() paste0("data-", Sys.Date(), ".csv"),
      content = function(file) readr::write_csv(df(), file)
    )

    # Downloadable Excel of the data frame
    output$downloadDataExcel <- shiny::downloadHandler(
      filename = function() paste0("data-", Sys.Date(), ".xlsx"),
      content = function(file) writexl::write_xlsx(df(), file)
    )

    return(df)
  })
}

csv_try_read <- function(file) {
  is_excel <- !is.na(readxl::excel_format(file))
  read_fun <- if (is_excel) readxl::read_excel else readr::read_csv

  err_title <- shiny::p(shiny::icon("ban"), "Error reading the uploaded file")
  err_msg <-   shiny::markdown(
    "The file could not be read as a valid CSV. Please ensure that the file:

    * Is in CSV format (e.g., .csv extension).
    * Is not empty or corrupted.
    * Uses a supported encoding (e.g., UTF-8).

    Try re-uploading the file or using a different one."
  )
  err_modal <- shiny::modalDialog(title = err_title, err_msg, easyClose = TRUE)
  err_fun <- function(cnd) {
    shiny::showModal(err_modal)
    shiny::req(FALSE)
  }

  tryCatch(read_fun(file), error = err_fun)
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
  err_fun <- function(cnd) {
    shiny::showModal(err_modal)
    shiny::req(FALSE)
  }

  tryCatch(assert_data.frame(df, template = template), error = err_fun)
}



csv_input_header <- function(columns) {
  assert_list(columns)

  markdown_text <- paste0("* **", names(columns), "**: ", columns,
                          collapse = "\n"
                          ) |>
    shiny::markdown()

  shiny::tags$details(
    shiny::tags$summary("See columns description"),
    markdown_text
  )
}

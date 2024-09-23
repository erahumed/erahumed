hbaUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(

    shiny::fluidRow(
      shiny::column(4,
                    shiny::selectInput(ns("variable"), "Select Variable",
                                       choices = hba_var_labs(invert = TRUE),
                                       selected = hba_var_labs(invert = TRUE)[[1]]
                    ))
    ),

    plotly::plotlyOutput(ns("hb_plot"))
  )
}

hbaServer <- function(id, inp_res) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    hb_data <- shiny::reactive({
      hb_data_full <- hba(inp_res = inp_res())
      row_idx <- setup$date_range[1] <= hb_data_full$date
      row_idx <- row_idx & hb_data_full$date <= setup$date_range[2]
      hb_data_full[row_idx, ]
    })

    output$hb_plot <- plotly::renderPlotly( plot(hb_data(), input$variable) )

  })
}

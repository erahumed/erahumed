hbaUI <- function(id) {
  ns <- shiny::NS(id)

  vars <- hba_var_labs(invert = TRUE)

  shiny::tabsetPanel(

    shiny::tabPanel("Output",

      shiny::fluidRow(
        shiny::column(4, shiny::selectInput(ns("variable"),
                                            "Select Variable",
                                            choices = vars,
                                            selected = vars[[1]])
                      )),

      plotly::plotlyOutput(ns("plot"))

      ),



      shiny::tabPanel("Setup", NULL)

    )


}

hbaServer <- function(id, model) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Read setup stuff
    #
    res <- shiny::reactive({ compute_hba(model()) })

    output$plot <- plotly::renderPlotly({ plot(hba(res()), input$variable) })

    return(res)
  })
}

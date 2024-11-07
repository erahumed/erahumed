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

hbaServer <- function(id, simulation) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Read setup stuff
    #
    res <- shiny::reactive({
      setup_hba(simulation()) |>
        run_simulation(layer = "hba")
        })

    output$plot <- plotly::renderPlotly({
      res() |>
        get_layer("hba") |>
        plot(input$variable)
      })

    return(res)
  })
}

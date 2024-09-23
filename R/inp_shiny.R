inpUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabsetPanel(
    shiny::tabPanel("Lake Levels and Outflows",
                    csvInputUI(ns("outflows"))),
    shiny::tabPanel("Precipitation and Evapotranspiration",
                    csvInputUI(ns("petp")))
    )
}

inpServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    o_df <-
      csvInputServer("outflows", initial_df = erahumed::albufera_outflows)
    p_df <-
      csvInputServer("petp", initial_df = erahumed::albufera_petp)

    res <- shiny::reactive({ inp(outflows_df = o_df(), petp_df = p_df()) })

    return(res)
  })
}




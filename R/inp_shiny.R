inpUI <- function(id) {
  ns <- shiny::NS(id)

  outflows_tab_title <- "Lake Levels and Outflows"
  petp_tab_title <- "Precipitation and Evapotranspiration"

  shiny::tabsetPanel(
    shiny::tabPanel(outflows_tab_title, csvInputUI(ns("outflows"))),
    shiny::tabPanel(petp_tab_title, csvInputUI(ns("petp")))
    )
}

inpServer <- function(id, model) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    outflows_df <- csvInputServer("outflows", erahumed::albufera_outflows)
    petp_df <- csvInputServer("petp", erahumed::albufera_petp)

    res <- shiny::reactive({
      compute_inp(model(), outflows_df = outflows_df(), petp_df = petp_df())
    })
    return(res)
  })
}




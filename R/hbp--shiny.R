hbp_input_ui <- function(id) {
  ns <- shiny::NS(id)

  tltp <- function(param) param_tooltip(layer = "hbp", param = param)

  management_df_desc <- erahumed_param_desc("management_df", "hbp", strip_roxy = T)

  shiny::tagList(
    shiny::numericInput(ns("ideal_flow_rate_cm"),
                        shiny::p("Ideal Flow Rate [cm]", tltp("ideal_flow_rate_cm")),
                        value = eval(formals(setup_hbp)$ideal_flow_rate_cm),
                        min = 0,
                        max = 20,
                        step = 0.5
                        ),
    shiny::numericInput(ns("height_thresh_cm"),
                        shiny::p("Height Threshold [cm]", tltp("height_thresh_cm")),
                        value = eval(formals(setup_hbp)$height_thresh_cm),
                        min = 0,
                        max = 10,
                        step = 0.1
                        ),
    shiny::actionButton(ns("open_management_df_modal"), "Setup Management DF") |>
      bslib::tooltip(management_df_desc)
    )
}

hbp_input_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tltp <- function(param) param_tooltip(layer = "hbp", param = param)

    management_df <- csvInputServer("management", erahumed::albufera_management)
    shiny::observeEvent(input$open_management_df_modal, {
      shiny::showModal(shiny::modalDialog(
        csvInputUI(
          ns("management"),
          columns = erahumed_docs("layers", "hbp", "parameters", "management_df", "columns")
        ),
        title = shiny::p("Setup Management Dataset", tltp("management_df")),
        size = "xl"
      ))
    })

    shiny::reactive({
      list(
        ideal_flow_rate_cm = input$ideal_flow_rate_cm,
        height_thresh_cm = input$height_thresh_cm,
        management_df = management_df()
      )
    })


  })
}

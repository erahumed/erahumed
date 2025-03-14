ca_input_ui <- function(id) {
  ns <- shiny::NS(id)

  ca_schedules_df_desc <- erahumed_param_desc("ca_schedules_df", "ca", strip_roxy = T)


  shiny::tagList(
    shiny::actionButton(ns("open_ca_schedules_df_modal"), "Setup Applications DF") |>
      bslib::tooltip(ca_schedules_df_desc)

  )
}

ca_input_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tltp <- function(param) param_tooltip(layer = "ca", param = param)


    ca_schedules_df <- csvInputServer("applications", erahumed::albufera_ca_schedules)
    shiny::observeEvent(input$open_ca_schedules_df_modal, {
      shiny::showModal(shiny::modalDialog(
        csvInputUI(
          ns("applications"),
          columns = erahumed_input_docs("layers", "ca", "parameters", "ca_schedules_df", "columns")
        ),
        title = shiny::p("Setup Applications Dataset", tltp("ca_schedules_df")),
        size = "xl"
      ))
    })

    shiny::reactive({
      list(ca_schedules_df = ca_schedules_df())
    })
  })
}


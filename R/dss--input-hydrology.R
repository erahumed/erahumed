dss_input_hydrology_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    inp_input_ui(ns("inp")),
    hbl_input_ui(ns("hbl")),
    hbd_input_ui(ns("hbd")),
    hbc_input_ui(ns("hbc"))
  )

}

dss_input_hydrology_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    param_inp <- inp_input_server("inp")
    param_hbl <- hbl_input_server("hbl")
    param_hbd <- hbd_input_server("hbd")
    param_hbc <- hbc_input_server("hbc")

    shiny::reactive( c(param_inp(), param_hbl(), param_hbd(), param_hbc()) )
  })
}

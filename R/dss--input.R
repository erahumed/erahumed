#' @importFrom bslib card card_header card_body card_footer
dss_input_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::page_fillable(
    title = "Input",
    bslib::layout_column_wrap(
      card(card_header("INP"), card_body(inp_input_ui( ns("inp") ))),
      card(card_header("HBA"), card_body(hba_input_ui( ns("hba") ))),
      card(card_header("HBP"), card_body(hbp_input_ui( ns("hbp") ))),
      card(card_header("CA"), card_body(ca_input_ui( ns("ca") ))),
      card(
        full_screen = TRUE,
        card_header("CT",
                    bslib::tooltip(
                      shiny_icon("question-circle"),
                      "Additional info",
                      placement = "right"
                    ),
                    class = "bg-dark"),
        card_body(ct_input_ui( ns("ct") )),
        card_footer("Card footer")
        )
    )


  )

}

dss_input_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    list(
      inp = inp_input_server("inp"),
      hba = hba_input_server("hba"),
      hbp = hbp_input_server("hbp"),
      ca = ca_input_server("ca"),
      ct = ct_input_server("ct")
      )
  })
}

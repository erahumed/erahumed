#' @importFrom bslib card card_header card_body card_footer
dss_input_ui <- function(id) {
  layers_docs <- erahumed_docs("layers")

  ns <- shiny::NS(id)

  bslib::page_fillable(
    title = "Input",
    bslib::layout_column_wrap(
      card(layer_card_header("inp"), card_body(inp_input_ui( ns("inp") ))),
      card(layer_card_header("hba"), card_body(hba_input_ui( ns("hba") ))),
      card(layer_card_header("hbp"), card_body(hbp_input_ui( ns("hbp") ))),
      card(layer_card_header("ca"), card_body(ca_input_ui( ns("ca") ))),
      card(layer_card_header("ct"), card_body(ct_input_ui( ns("ct") )))
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

layer_card_header <- function(layer) {
  docs <- erahumed_docs("layers", layer)

  title <- docs[["title"]]
  description <- docs[["description"]]

  card_header(title,
              bslib::tooltip(
                shiny_icon("question-circle"),
                description,
                placement = "right"
              ),
              class = "bg-dark")

}

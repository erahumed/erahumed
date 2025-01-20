#' @importFrom bslib card card_header card_body card_footer
dss_input_ui <- function(id) {
  layers_docs <- erahumed_docs("layers")

  ns <- shiny::NS(id)

  bslib::page_fillable(
    title = "Input",
    bslib::layout_column_wrap(
      card(layer_card_header("inp"), card_body(inp_input_ui( ns("inp") ))),
      card(layer_card_header("hbl"), card_body(hbl_input_ui( ns("hbl") ))),
      card(layer_card_header("hbc"), card_body(hbc_input_ui( ns("hbc") ))),
      card(layer_card_header("hbd"), card_body(hbd_input_ui( ns("hbd") ))),
      card(layer_card_header("ca"), card_body(ca_input_ui( ns("ca") ))),
      card(layer_card_header("ctc"), card_body(ctc_input_ui( ns("ctc") )))
      )
    )

}

dss_input_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    list(
      inp = inp_input_server("inp"),
      hbl = hbl_input_server("hbl"),
      hbc = hbc_input_server("hbc"),
      hbd = hbd_input_server("hbd"),
      ca = ca_input_server("ca"),
      ctc = ct_input_server("ctc")
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

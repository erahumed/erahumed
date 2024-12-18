#' @importFrom bslib card card_header card_body card_footer
dss_output_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::page_fillable(
    title = "Input",
    bslib::layout_column_wrap(
      card(card_header("Hidrology"), card_body(
        shiny::textOutput(ns("hba_print_text")),
        shiny::textOutput(ns("hbp_print_text"))
        )),
      card(card_header("Chemicals"), card_body(
        shiny::textOutput(ns("ca_print_text"))
        )),
      card(card_header("Other things"), card_body( "Things." ))
    )
  )
}

dss_output_server <- function(id, layers) {
  shiny::moduleServer(id, function(input, output, session) {
    output$hba_print_text <- shiny::renderText(capture.output(layers$hba()))
    output$hbp_print_text <- shiny::renderText(capture.output(layers$hbp()))
    output$ca_print_text <- shiny::renderText(capture.output(layers$ca()))
  })
}

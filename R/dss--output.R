#' @importFrom bslib card card_header card_body card_footer
dss_output_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::page_fillable(
    title = "Input",
    bslib::layout_column_wrap(
      card(card_header("Hidrology"), card_body(
        dygraphs::dygraphOutput(ns("hba_plot")),
        dygraphs::dygraphOutput(ns("hbp_plot"))
        )),
      card(card_header("Chemicals"), card_body(
        dygraphs::dygraphOutput(ns("ca_plot")),
        dygraphs::dygraphOutput(ns("ct_plot"))
        )),
      card(card_header("Other things"), card_body( "Things." ))
    )
  )
}

dss_output_server <- function(id, layers) {
  shiny::moduleServer(id, function(input, output, session) {
    output$hba_plot <- dygraphs::renderDygraph(plot(layers$hba))
    output$hbp_plot <- dygraphs::renderDygraph(plot(layers$hbp))
    output$ca_plot <- dygraphs::renderDygraph(plot(layers$ca))
    output$ct_plot <- dygraphs::renderDygraph(plot(layers$ct))
  })
}

my_plot <- function(x) {
  x <- cat("The class of", deparse(substitute(x)), "is", class(x))
  plot(x)
}

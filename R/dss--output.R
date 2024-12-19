#' @importFrom bslib card card_header card_body card_footer
dss_output_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::page_fillable(
    title = "Input",
    shiny::selectInput("selected_cluster_id",
                       label = "Select cluster",
                       choices = info_clusters()$cluster_id,
                       selected = info_clusters()$cluster_id[[1]]
                       ),
    bslib::layout_column_wrap(
      card(card_header("Hidrology"), full_screen = TRUE,
        dygraphs::dygraphOutput(ns("hba_plot")),
        dygraphs::dygraphOutput(ns("ca_plot")),
        ),
      card(card_header("Chemicals"), full_screen = TRUE,
        dygraphs::dygraphOutput(ns("ct_plot"))
        ),
      card(card_header("Effects"), full_screen = TRUE, "Not yet implemented.")
    )
  )
}

dss_output_server <- function(id, layers) {
  shiny::moduleServer(id, function(input, output, session) {
    output$hba_plot <- dygraphs::renderDygraph(plot(layers$hba))
    output$ca_plot <- dygraphs::renderDygraph(
      plot(layers$ca, cluster_id = input$selected_cluster_id)
      )
    output$ct_plot <- dygraphs::renderDygraph(
      plot(layers$ct, cluster_id = input$selected_cluster_id)
      )
  })
}


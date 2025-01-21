#' @importFrom bslib card card_header card_body card_footer
#' @importFrom shinycssloaders withSpinner
dss_output_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::page_fillable(
    title = "Output",

    shiny::selectInput(ns("selected_cluster_id"),
                       label = "Select cluster",
                       choices = info_clusters()$cluster_id,
                       selected = info_clusters()$cluster_id[[1]]
                       ),
    bslib::layout_column_wrap(
      card(card_header("Hidrology"), full_screen = TRUE,
        dygraphs::dygraphOutput(ns("hbl_plot")) |> withSpinner(),
        dygraphs::dygraphOutput(ns("hbd_plot")) |> withSpinner(),
        dygraphs::dygraphOutput(ns("ca_plot")) |> withSpinner(),
        ),
      card(card_header("Chemicals"), full_screen = TRUE,
        dygraphs::dygraphOutput(ns("ct_plot")) |> withSpinner()
        ),
      card(card_header("Effects"), full_screen = TRUE, "Not yet implemented.")
    )
  )
}

dss_output_server <- function(id, simulation, clicked_cluster_id) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observeEvent(clicked_cluster_id(), {
      shiny::updateSelectInput(session,
                               "selected_cluster_id",
                               selected = clicked_cluster_id())
      })

    ditch <- shiny::reactive({
      info_clusters() ->.; .[.$cluster_id == input$selected_cluster_id, ]$ditch
      })

    output$hbl_plot <- dygraphs::renderDygraph(
      plot(get_layer(simulation(), "hbl"))
      )
    output$hbd_plot <- dygraphs::renderDygraph(
      plot(get_layer(simulation(), "hbd"), ditch = ditch())
      )
    output$ca_plot <- dygraphs::renderDygraph(
      plot(get_layer(simulation(), "ca"), cluster_id = input$selected_cluster_id)
      )
    output$ct_plot <- dygraphs::renderDygraph(
      plot(get_layer(simulation(), "ctc"), cluster_id = input$selected_cluster_id)
    )
  })
}


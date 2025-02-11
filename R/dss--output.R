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
      card(card_header("Hydrology", class = "bg-dark"), full_screen = TRUE,
        dygraphs::dygraphOutput(ns("hbl_plot")) |> withSpinner(),
        dygraphs::dygraphOutput(ns("hbd_plot")) |> withSpinner(),
        dygraphs::dygraphOutput(ns("ca_plot")) |> withSpinner(),
        ),
      card(card_header("Exposure", class = "bg-dark"), full_screen = TRUE,
        shiny::uiOutput(ns("select_chemical")),
        dygraphs::dygraphOutput(ns("ctl_plot")) |> withSpinner(),
        dygraphs::dygraphOutput(ns("ctd_plot")) |> withSpinner(),
        dygraphs::dygraphOutput(ns("ctc_plot")) |> withSpinner()
        ),
      card(card_header("Risk", class = "bg-dark"), full_screen = TRUE,
       dygraphs::dygraphOutput(ns("rl_plot")) |> withSpinner(),
       dygraphs::dygraphOutput(ns("rd_plot")) |> withSpinner(),
       dygraphs::dygraphOutput(ns("rc_plot")) |> withSpinner()
       )
    )
  )
}

dss_output_server <- function(id, simulation, clicked_cluster_id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(clicked_cluster_id(), {
      shiny::updateSelectInput(session,
                               "selected_cluster_id",
                               selected = clicked_cluster_id())
      })

    ditch <- shiny::reactive({
      info_clusters() ->.; .[.$cluster_id == input$selected_cluster_id, ]$ditch
      })

    output$select_chemical <- shiny::renderUI({
      chemicals <- get_layer_output(simulation(), "ctl")$chemical |> unique()
      shiny::selectInput(ns("chemical"),
                         label = "Select chemicals",
                         choices = chemicals,
                         multiple = TRUE
                         )
    })


    output$hbl_plot <- dygraphs::renderDygraph(
      plot(get_layer(simulation(), "hbl"),
           dygraph_group = "dss")
      )
    output$hbd_plot <- dygraphs::renderDygraph(
      plot(get_layer(simulation(), "hbd"),
           ditch = ditch(),
           dygraph_group = "dss")
      )
    output$ca_plot <- dygraphs::renderDygraph(
      plot(get_layer(simulation(), "ca"),
           cluster_id = input$selected_cluster_id,
           dygraph_group = "dss")
      )
    output$ctc_plot <- dygraphs::renderDygraph(
      plot(get_layer(simulation(), "ctc"),
           cluster_id = input$selected_cluster_id,
           chemical = input$chemical,
           dygraph_group = "dss")
    )
    output$ctd_plot <- dygraphs::renderDygraph(
      plot(get_layer(simulation(), "ctd"),
           ditch = ditch(),
           chemical = input$chemical,
           dygraph_group = "dss")
    )
    output$ctl_plot <- dygraphs::renderDygraph(
      plot(get_layer(simulation(), "ctl"),
           chemical = input$chemical,
           dygraph_group = "dss")
    )
    output$rc_plot <- dygraphs::renderDygraph(
      plot(get_layer(simulation(), "rc"),
           cluster_id = input$selected_cluster_id,
           dygraph_group = "dss")
    )
    output$rd_plot <- dygraphs::renderDygraph(
      plot(get_layer(simulation(), "rd"),
           ditch = ditch(),
           dygraph_group = "dss")
      )
    output$rl_plot <- dygraphs::renderDygraph(
      plot(get_layer(simulation(), "rl"),
           dygraph_group = "dss")
    )
  })
}


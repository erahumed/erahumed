#' @importFrom bslib card card_header card_body card_footer
#' @importFrom shinycssloaders withSpinner
dss_output_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::page_fillable(
    title = "Output",

    shinyWidgets::pickerInput(
      ns("water_body"),
      "Water body",
      choices = list(
        Lake = c("Albufera Lake"),
        Ditch = info_ditches()$ditch,
        Cluster = info_clusters()$cluster_id
        )
      ),

    bslib::layout_column_wrap(
      card(card_header("Hydrology", class = "bg-dark"), full_screen = TRUE,
        dygraphs::dygraphOutput(ns("hb_plot")) |> withSpinner()
        ),
      card(card_header("Exposure", class = "bg-dark"), full_screen = TRUE,
        shiny::uiOutput(ns("select_chemical")),
        shinyWidgets::prettyRadioButtons(
          inputId = ns("exposure_plot_type"),
          label = "Plot variable",
          choices = list(Mass = "mass", Density = "density"),
          selected = "density",
          inline = TRUE,
          status = "danger",
          fill = TRUE
          ),
        dygraphs::dygraphOutput(ns("ct_plot")) |> withSpinner()
        ),
      card(card_header("Risk", class = "bg-dark"), full_screen = TRUE,
        shiny::selectInput(inputId = ns("risk_type"),
                           label = "Risk type",
                           choices = list(Chronic = "chronic", Acute = "acute"),
                           selected = "chronic"),
        dygraphs::dygraphOutput(ns("r_plot")) |> withSpinner()
       )
    )
  )
}

dss_output_server <- function(id, simulation, clicked_cluster_id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    element_type <- shiny::reactive({
      if(!is.na(match(input$water_body, info_clusters()$cluster_id))) "c"
      else if (!is.na(match(input$water_body, info_ditches()$ditch))) "d"
      else "l"
    })

    output$select_chemical <- shiny::renderUI({
      chemicals <- get_layer_output(simulation(), "ctl")$chemical |> unique()

      shinyWidgets::checkboxGroupButtons(
        inputId = ns("chemical"),
        label = "Chemicals",
        choices = chemicals,
        selected = c("Penoxulam", "Difeno"),
        individual = TRUE,
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
      )

    })

    output$hb_plot <- dygraphs::renderDygraph({
      layer <- paste0("hb", element_type())
      layer_obj <- get_layer(simulation(), layer)
      plot(layer_obj,
           cluster_id = input$water_body,
           ditch = input$water_body,
           dygraph_group = "dss")
    })

    output$ct_plot <- dygraphs::renderDygraph({
      layer <- paste0("ct", element_type())
      layer_obj <- get_layer(simulation(), layer)
      plot(layer_obj,
           cluster_id = input$water_body,
           ditch = input$water_body,
           variable = input$exposure_plot_type,
           chemical = input$chemical,
           dygraph_group = "dss")
    })

    output$r_plot <- dygraphs::renderDygraph({
      layer <- paste0("r", element_type())
      layer_obj <- get_layer(simulation(), layer)
      plot(layer_obj,
           cluster_id = input$water_body,
           ditch = input$water_body,
           type = input$risk_type,
           dygraph_group = "dss")
    })

  })
}


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
      bslib::navset_card_tab(
        title = shiny::div(
          "Hydrology",
          bslib::popover(
            shiny_icon("gear"),
            shinyWidgets::radioGroupButtons(ns("hb_variable"),
                                            label = "Variable",
                                            choices = list(Depth = "depth",
                                                           Volume = "volume"),
                                            selected = "depth")
            )
          ),
        full_screen = TRUE,
        bslib::nav_panel("Storage", dygraphs::dygraphOutput(ns("hb_plot_storage")) |> withSpinner()),
        bslib::nav_panel("Flows", dygraphs::dygraphOutput(ns("hb_plot_flows")) |> withSpinner())
        ),
      bslib::navset_card_tab(
        title = shiny::div(
          "Exposure",
          bslib::popover(
            shiny_icon("gear"),
            shiny::uiOutput(ns("select_chemical"))
            )
          ),
        full_screen = TRUE,
        bslib::nav_panel("Water", dygraphs::dygraphOutput(ns("ct_plot_water")) |> withSpinner()),
        bslib::nav_panel("Sediment", dygraphs::dygraphOutput(ns("ct_plot_sediment")) |> withSpinner())
        ),
      bslib::navset_card_tab(
        title = shiny::div(
          "Risk",
          bslib::popover(
            shiny_icon("gear"),
            shinyWidgets::radioGroupButtons(
              inputId = ns("risk_type"),
              label = "Risk type",
              choices = list(Chronic = "chronic", Acute = "acute"),
              selected = "chronic")
            )
          ),
        full_screen = TRUE,
        bslib::nav_panel("Species Sensitivity", dygraphs::dygraphOutput(ns("r_plot")) |> withSpinner())
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

    output$hb_plot_storage <- dygraphs::renderDygraph({
      simulation() |>
        get_layer(paste0("hb", element_type())) |>
        plot(element_id = input$water_body,
             type = "storage",
             variable = input$hb_variable,
             dygraph_group = "dss")
    })

    output$hb_plot_flows <- dygraphs::renderDygraph({
      simulation() |>
        get_layer(paste0("hb", element_type())) |>
        plot(element_id = input$water_body,
             type = "flows",
             variable = input$hb_variable,
             dygraph_group = "dss")
    })

    output$ct_plot_water <- dygraphs::renderDygraph({
      simulation() |>
        get_layer(paste0("ct", element_type())) |>
        plot(element_id = input$water_body,
             compartment = "water",
             chemicals = input$chemical,
             dygraph_group = "dss")
    })

    output$ct_plot_sediment <- dygraphs::renderDygraph({
      simulation() |>
        get_layer(paste0("ct", element_type())) |>
        plot(element_id = input$water_body,
             compartment = "sediment",
             chemicals = input$chemical,
             dygraph_group = "dss")
    })

    output$r_plot <- dygraphs::renderDygraph({
      layer <- paste0("r", element_type())
      layer_obj <- get_layer(simulation(), layer)
      plot(layer_obj,
           element_id = input$water_body,
           type = input$risk_type,
           dygraph_group = "dss")
    })

  })
}


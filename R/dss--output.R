#' @importFrom bslib card card_header card_body card_footer
#' @importFrom shinycssloaders withSpinner
dss_output_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::page_fillable(
    title = "Output",

    shinyWidgets::pickerInput(ns("water_body"),
                              "Water body",
                              choices = water_body_choices()),

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
        bslib::nav_panel("Storage",
                         dygraphs::dygraphOutput(ns("hb_plot_storage"),
                                                 height = "600px"
                                                 ) |> withSpinner()
                         ),
        bslib::nav_panel("Flows", dygraphs::dygraphOutput(ns("hb_plot_flows"),
                                                          height = "600px"
                                                          ) |> withSpinner()
                         )
        ),
      bslib::navset_card_tab(
        title = shiny::div(
          "Exposure",
          bslib::popover(
            shiny_icon("gear"),
            shinyWidgets::checkboxGroupButtons(
              inputId = ns("chemical"),
              label = "Chemicals",
              choices = names(info_chemicals()),
              selected = c("Penoxsulam", "Difenoconazole"),
              individual = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon")),
              size = "sm"
            )
            )
          ),
        full_screen = TRUE,
        bslib::nav_panel(
          "Water",
          dygraphs::dygraphOutput(ns("ct_plot_water"), height = "600px") |>
            withSpinner()
                         ),
        bslib::nav_panel(
          "Sediment",
          dygraphs::dygraphOutput(ns("ct_plot_sediment"), height = "600px") |>
            withSpinner()
          )
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
        bslib::nav_panel(
          "Species Sensitivity",
          dygraphs::dygraphOutput(ns("r_plot"), height = "600px") |>
            withSpinner()
          )
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

water_body_choices <- function() {
  ditches <- info_ditches()$ditch
  names(ditches) <- paste0(seq_along(ditches), ": ", info_ditches()$ditch_name)
  clusters <- info_clusters()$cluster_id
  names(clusters) <- info_clusters()$cluster_name

  list(Lake = "Albufera Lake", Ditch = ditches, Cluster = clusters)
}

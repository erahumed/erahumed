#' @importFrom bslib card card_header card_body card_footer
#' @importFrom shinycssloaders withSpinner
dss_output_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::page_fillable(
    title = "Output",

    shinyWidgets::pickerInput(ns("water_body"),
                              "Water body",
                              choices = water_body_choices()),
    shiny::textOutput(ns("selected_wb_info")),

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
              selected = names(info_chemicals()),
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
          "Potentially Affected Fraction",
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
      if(!is.na(match(input$water_body, info_clusters()$element_id))) "c"
      else if (!is.na(match(input$water_body, info_ditches()$element_id))) "d"
      else "l"
    })

    output$selected_wb_info <- shiny::renderText({

      if ( element_type() == "c" ) {
        cvmap <- get_input(simulation(), "cluster_map")[["map_df"]]
        cvmap$variety <- cvmap$ms_id
        cvmap$element_id <- cvmap$cluster_id
        cvmap <- cvmap[, c("element_id", "variety")]

        cluster_data <- info_clusters() |>
          (\(.) .[.$element_id == input$water_body, ])() |>
          merge(cvmap, by = "element_id") |>
          merge(info_ditches(), by.x = "ditch_element_id", by.y = "element_id")

        paste0("Cluster: ", cluster_data$cluster_name,
               " - Ditch: ", cluster_data$ditch_name,
               " - Tancat: ", cluster_data$tancat,
               " - Variety: ", cluster_data$variety)
        } else if ( element_type() == "d" ) {
        ditch_data <- info_ditches() |>
          (\(.) .[.$element_id == input$water_body, ])()
        paste0("Ditch: ", ditch_data$ditch_name)
      } else {
        paste0("Albufera Lake")
      }

    })


    output$hb_plot_storage <- dygraphs::renderDygraph({
      plot_fun <- switch(element_type(),
                         c = plot_hbc, d = plot_hbd, l = plot_hbl)

      simulation() |>
        plot_fun(element_id = input$water_body,
                 type = "storage",
                 variable = input$hb_variable,
                 dygraph_group = "dss")
    })

    output$hb_plot_flows <- dygraphs::renderDygraph({
      plot_fun <- switch(element_type(),
                         c = plot_hbc, d = plot_hbd, l = plot_hbl)

      simulation() |>
        plot_fun(element_id = input$water_body,
                 type = "flows",
                 variable = input$hb_variable,
                 dygraph_group = "dss")
    })

    output$ct_plot_water <- dygraphs::renderDygraph({
      plot_fun <- switch(element_type(),
                         c = plot_ctc, d = plot_ctd, l = plot_ctl)

      simulation() |>
        plot_fun(
          element_id = input$water_body,
          compartment = "water",
          chemicals = input$chemical,
          dygraph_group = "dss")
    })

    output$ct_plot_sediment <- dygraphs::renderDygraph({
      plot_fun <- switch(element_type(),
                         c = plot_ctc, d = plot_ctd, l = plot_ctl)
      simulation() |>
        plot_fun(element_id = input$water_body,
                 compartment = "sediment",
                 chemicals = input$chemical,
                 dygraph_group = "dss")
    })

    output$r_plot <- dygraphs::renderDygraph({
      plot_fun <- switch(element_type(),
                         c = plot_rc, d = plot_rd, l = plot_rl)

      simulation() |>
        plot_fun(element_id = input$water_body,
                 type = input$risk_type,
                 dygraph_group = "dss")

    })

  })
}

water_body_choices <- function()
{
  ditches <- info_ditches()$element_id
  names(ditches) <- paste0(seq_along(ditches), ": ", info_ditches()$ditch_name)

  clusters <- info_clusters()$element_id
  names(clusters) <- info_clusters()$cluster_name

  return( list(Lake = "Albufera Lake", Ditch = ditches, Cluster = clusters) )
}

# Old dashboard, for internal use only
launch_dss_v0 <- function() {
  shiny::runApp(dss_v0(), launch.browser = TRUE)
}

dss_v0 <- function() {
  shiny::shinyApp(ui = dss_ui_v0, server = dss_server_v0)
}

dss_ui_v0 <- function() {
  app_title <- paste0("ERAHUMED v", utils::packageVersion("erahumed"))
  favicon_path <- system.file("app/www/favicon.ico", package = "erahumed")
  favicon <- base64enc::dataURI(file = favicon_path, mime = "image/x-icon")
  favicon_tag <- shiny::tags$head(
    shiny::tags$link(rel = "icon", type = "image/x-icon", href = favicon)
  )
  footer_style <-
    "position: fixed; bottom: 5px; right: 10px; font-size: 14px; color: gray; background-color: transparent;"
  footer <- shiny::tags$div(
    style = footer_style,
    shiny::p(paste0("ERAHUMED v", utils::packageVersion("erahumed")),
             "was developed with ", shiny::icon("r-project"), "and Shiny,",
             "by Valerio Gherardi.",
             "License:", shiny::a("GPL-3.0",
                                  href = "https://www.gnu.org/licenses/gpl-3.0.html",
                                  target = "_blank")
    )
  )


  sidebar_width <- 400

  map_output <- leaflet::leafletOutput("map", width = 0.99 * sidebar_width) |>
    shinycssloaders::withSpinner()

  body <- shinydashboard::dashboardBody(
    favicon_tag,
    footer,
    shinydashboard::tabItems(
      shinydashboard::tabItem("inp", inpUI("inp")),
      shinydashboard::tabItem("hba", hbaUI("hba")),
      shinydashboard::tabItem("hbp", hbpUI("hbp")),
      shinydashboard::tabItem("ca", caUI("ca")),
      shinydashboard::tabItem("ct", ctUI("ct"))
    )
  )

  sidebar <- shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem(
        "Simulation",
        shinydashboard::menuSubItem("INPut data", "inp"),
        shinydashboard::menuSubItem("Hydrological Balance (Albufera)", "hba"),
        shinydashboard::menuSubItem("Hydrological Balance (Paddies)", "hbp"),
        shinydashboard::menuSubItem("Chemical Applications", "ca"),
        shinydashboard::menuSubItem("Chemical Transport", "ct"),
        startExpanded = TRUE
      )
    ),


    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Map of the Albufera", map_output)
    ),

    shinydashboard::sidebarMenu(
      id = "help",
      shinydashboard::menuItem(
        "Help", icon = shiny::icon("question-circle"),
        shinydashboard::menuSubItem(
          "ERAHUMED Website", icon = shiny::icon("globe"),
          href = "https://erahumed.com",
          newtab = TRUE
        ),
        shinydashboard::menuSubItem(
          "R Package Website", icon = shiny::icon("r-project"),
          href = "https://erahumed.github.io/erahumed",
          newtab = TRUE
        ),
        shinydashboard::menuSubItem(
          "Report a bug", icon = shiny::icon("github"),
          href = "https://github.com/erahumed/erahumed/issues/new",
          newtab = TRUE
        ),
        shiny::hr(style = "border-top: 1px solid #000000;"),
        shinydashboard::menuSubItem(
          "Bird's eye view on simulation layers",
          href = "https://erahumed.github.io/erahumed/articles/pipeline-scheme.html",
          newtab = TRUE
        )
      )
    ),
    width = sidebar_width
  )

  header <- shinydashboard::dashboardHeader(title = app_title,
                                            titleWidth = sidebar_width)


  shinydashboard::dashboardPage(header, sidebar, body)
}

dss_server_v0 <- function(input, output, session) {

  shared <- shiny::reactiveValues(
    selected_cluster_id = albufera_clusters$cluster_id[1]
  )

  inp <- inpServer("inp", shared)
  hba <- hbaServer("hba", inp = inp, shared)
  hbp <- hbpServer("hbp", inp = inp, hba = hba, shared)
  ca <- caServer("ca", inp = inp, hba = hba, hbp = hbp, shared)
  ct <- ctServer("ct", inp = inp, hba = hba, hbp = hbp, ca = ca, shared)

  vmap <- shiny::reactive({ get_layer_aux(inp())[["cluster_variety_map"]] })

  output$map <- leaflet::renderLeaflet({
    plot_albufera_clusters(cluster_variety_map = vmap())
  }) |>
    shiny::snapshotExclude() |>
    identity()

  shiny::observeEvent(input$map_shape_click, {
    shared$selected_cluster_id <- input$map_shape_click$id
  })

  return(0)
}

#' ERAHUMED Decision Support System dashboard
#'
#' Launches the ERAHUMED Decision Support System dashboard Shiny app.
#'
#' @export
launch_app <- function() {
  shiny::runApp(shiny_app(), launch.browser = TRUE)
}

shiny_app <- function() {
  shiny::shinyApp(ui = shiny_ui, server = shiny_server)
}

shiny_ui <- function() {
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
        )
      )
    ),
    width = sidebar_width
  )

  header <- shinydashboard::dashboardHeader(title = app_title,
                                            titleWidth = sidebar_width)


  shinydashboard::dashboardPage(header, sidebar, body)
}

shiny_server <- function(input, output, session) {
  shared <- shiny::reactiveValues(
    selected_cluster_id = albufera_clusters$cluster_id[1]
  )

  simulation <- shiny::reactiveVal(erahumed_simulation())
  inp_res <- inpServer("inp", simulation, shared)
  hba_res <- hbaServer("hba", inp_res, shared)
  hbp_res <- hbpServer("hbp", hba_res, shared)
  ca_res <- caServer("ca", hbp_res, shared)
  ct_res <- ctServer("ct", ca_res, shared)

  output$map <- plot_albufera_clusters(
    cluster_variety_map =
      get_layer_aux(inp_res(), "inp")[["cluster_variety_map"]]
    ) |>
    leaflet::renderLeaflet() |>
    shiny::snapshotExclude()

  shiny::observeEvent(input$map_shape_click, {
    shared$selected_cluster_id <- input$map_shape_click$id
  })

}

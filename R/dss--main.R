#' @importFrom bslib nav_panel nav_item nav_menu nav_spacer
#' @importFrom shiny icon

dss_ui <- function() { bslib::page_navbar(
  id = "navbar",
  title = dss_title(),
  header = dss_header(),
  footer = dss_footer(),
  theme = bslib::bs_theme() |>
    bslib::bs_add_rules("
    .btn:disabled {
      background-color: #d3d3d3 !important; /* Light gray */
      color: #6c757d !important;           /* Dimmed text */
      border-color: #adb5bd !important;    /* Subtle border */
      opacity: 1 !important;               /* Ensure it's not too transparent */
      cursor: not-allowed !important;      /* Show 'not-allowed' cursor */
    }"
    ),
  selected = "Output",

  nav_panel("Input", dss_input_ui("dss_input"), icon = icon("sliders")),

  nav_panel("Output", dss_output_ui("dss_output"), icon = icon("chart-line")),

  nav_menu(icon = icon("wrench"), title = "Tools", align = "right",
           nav_action_link("show_map_card", "Show map", icon = "map"),
           nav_action_link("take_screenshot", "Screenshot", icon = "camera")
  ),

  nav_menu(icon = icon("question-circle"), title = "Help", align = "right",
           nav_action_link("show_about", "About this app", icon = "info"),
           nav_menu_hr(),  # ---------------------------------
           nav_hyperlink("User manual",
                         "https://erahumed.github.io/erahumed-book/",
                         icon = "book"),
           nav_menu_hr(),  # ---------------------------------
           nav_hyperlink("R Package Homepage",
                         "https://erahumed.github.io/erahumed",
                         icon = "r-project"),
           nav_hyperlink("Report a bug",
                         "https://github.com/erahumed/erahumed/issues/new",
                         icon = "github"),
           nav_menu_hr(),  # ---------------------------------
           nav_hyperlink("ERAHUMED Project Homepage", "https://erahumed.com",
                         icon = "globe")
  ),

  nav_spacer(),

  bslib::nav_item(
    shiny::actionButton("run",
                        "Run simulation",
                        width = "100%",
                        icon = shiny_icon("play"),
                        class = "btn btn-primary")
    ),
  bslib::nav_item(shiny::div(style = "margin-left: auto;",
                             shiny::div(class = "btn-group",
                                        shiny::tags$button(
                                          type = "button",
                                          class = "btn btn-outline-secondary dropdown-toggle",
                                          `data-bs-toggle` = "dropdown",
                                          `aria-expanded` = "false",
                                          shiny::icon("download"),
                                          "Download results"
                                        ),
                                        shiny::tags$ul(class = "dropdown-menu dropdown-menu-end",
                                                       shiny::tags$li(
                                                         shiny::downloadLink("download_csv", "Download as CSV", class = "dropdown-item")
                                                       ),
                                                       shiny::tags$li(
                                                         shiny::downloadLink("download_xlsx", "Download as Excel", class = "dropdown-item")
                                                       )
                                        )
                             )
  ))


)}


dss_server <- function(input, output, session) {
  clicked_cluster_id <- shiny::reactive({ input$map_shape_click$id })

  run <- shiny::reactiveVal(1)
  shiny::observe(run(run() + 1)) |> shiny::bindEvent(input$run)

  parameters <- dss_input_server("dss_input")

  # Here ID is the same as the dss output module, a little hack so that we can
  # easily interact with the dss_output UI from this server also.
  simulation <- dss_run_server("dss_output", parameters = parameters, run = run)
  dss_output_server("dss_output",
                    simulation = simulation,
                    clicked_cluster_id = clicked_cluster_id)

  output$download_csv <- shiny::downloadHandler(
    filename = function() paste0("erahumed-dss-results-", Sys.Date(), ".zip"),
    content = function(filename) {
      sim <- simulation()
      shiny::req(sim)
      shiny::showModal(shiny::modalDialog(
        title = "Please wait",
        shiny::tagList(
          shiny::icon("spinner", class = "fa-spin", style = "margin-right: 10px;"),
          "Preparing CSV download..."
        ),
        footer = NULL,
        easyClose = FALSE
      ))

      tryCatch({
        dss_download(filename, sim, format = "csv")
      }, error = function(e) {
        shiny::showNotification(paste("Download failed:", e$message), type = "error")
      }, finally = {
        shiny::removeModal()
      })
    }
  )

  output$download_xlsx <- shiny::downloadHandler(
    filename = function() paste0("erahumed-dss-results-", Sys.Date(), ".zip"),
    content = function(filename) {
      sim <- simulation()
      shiny::req(sim)
      shiny::showModal(shiny::modalDialog(
        title = "Please wait",
        shiny::tagList(
          shiny::icon("spinner", class = "fa-spin", style = "margin-right: 10px;"),
          "Preparing Excel download..."
        ),
        footer = NULL,
        easyClose = FALSE
      ))

      tryCatch({
        dss_download(filename, sim, format = "xlsx")
      }, error = function(e) {
        shiny::showNotification(paste("Download failed:", e$message), type = "error")
      }, finally = {
        shiny::removeModal()
      })
    }
  )



  output$map <- leaflet::renderLeaflet(
    plot_albufera_clusters(rfms_map = parameters()[["rfms_map"]])
    ) |>
    shiny::snapshotExclude()
  shiny::observeEvent(input$hide_map_card, shinyjs::hide("map_card"))
  shiny::observeEvent(input$show_map_card, shinyjs::show("map_card"))

  shiny::observeEvent(input$show_about, shiny::showModal(dss_about_modal()))

  shiny::observeEvent(input$take_screenshot, {
    timestr <- Sys.time() |> format() |> gsub("[^0-9]", "", x = _)
    filename <- paste0("erahumed-screenshot-", timestr)
    shinyscreenshot::screenshot(filename = filename)
    })

  sever::sever(
    opacity = 0.8,
    bg_color = "black",
    html = sever::sever_default(
      title = "Session Ended",
      subtitle = paste0("You have been disconnected from the server. ",
                        "Please check your internet connection and click ",
                        "the button below to attempt to reconnect. If the ",
                        "issue persists, please contact the administrators."),
      button = "Reconnect",
      button_class = "info"
  ))
}

#' @importFrom bslib nav_panel nav_item nav_menu nav_spacer
#' @importFrom shiny icon

dss_ui <- function() bslib::page_navbar(
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
    nav_action_link("show_about_modal", "About this app", icon = "info"),
    nav_menu_hr(),  # ---------------------------------
    nav_hyperlink("ERAHUMED Project Website", "https://erahumed.com", icon = "globe"),
    nav_menu_hr(),  # ---------------------------------
    nav_hyperlink("{erahumed} R Package Website", "https://erahumed.github.io/erahumed", icon = "r-project"),
    nav_hyperlink("Report a bug", "https://github.com/erahumed/erahumed/issues/new", icon = "github"),
    nav_menu_hr(),  # ---------------------------------
    nav_hyperlink("Bird's eye view on simulation layers",
      "https://erahumed.github.io/erahumed/articles/pipeline-scheme.html",
      icon = NULL
      )

    ),

  nav_spacer(),

  bslib::nav_item(
    shiny::actionButton("run",
                        "Run simulation",
                        width = "100%",
                        icon = shiny_icon("play"),
                        class = "btn btn-primary"
                        )
    )

)




dss_title <- function() {
  paste0("ERAHUMED v", utils::packageVersion("erahumed"))
}

dss_favicon <- function() {
  favicon_path <- system.file("app/www/favicon.ico", package = "erahumed")
  favicon <- base64enc::dataURI(file = favicon_path, mime = "image/x-icon")
  favicon_tag <- shiny::tags$head(
    shiny::tags$link(rel = "icon", type = "image/x-icon", href = favicon)
  )
}

dss_header <- function() {
  shiny::tagList(
    shinyjs::useShinyjs(),
    sever::useSever(),
    dss_favicon()
  )
}

dss_footer <- function() {
  footer_style <- paste(
    "text-align: right",
    "padding-right: 10px",
    "font-size: 14px",
    "color: gray",
    sep = "; "
  )

  gpl3_hl <- shiny_a("GPL-3.0", "https://www.gnu.org/licenses/gpl-3.0.html")

  r_icon <- shiny::icon("r-project")

  map_card <- close_card(
    full_screen = TRUE,
    leaflet::leafletOutput("map") |> shinycssloaders::withSpinner(),
    id = "map_card",
    style = "position: absolute; bottom: 50px; left: 50px; width: 300px;"
    )

  shiny::tagList(
    shiny::p(
      dss_title(),
      "was developed with ", r_icon, "and Shiny by Valerio Gherardi.",
      "License:", gpl3_hl,
      style = footer_style
    ),
    map_card,
    shiny::tags$head(shiny::tags$style(
      shiny::HTML("#map_card { display: none; }")
      ))
  )
}

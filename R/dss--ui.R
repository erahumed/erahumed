dss_ui <- function() {


  shinydashboard::menuSubItem(
    "Bird's eye view on simulation layers",
    href = "https://erahumed.github.io/erahumed/articles/pipeline-scheme.html",
    newtab = TRUE)


  bslib::page_navbar(
    title = dss_title(),
    header = dss_header(),
    footer = dss_footer(),

    bslib::nav_panel(
      icon = shiny::icon("sliders"),
      title = "Input",
      shiny::p("Input content.")
      ),

    bslib::nav_panel(
      icon = shiny::icon("chart-line"),
      title = "Output",
      shiny::p("Output content.")
    ),

    bslib::nav_spacer(),

    bslib::nav_menu(
      icon = shiny::icon("wrench"),
      title = "Tools",
      align = "right",
      bslib::nav_item(
        shiny::actionLink("show_map_card",
                          "Show map",
                          icon = shiny::icon("map") )
        ),
      bslib::nav_item(
        shiny::actionLink("take_screenshot",
                          "Screenshot",
                          icon = shiny::icon("camera") )
        )
      ),

    bslib::nav_menu(
      icon = shiny::icon("question-circle"),
      title = "Help",
      align = "right",

      bslib::nav_item(shiny::tags$a(
        shiny::icon("globe"), "ERAHUMED Project Website",
        href = "https://erahumed.com",
        target = "_blank"
        )),
      nav_menu_hr(),
      bslib::nav_item(shiny::tags$a(
        shiny::icon("r-project"), "{erahumed} R Package Website",
        href = "https://erahumed.github.io/erahumed",
        target = "_blank")),
      bslib::nav_item(shiny::tags$a(
        shiny::icon("github"), "Report a bug",
        href = "https://github.com/erahumed/erahumed/issues/new",
        target = "_blank"
        )),
      nav_menu_hr(),
      bslib::nav_item(shiny::tags$a(
        "Bird's eye view on simulation layers",
        href = "https://erahumed.github.io/erahumed/articles/pipeline-scheme.html",
        target = "_blank"
      ))

    )

  )




  }

nav_menu_hr <- function() {
  bslib::nav_item(shiny::tags$hr(
    style = "margin: 0.25rem 0; border: 0; border-top: 1px solid #666;"
    ))
}

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

  gpl3_hl <- shiny::a("GPL-3.0",
                      href = "https://www.gnu.org/licenses/gpl-3.0.html",
                      target = "_blank")

  r_icon <- shiny::icon("r-project")

  map_card <- bslib::card(
    full_screen = TRUE,
    bslib::card_body(leaflet::leafletOutput("map"),
                     shiny::actionButton("hide_map_card",
                                         label = shiny::icon("times"),
                                         class = "hide-map-btn",
                                         title = "Close"),
                     ),
    id = "map_card",
    style = "position: absolute; bottom: 50px; left: 50px; width: 300px;"
    )

  shiny::tagList(
    shiny::tags$style(shiny::HTML("
                                  .hide-map-btn {
                                    position: absolute;
                                    top: 5px;
                                    right: 5px;
                                    width: 30px; height: 30px;
                                    border-radius: 50%;
                                    border: none;
                                    background-color: #ffffff;
                                    color: #666;
                                    font-size: 18px;
                                    text-align: center;
                                    padding: 0;
                                    cursor: pointer;
                                    display: flex;
                                    align-items: center;
                                    justify-content: center;
                                    opacity: 0.6;
                                    }
                                  .hide-map-btn:hover { opacity: 1; }")),
    shiny::p(
      dss_title(),
      "was developed with ", r_icon, "and Shiny by Valerio Gherardi.",
      "License:", gpl3_hl,
      style = footer_style
    ),
    map_card
  )
}

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

dss_about_modal <- function() {
  logo_uri <- system.file("app/www/logo.png", package = "erahumed") |>
    base64enc::dataURI(file = _, mime = "image/x-icon")
  logo_icbibe_uri <-
    system.file("app/www/logo-icbibe.png", package = "erahumed") |>
    base64enc::dataURI(file = _, mime = "image/x-icon")

  gpl3_hl <- shiny_a("GPL-3.0", "https://www.gnu.org/licenses/gpl-3.0.html")
  r_icon <- shiny::icon("r-project")
  version <- paste0("v", format(utils::packageVersion("erahumed")))

  shiny::modalDialog(
    title = shiny::div(
      shiny::img(src = logo_uri, width = 80, style = "margin-right: 10px;"),
      "About ERAHUMED"
    ),
    shiny::div(
      style = "font-family: Arial, sans-serif;",
      shiny::p(shiny::strong("ERAHUMED"), version),
      shiny::p(
        "This software was developed with ", r_icon, "and Shiny by",
        "Valerio Gherardi, Pablo Amador and Andreu Rico.",
        "It is licensed to you under the terms of version 3 of the", gpl3_hl,
        " License."
      )
    ),
    footer = shiny::tagList(
      shiny::img(src = logo_icbibe_uri, height = 40, style = "position:absolute; left:10px;"),
      shiny::modalButton("Close")
    ),
    size = "xl",
    easyClose = TRUE
  )
}

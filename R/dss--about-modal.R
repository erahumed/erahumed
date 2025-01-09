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

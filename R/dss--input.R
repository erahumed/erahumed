#' @importFrom bslib card card_header card_body card_footer
dss_input_ui <- function(id) {
  layers_docs <- erahumed_docs("layers")

  ns <- shiny::NS(id)

  bslib::page_fillable(
    title = "Input",
    bslib::layout_column_wrap(
      card(card_header("Hydrology", class = "bg-dark"),
           card_body( dss_input_hydrology_ui(ns("hydrology")) ),
           full_screen = TRUE
           ),
      card(card_header("Exposure", class = "bg-dark"),
           card_body( dss_input_exposure_ui(ns("exposure")) ),
           full_screen = TRUE
           ),
      card(card_header("Risk", class = "bg-dark"),
           card_body( dss_input_risk_ui(ns("risk")) ),
           full_screen = TRUE
           )
      )
    )

}

dss_input_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    hydrology <- dss_input_hydrology_server("hydrology")
    exposure <- dss_input_exposure_server("exposure")
    risk <- dss_input_risk_server("risk")

    shiny::reactive( c(hydrology(), exposure(), risk()) )
  })
}

layer_card_header <- function(layer) {
  docs <- erahumed_docs("layers", layer)

  title <- docs[["title"]]
  description <- docs[["description"]]

  card_header(title,
              bslib::tooltip(
                shiny_icon("question-circle"),
                description,
                placement = "right"
              ),
              class = "bg-dark")

}

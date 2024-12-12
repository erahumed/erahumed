shiny_a <- function(text, href, icon = NULL, target = "_blank") {
  if (!is.null(icon)) {
    icon <- shiny::icon(icon)
  }
  shiny::a(icon, text, href = href, target = target)
}

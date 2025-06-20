shiny_icon <- function(icon) {
  if (!is.null(icon))
    icon <- shiny::icon(icon)
  return(icon)
}


shiny_a <- function(text, href, icon = NULL, target = "_blank") {
  shiny::a(shiny_icon(icon), text, href = href, target = target)
}

close_card <- function(...,
                       full_screen = FALSE,
                       height = NULL,
                       max_height = NULL,
                       min_height = NULL,
                       fill = TRUE,
                       class = NULL,
                       wrapper = bslib::card_body,
                       id = NULL
                       )
{
  close_btn <-
    shiny::actionButton(inputId = paste0("hide_", id),
                        label = shiny::icon("times"),
                        style = "position: absolute;
                                 top: 5px;
                                 right: 5px;
                                 width: 30px;
                                 height: 30px;
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
                                 z-index: 9999;",
                        onmouseover = "this.style.opacity=1;",
                        onmouseout = "this.style.opacity=0.6;",
                        title = "Hide")

  bslib::card(close_btn,
              ...,
              full_screen = full_screen,
              height = height,
              max_height = max_height,
              min_height = min_height,
              fill = fill,
              class = class,
              wrapper = wrapper,
              id = id,
              style = "z-index: 9999"
  )
}


# Helpers for bslib::page_navbar()-based UI.
nav_action_link <- function(id, label, icon)
  nav_item(shiny::actionLink(inputId = id,
                             label = label,
                             icon = shiny_icon(icon)
  ))

nav_hyperlink <- function(text, href, icon = NULL)
  nav_item(shiny_a(text = text,
                   href = href,
                   icon = icon))

nav_menu_hr <- function() {
  bslib::nav_item(shiny::tags$hr(
    style = "margin: 0.25rem 0; border: 0; border-top: 1px solid #666;"
  ))
}

add_inline_label <- function(input_element, label, id = NULL) {
  shiny::div(
    class = "mb-2 d-flex align-items-center gap-2",
    shiny::tags$label(label, `for` = id, class = "form-label mb-0 me-2", style = "min-width: 180px;"),
    input_element
  )
}

inline_numeric_input <- function(inputId, label, value = NA, min = NA, max = NA, step = NA, width = NULL) {
  shiny::numericInput(inputId = inputId, label = NULL, value = value, min = min, max = max, step = step, width = width) |>
    add_inline_label(label = label, id = inputId)
}

inline_text_input <- function(id, label, value = "", width = NULL, placeholder = NULL) {
  shiny::textInput(id, label = NULL, value = value, width = width, placeholder = placeholder) |>
    add_inline_label(label = label, id = id)
}

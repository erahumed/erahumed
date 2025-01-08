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
              id = id
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



# UI elements for erahumed parameters and dataset columns
param_tooltip <- function(layer, param) {
  desc <- erahumed_docs("layers", layer, "parameters", param, "description")

  param_md <- desc |>
    gsub("\\\\link(?:\\[[^]]+\\])?\\{([^}]+)\\}", "`\\1`", x = _)

  bslib::tooltip(shiny_icon("question-circle"), param_md, placement = "right")
}

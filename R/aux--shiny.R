shiny_a <- function(text, href, icon = NULL, target = "_blank") {
  if (!is.null(icon)) {
    icon <- shiny::icon(icon)
  }
  shiny::a(icon, text, href = href, target = target)
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
                                 opacity: 0.6;",
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

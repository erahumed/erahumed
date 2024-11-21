# Customized shiny::absolutePanel() with minimization capability
absolutePanel2 <- function(
    id,
    ...,
    restore_button_text = paste("Restore", id),
    top = NULL,
    right = NULL,
    bottom = NULL,
    left = NULL,
    width = NULL,
    height = NULL,
    cursor = "default",
    draggable = TRUE
    )
{
  restore_btn_style <-
    "display: none; position: fixed; bottom: 10px; right: 10px; z-index: 11;"
  absolute_panel_style <-
    "z-index: 10; background-color: #ffffff; padding: 10px; border: 1px solid #ccc; border-radius: 5px;"

  minimize_js <- "
    $(document).ready(function() {

      $('#<ID>-close').click(function() {
        $('#<ID>').hide();
        $('#<ID>-restore').show();
      });

      $('#<ID>-restore').click(function() {
        $('#<ID>').show();
        $('#<ID>-restore').hide();
      });

    });
  " |> gsub("<ID>", id, x = _, fixed = TRUE)

  shiny::tagList(

    shiny::tags$button(
      id = paste0(id, "-restore"),
      restore_button_text,
      class = "btn btn-primary",
      style = restore_btn_style
      ),

    absolutePanel(
      id = id,
      top = top,
      right = right,
      bottom = bottom,
      left = left,
      width = width,
      height = height,
      draggable = draggable,
      cursor = cursor,
      style = absolute_panel_style,

      shiny::div(style = "text-align: right;",
                 shiny::tags$button("X",
                                    id = paste0(id, "-close"),
                                    class = "btn btn-danger btn-sm"
                                    )
                 ),

      # Panel content passed via ...
      shiny::div(...),

      # JavaScript to handle minimize/restore
      shiny::tags$script( shiny::HTML(minimize_js) )
    )
  )
}

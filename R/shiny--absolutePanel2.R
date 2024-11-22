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
    draggable = TRUE,
    transparency = 0.6
    )
{
  restore_btn_style <-
    "display: none; position: fixed; bottom: 10px; right: 10px; z-index: 11;"
  absolute_panel_style <- sprintf(
    "z-index: 10; background-color: rgba(255, 255, 255, %f); padding: 10px; border: 1px solid #ccc; border-radius: 5px;",
    transparency)
  minimize_btn_style <-
    "border-radius: 5px; width: 30px; height: 30px; padding: 0; font-size: 18px; border: none; text-align: center; line-height: 1.2;"

  minimize_js <- "
    $(document).ready(function() {

      $('#<ID>-minimize').click(function() {
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

    shiny::absolutePanel(
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
                 shiny::tags$button(id = paste0(id, "-minimize"),
                                    class = "btn btn-primary btn-sm",
                                    style = minimize_btn_style,
                                    shiny::HTML("&lowbar;")
                                    )
                 ),

      # Panel content passed via ...
      shiny::div(...),

      # JavaScript to handle minimize/restore
      shiny::tags$script( shiny::HTML(minimize_js) )
    )
  )
}

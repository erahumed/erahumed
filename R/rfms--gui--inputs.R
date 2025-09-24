rfms_input_crop_calendar <- function(id) {


  slider_css <- "
    /* palette */
    :root { --per: #ADD8E6; --per-alpha: rgba(33,150,243,.45); --sow: #4682b4; }

    /* All connects default to Perellona color (covers left & right) */
    #_CALENDAR_ID_ .noUi-connects .noUi-connect {
      background: var(--per-alpha) !important;
    }

    /* The middle connect (the 2nd of the rendered ones) = Sowing */
    #_CALENDAR_ID_ .noUi-connects .noUi-connect:nth-child(2) {
      background: var(--sow) !important;
    }

   /* Slimmer track */
    #_CALENDAR_ID_ .noUi-target { border: 0; box-shadow: none; }
    #_CALENDAR_ID_ .noUi-base   { background: var(--track); /* height: 10px; border-radius: 6px; */}

    #_CALENDAR_ID_ .noUi-horizontal .noUi-handle {
      width: 18px !important;    /* narrower */
      height: 18px;   /* shorter */
      top: -1px;
      border-radius: 0;
      background: #fff;
    }

    /* remove inner grip bars */
    #_CALENDAR_ID_ .noUi-handle:before,
    #_CALENDAR_ID_ .noUi-handle:after { display: none; }

    /* Smaller tooltips */
    #_CALENDAR_ID_ .noUi-tooltip {
      font-size: 11px;
      padding: 1px 4px;
      border-radius: 3px;
    }

    /* Tighten vertical spacing */
    #_CALENDAR_ID_ { margin-top: 4px; }
    .small .legend { vertical-align: middle; }

    /* optional: lighten the unfilled gaps on hover/active (visual consistency) */
    #_CALENDAR_ID_ .noUi-base { background: rgba(0,0,0,0.08); }
    /* legend pills */
    .legend { display:inline-block; width:14px; height:10px; border-radius:3px; margin-right:8px; }
    .legend.per { background: var(--per-alpha); }
    .legend.sow { background: var(--sow); }
  " |> gsub("_CALENDAR_ID_", id, x = _, fixed = TRUE)

  shiny::tagList(
    shiny::tags$style(shiny::HTML(slider_css)),
    shinyWidgets::noUiSliderInput(
      inputId = id,
      label   = NULL,
      min = 1, max = 366, step = 1,
      value   = c(15, 111, 251, 306),
      connect = c(TRUE, FALSE, TRUE, FALSE, TRUE),
      margin = 1,
      tooltips = TRUE,
      format = shinyWidgets::wNumbFormat(decimals = 0)
      ),
    shiny::div(class="mt-2",
               shiny::span(class="legend per"), " Perellona  ",
               shiny::span(class="legend sow"), " Sowing season"
               )
    )

}

rfms_input_sowing_season <- function(id) {
  tltp <- bslib::tooltip(trigger = shiny_icon("question-circle"),
                         "Days of the year marking the start and end of the sowing season (1-366, assuming a leap year)",
                         placement = "right")


  shiny::sliderInput(id,
                     shiny::p("Sowing season", tltp),
                     min = 1,
                     max = 366,
                     step = 1,
                     value = c(rfms_input_defaults()$sowing_yday,
                               rfms_input_defaults()$harvesting_yday),
                     ticks = FALSE,
                     dragRange = TRUE
                     )
}

rfms_input_perellona_start_yday <- function(id) {
  inline_numeric_input(
    id,
    label = rfms_input_label("perellona_start_yday"),
    value = rfms_input_defaults()$perellona_start_yday,
    min = 1, max = 366
  )
}

rfms_input_perellona_end_yday <- function(id) {
  inline_numeric_input(
    id,
    label = rfms_input_label("perellona_end_yday"),
    value = rfms_input_defaults()$perellona_end_yday,
    min = 1, max = 366
  )
}

rfms_input_harvesting_yday <- function(id) {
  inline_numeric_input(
    id,
    label = rfms_input_label("harvesting_yday"),
    value = rfms_input_defaults()$harvesting_yday,
    min = 1, max = 366
  )
}

rfms_input_flow_height_cm <- function(id) {
  inline_numeric_input(
    id,
    label = rfms_input_label("flow_height_cm"),
    value = rfms_input_defaults()$flow_height_cm,
    min = 0
  )
}

rfms_input_perellona_height_cm <- function(id) {
  inline_numeric_input(
    id,
    label = rfms_input_label("perellona_height_cm"),
    value = rfms_input_defaults()$perellona_height_cm,
    min = 0
  )
}

rfms_input_chemical_id <- function(id, choices) {
  inline_select_input(id, "Select chemical", choices = choices)
}

rfms_input_seed_day <- function(id, max) {
  inline_numeric_input(
    id,
    label = application_input_label("seed_day"),
    value = NA,
    min = 1,
    max = max
  )
}

rfms_input_amount_kg_ha <- function(id) {
  inline_numeric_input(
    id,
    label = application_input_label("amount_kg_ha"),
    value = NA,
    min = 0
  )
}

rfms_input_type <- function(id) {
  inline_select_input(
    id,
    label = application_input_label("type"),
    choices = c("ground", "aerial"),
    selected = NA
  )
}

rfms_input_emptying_days <- function(id) {
  inline_numeric_input(
    id,
    label = application_input_label("emptying_days"),
    value = NA,
    min = 1,
    step = 1
  )
}


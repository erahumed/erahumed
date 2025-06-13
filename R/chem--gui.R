chemical_db_ui <- function(id)
{
  ns <- shiny::NS(id)

  list_manager_ui(ns("manager"),
                  object_name = "Chemical",
                  list_description = "list"
                  )
}

chemical_db_server <- function(id)
{
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    list_manager_server("manager",
                        item_editor_ui = chemical_editor_ui,
                        item_editor_server = chemical_editor_server,
                        item_display_function = function(x) x$display_name,
                        default_items = default_chemical_db()
                        )
  })
}


chemical_editor_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_column_wrap(
    width = 1,  # One column to stack cards vertically
    gap = "1rem",
    heights_equal = "row",

    ## General info
    bslib::card(
      bslib::card_header("General Information"),
      bslib::card_body(bslib::layout_column_wrap(
        shiny::textInput(ns("display_name"), "Display name", value = NA),
        shiny::textInput(ns("tmoa_id"), "TMoA ID", value = NA),
        shiny::numericInput(ns("MW"), "Molecular Weight (g/mol)", value = NA),
        shiny::numericInput(ns("sol_ppm"), "Solubility (ppm)", value = NA),
        shiny::numericInput(ns("koc_cm3_g"), "Koc (cm<sup>3</sup>/g)", value = NA),
        shiny::numericInput(ns("fet_cm"), "Film exchange thickness (cm)", value = NA),
        shiny::numericInput(ns("dinc_m"), "Incorporation depth (m)", value = NA)
      ))
    ),

    ## Degradation parameters
    bslib::card(
      bslib::card_header("Degradation Parameters"),
      bslib::card_body(
        bslib::layout_column_wrap(
          shiny::numericInput(ns("kf_day"), "k<sub>f</sub> (1/day)", value = NA),
          shiny::numericInput(ns("kw_day"), "k<sub>w</sub> (1/day)", value = NA),
          shiny::numericInput(ns("ks_sat_day"), "k<sub>s_sat</sub> (1/day)", value = NA),
          shiny::numericInput(ns("ks_unsat_day"), "k<sub>s_unsat</sub> (1/day)", value = NA),
          shiny::numericInput(ns("ksetl_m_day"), "k<sub>setl</sub> (m/day)", value = NA),
          shiny::numericInput(ns("kvolat_m_day"), "k<sub>volat</sub> (m/day)", value = NA),

          shiny::numericInput(ns("kw_temp"), "T ref for k<sub>w</sub> (°C)", value = NA),
          shiny::numericInput(ns("ks_sat_temp"), "T ref for k<sub>s_sat</sub> (°C)", value = NA),
          shiny::numericInput(ns("ks_unsat_temp"), "T ref for k<sub>s_unsat</sub> (°C)", value = NA),

          shiny::numericInput(ns("Q10_kw"), "Q<sub>10</sub> for k<sub>w</sub>", value = NA),
          shiny::numericInput(ns("Q10_ks_sat"), "Q<sub>10</sub> for k<sub>s_sat</sub>", value = NA),
          shiny::numericInput(ns("Q10_ks_unsat"), "Q<sub>10</sub> for k<sub>s_unsat</sub>", value = NA)
        )
      )
    ),

    ## Toxicology (SSD)
    bslib::card(
      bslib::card_header("Toxicological Properties (SSD Parameters)"),
      bslib::card_body(
        bslib::layout_column_wrap(
          shiny::numericInput(ns("ssd_acute_mu"), "SSD Acute μ", value = NA),
          shiny::numericInput(ns("ssd_acute_sigma"), "SSD Acute σ", value = NA),
          shiny::numericInput(ns("ssd_chronic_mu"), "SSD Chronic μ", value = NA),
          shiny::numericInput(ns("ssd_chronic_sigma"), "SSD Chronic σ", value = NA)
        )
      )
    )
  )
}

chemical_editor_server <- function(id, item = shiny::reactive(NULL))
{
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      shiny::req(item())

      shiny::updateTextInput(inputId = "display_name", value = item()$display_name)
      shiny::updateTextInput(inputId = "tmoa_id", value = item()$tmoa_id)
      shiny::updateNumericInput(inputId = "MW", value = item()$MW)
      shiny::updateNumericInput(inputId = "sol_ppm", value = item()$sol_ppm)
      shiny::updateNumericInput(inputId = "koc_cm3_g", value = item()$koc_cm3_g)
      shiny::updateNumericInput(inputId = "fet_cm", value = item()$fet_cm)
      shiny::updateNumericInput(inputId = "dinc_m", value = item()$dinc_m)
      shiny::updateNumericInput(inputId = "kf_day", value = item()$kf_day)
      shiny::updateNumericInput(inputId = "kw_day", value = item()$kw_day)
      shiny::updateNumericInput(inputId = "ks_sat_day", value = item()$ks_sat_day)
      shiny::updateNumericInput(inputId = "ks_unsat_day", value = item()$ks_unsat_day)
      shiny::updateNumericInput(inputId = "ksetl_m_day", value = item()$ksetl_m_day)
      shiny::updateNumericInput(inputId = "kvolat_m_day", value = item()$kvolat_m_day)

      shiny::updateNumericInput(inputId = "kw_temp", value = item()$kw_temp)
      shiny::updateNumericInput(inputId = "ks_sat_temp", value = item()$ks_sat_temp)
      shiny::updateNumericInput(inputId = "ks_unsat_temp", value = item()$ks_unsat_temp)

      shiny::updateNumericInput(inputId = "Q10_kw", value = item()$Q10_kw)
      shiny::updateNumericInput(inputId = "Q10_ks_sat", value = item()$Q10_ks_sat)
      shiny::updateNumericInput(inputId = "Q10_ks_unsat", value = item()$Q10_ks_unsat)
      shiny::updateNumericInput(inputId = "ssd_acute_mu", value = item()$ssd_acute_mu)
      shiny::updateNumericInput(inputId = "ssd_acute_sigma", value = item()$ssd_acute_sigma)
      shiny::updateNumericInput(inputId = "ssd_chronic_mu", value = item()$ssd_chronic_mu)
      shiny::updateNumericInput(inputId = "ssd_chronic_sigma", value = item()$ssd_chronic_sigma)
    })

    shiny::reactive(
      chemical(
        display_name = input$display_name,
        tmoa_id = input$tmoa_id,
        MW = input$MW,
        ksetl_m_day = input$ksetl_m_day,
        kvolat_m_day = input$kvolat_m_day,
        sol_ppm = input$sol_ppm,
        koc_cm3_g = input$koc_cm3_g,
        dinc_m = input$dinc_m,
        fet_cm = input$fet_cm,
        kf_day = input$kf_day,
        kw_day = input$kw_day,
        ks_sat_day = input$ks_sat_day,
        ks_unsat_day = input$ks_unsat_day,
        kw_temp = input$kw_temp,
        ks_sat_temp = input$ks_sat_temp,
        ks_unsat_temp = input$ks_unsat_temp,
        Q10_kw = input$Q10_kw,
        Q10_ks_sat = input$Q10_ks_sat,
        Q10_ks_unsat = input$Q10_ks_unsat,
        ssd_acute_mu = input$ssd_acute_mu,
        ssd_acute_sigma = input$ssd_acute_sigma,
        ssd_chronic_mu = input$ssd_chronic_mu,
        ssd_chronic_sigma = input$ssd_chronic_sigma
    ))
  })
}

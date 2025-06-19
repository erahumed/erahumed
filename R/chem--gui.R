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


  shiny::tagList(
    inline_text_input(ns("display_name"), "Compound name", value = NA),
    bslib::accordion(
      open = FALSE,
      multiple = TRUE,

      ## Partitioning and mobility
      bslib::accordion_panel("Partitioning and Mobility",
                             inline_numeric_input(ns("MW"), shiny::HTML("Molecular Weight (g/mol)"), value = NA),
                             inline_numeric_input(ns("sol_ppm"), shiny::HTML("Solubility (ppm)"), value = NA),
                             inline_numeric_input(ns("koc_cm3_g"), shiny::HTML("K<sub>oc</sub> (cm<sup>3</sup>/g)"), value = NA),
                             inline_numeric_input(ns("fet_cm"), shiny::HTML("Film exchange thickness (cm)"), value = NA),
                             inline_numeric_input(ns("dinc_m"), shiny::HTML("Incorporation depth (m)"), value = NA),
                             inline_numeric_input(ns("ksetl_m_day"), shiny::HTML("k<sub>setl</sub> (m/day)"), value = NA),
                             inline_numeric_input(ns("kvolat_m_day"), shiny::HTML("k<sub>volat</sub> (m/day)"), value = NA)
      ),

      ## Degradation parameters
      bslib::accordion_panel("Degradation Constants",
                             inline_numeric_input(ns("kf_day"), shiny::HTML("k<sub>f</sub> (1/day)"), value = NA),
                             inline_numeric_input(ns("kw_day"), shiny::HTML("k<sub>w</sub> (1/day)"), value = NA),
                             inline_numeric_input(ns("ks_sat_day"), shiny::HTML("k<sub>s,sat</sub> (1/day)"), value = NA),
                             inline_numeric_input(ns("ks_unsat_day"), shiny::HTML("k<sub>s,unsat</sub> (1/day)"), value = NA)
      ),

      ## Temperature dependence
      bslib::accordion_panel("Temperature Dependence of Degradation",
                             inline_numeric_input(ns("kw_temp"), shiny::HTML("T<sub>ref</sub> for k<sub>w</sub> (°C)"), value = NA),
                             inline_numeric_input(ns("ks_sat_temp"), shiny::HTML("T<sub>ref</sub> for k<sub>s,sat</sub> (°C)"), value = NA),
                             inline_numeric_input(ns("ks_unsat_temp"), shiny::HTML("T<sub>ref</sub> for k<sub>s,unsat</sub> (°C)"), value = NA),
                             inline_numeric_input(ns("Q10_kw"), shiny::HTML("Q<sub>10</sub> for k<sub>w</sub>"), value = NA),
                             inline_numeric_input(ns("Q10_ks_sat"), shiny::HTML("Q<sub>10</sub> for k<sub>s,sat</sub>"), value = NA),
                             inline_numeric_input(ns("Q10_ks_unsat"), shiny::HTML("Q<sub>10</sub> for k<sub>s,unsat</sub>"), value = NA)
      ),

      ## Toxicology (SSD)
      bslib::accordion_panel("Toxicological Properties",
                             inline_text_input(ns("tmoa_id"), "TMoA ID", value = NA),
                             inline_numeric_input(ns("ssd_acute_mu"), shiny::HTML("SSD Acute &mu;"), value = NA),
                             inline_numeric_input(ns("ssd_acute_sigma"), shiny::HTML("SSD Acute &sigma;"), value = NA),
                             inline_numeric_input(ns("ssd_chronic_mu"), shiny::HTML("SSD Chronic &mu;"), value = NA),
                             inline_numeric_input(ns("ssd_chronic_sigma"), shiny::HTML("SSD Chronic &sigma;"), value = NA)
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

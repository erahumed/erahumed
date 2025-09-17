chemical_db_ui <- function(id)
{
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3("Chemical definitions"),
    list_manager_ui(ns("manager"), object_name = "Chemical")
  )

}

chemical_db_server <- function(id)
{
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    list_manager_server("manager",
                        item_editor_ui = chemical_editor_ui,
                        item_editor_server = chemical_editor_server,
                        item_display_function = chemical_display_function,
                        default_items = default_chemical_db()
                        )
  })
}

chemical_editor_ui <- function(id) {
  ns <- shiny::NS(id)


  shiny::tagList(
    chem_input_display_name(ns("display_name")),
    bslib::accordion(
      open = FALSE,
      multiple = TRUE,

      ## Partitioning and mobility
      bslib::accordion_panel("Partitioning and Mobility",
                             chem_input_MW(ns("MW")),
                             chem_input_sol_ppm(ns("sol_ppm")),
                             chem_input_koc_cm3_g(ns("koc_cm3_g")),
                             chem_input_fet_cm(ns("fet_cm"))
      ),

      bslib::accordion_panel("Degradation Constants",
                             chem_input_kf_day(ns("kf_day")),
                             chem_input_kw_day(ns("kw_day")),
                             chem_input_ks_sat_day(ns("ks_sat_day")),
                             chem_input_ks_unsat_day(ns("ks_unsat_day"))
      ),

      bslib::accordion_panel("Temperature Dependence of Degradation",
                             chem_input_kw_temp(ns("kw_temp")),
                             chem_input_ks_sat_temp(ns("ks_sat_temp")),
                             chem_input_ks_unsat_temp(ns("ks_unsat_temp")),
                             chem_input_Q10_kw(ns("Q10_kw")),
                             chem_input_Q10_ks_sat(ns("Q10_ks_sat")),
                             chem_input_Q10_ks_unsat(ns("Q10_ks_unsat"))
      ),

      bslib::accordion_panel("Toxicological Properties",
                             chem_input_tmoa_id(ns("tmoa_id")),
                             chem_input_ssd_acute_mu(ns("ssd_acute_mu")),
                             chem_input_ssd_acute_sigma(ns("ssd_acute_sigma")),
                             chem_input_ssd_chronic_mu(ns("ssd_chronic_mu")),
                             chem_input_ssd_chronic_sigma(ns("ssd_chronic_sigma"))
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
      shiny::updateNumericInput(inputId = "kf_day", value = item()$kf_day)
      shiny::updateNumericInput(inputId = "kw_day", value = item()$kw_day)
      shiny::updateNumericInput(inputId = "ks_sat_day", value = item()$ks_sat_day)
      shiny::updateNumericInput(inputId = "ks_unsat_day", value = item()$ks_unsat_day)

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
        sol_ppm = input$sol_ppm,
        koc_cm3_g = input$koc_cm3_g,
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

chemical_display_function <- function(x) {
  paste0(x$display_name, " (", x$tmoa_id, ")")
}

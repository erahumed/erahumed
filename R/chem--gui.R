chemical_db_ui <- function(id)
{
  ns <- shiny::NS(id)

  bslib::page_fillable(
    title = "Chemical database",
    shiny::tags$script(shiny::HTML(
      "window.edit_counter = 0; window.delete_counter = 0;")
      ),
    shinyjs::useShinyjs(),
    shiny::actionButton(ns("add_chemical"), "Add Chemical"),
    shiny::tags$h4("Defined chemicals:"),
    shiny::uiOutput(ns("db_output"))
    )
}

chemical_db_server <- function(id)
{
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    db <- shiny::reactiveVal(default_chemical_db())
    onclick_js <- function(i, idx_input_name, idx_trigger_name, counter_name)
      paste("%s++",
            "Shiny.setInputValue('%s', %s)",
            "Shiny.setInputValue('%s', %s)",
            sep = "; "
            ) |>
      sprintf(counter_name, idx_input_name, i, idx_trigger_name, counter_name)
    edit_onclick_js <- function(i)
      onclick_js(i, ns("edit_idx"), ns("edit_trigger"), "window.edit_counter")
    delete_onclick_js <- function(i)
      onclick_js(i, ns("delete_idx"), ns("delete_trigger"), "window.delete_counter")


    output$db_output <- shiny::renderUI({
      if (length(db()) == 0) {
        shiny::tags$em("No chemicals defined yet.")
      } else {
        shiny::tags$ul(
          lapply(seq_along(db()), function(i) {
            chem <- db()[[i]]
            shiny::tags$li(
              shiny::strong(chem$display_name),
              " (TMoA ID: ", chem$tmoa_id, ", MW: ", chem$MW, ") ",
              shiny::actionLink(paste0(ns("edit_btn_"), i),
                                "Edit",
                                onclick = edit_onclick_js(i)),
              " | ",
              shiny::actionLink(paste0(ns("delete_btn_"), i),
                                label = "Delete",
                                onclick = delete_onclick_js(i))
            )
          })
        )
      }
    })


    open_editor_modal <- function(title, chemical = NULL) {
      shiny::showModal(
        session = session,
        shiny::modalDialog(
          title = title,
          bslib::layout_column_wrap(
            width = 1,  # One column to stack cards vertically
            gap = "1rem",
            heights_equal = "row",

            ## General info
            bslib::card(
              bslib::card_header("General Information"),
              bslib::card_body(bslib::layout_column_wrap(
                shiny::textInput(ns("display_name"), "Display name", value = chemical$display_name %||% ""),
                shiny::textInput(ns("tmoa_id"), "TMoA ID", value = chemical$tmoa_id %||% ""),
                shiny::numericInput(ns("MW"), "Molecular Weight (g/mol)", value = chemical$MW %||% NA),
                shiny::numericInput(ns("sol_ppm"), "Solubility (ppm)", value = chemical$sol_ppm %||% NA),
                shiny::numericInput(ns("koc_cm3_g"), "Koc (cm<sup>3</sup>/g)", value = chemical$koc_cm3_g %||% NA),
                shiny::numericInput(ns("fet_cm"), "Film exchange thickness (cm)", value = chemical$fet_cm %||% NA),
                shiny::numericInput(ns("dinc_m"), "Incorporation depth (m)", value = chemical$dinc_m %||% NA)
              ))
            ),

            ## Degradation parameters
            bslib::card(
              bslib::card_header("Degradation Parameters"),
              bslib::card_body(
                bslib::layout_column_wrap(
                  shiny::numericInput(ns("kf_day"), "k<sub>f</sub> (1/day)", value = chemical$kf_day %||% NA),
                  shiny::numericInput(ns("kw_day"), "k<sub>w</sub> (1/day)", value = chemical$kw_day %||% NA),
                  shiny::numericInput(ns("ks_sat_day"), "k<sub>s_sat</sub> (1/day)", value = chemical$ks_sat_day %||% NA),
                  shiny::numericInput(ns("ks_unsat_day"), "k<sub>s_unsat</sub> (1/day)", value = chemical$ks_unsat_day %||% NA),
                  shiny::numericInput(ns("ksetl_m_day"), "k<sub>setl</sub> (m/day)", value = chemical$ksetl_m_day %||% NA),
                  shiny::numericInput(ns("kvolat_m_day"), "k<sub>volat</sub> (m/day)", value = chemical$kvolat_m_day %||% NA),

                  shiny::numericInput(ns("kw_temp"), "T ref for k<sub>w</sub> (°C)", value = chemical$kw_temp %||% NA),
                  shiny::numericInput(ns("ks_sat_temp"), "T ref for k<sub>s_sat</sub> (°C)", value = chemical$ks_sat_temp %||% NA),
                  shiny::numericInput(ns("ks_unsat_temp"), "T ref for k<sub>s_unsat</sub> (°C)", value = chemical$ks_unsat_temp %||% NA),

                  shiny::numericInput(ns("Q10_kw"), "Q<sub>10</sub> for k<sub>w</sub>", value = chemical$Q10_kw %||% NA),
                  shiny::numericInput(ns("Q10_ks_sat"), "Q<sub>10</sub> for k<sub>s_sat</sub>", value = chemical$Q10_ks_sat %||% NA),
                  shiny::numericInput(ns("Q10_ks_unsat"), "Q<sub>10</sub> for k<sub>s_unsat</sub>", value = chemical$Q10_ks_unsat %||% NA)
                )
              )
            ),

            ## Toxicology (SSD)
            bslib::card(
              bslib::card_header("Toxicological Properties (SSD Parameters)"),
              bslib::card_body(
                bslib::layout_column_wrap(
                  shiny::numericInput(ns("ssd_acute_mu"), "SSD Acute μ", value = chemical$ssd_acute_mu %||% NA),
                  shiny::numericInput(ns("ssd_acute_sigma"), "SSD Acute σ", value = chemical$ssd_acute_sigma %||% NA),
                  shiny::numericInput(ns("ssd_chronic_mu"), "SSD Chronic μ", value = chemical$ssd_chronic_mu %||% NA),
                  shiny::numericInput(ns("ssd_chronic_sigma"), "SSD Chronic σ", value = chemical$ssd_chronic_sigma %||% NA)
                )
              )
            )
          ),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(ns("save_chemical"), "Save")
          ),
          size = "l", easyClose = TRUE, scrollable = TRUE
        )
      )
    }

    shiny::observe({
      shinyjs::runjs(sprintf("Shiny.setInputValue('edit_idx', %s)", length(db()) + 1))
      open_editor_modal(title = "Add New Chemical")
    }) |> shiny::bindEvent(input$add_chemical)

    shiny::observe(
      open_editor_modal(title = "Edit Chemical", chemical = db()[[input$edit_idx]])
      ) |>
      shiny::bindEvent(input$edit_trigger, ignoreNULL = TRUE)

    shiny::observeEvent(input$save_chemical, {

      tryCatch(
        new_chem <- chemical(
          display_name      = input$display_name,
          tmoa_id           = input$tmoa_id,
          MW                = input$MW,
          ksetl_m_day       = input$ksetl_m_day,
          kvolat_m_day      = input$kvolat_m_day,
          sol_ppm           = input$sol_ppm,
          koc_cm3_g         = input$koc_cm3_g,
          dinc_m            = input$dinc_m,
          fet_cm            = input$fet_cm,
          kf_day            = input$kf_day,
          kw_day            = input$kw_day,
          ks_sat_day        = input$ks_sat_day,
          ks_unsat_day      = input$ks_unsat_day,
          kw_temp           = input$kw_temp,
          ks_sat_temp       = input$ks_sat_temp,
          ks_unsat_temp     = input$ks_unsat_temp,
          Q10_kw            = input$Q10_kw,
          Q10_ks_sat        = input$Q10_ks_sat,
          Q10_ks_unsat      = input$Q10_ks_unsat,
          ssd_acute_mu      = input$ssd_acute_mu,
          ssd_acute_sigma   = input$ssd_acute_sigma,
          ssd_chronic_mu    = input$ssd_chronic_mu,
          ssd_chronic_sigma = input$ssd_chronic_sigma
        ),
        error = function(e) {
          shiny::showNotification(paste("Invalid chemical definition:", e$message), type = "error")
          cat(e$message)
          shiny::req(FALSE)
        })

      current <- db()

      i <- input$edit_idx
      if (is.null(i)) {
        current[[length(current) + 1]] <- new_chem
      } else {
        current[[i]] <- new_chem
      }

      db(current)
      shiny::removeModal(session = session)
    })


    shiny::observe({
      i <- input$delete_idx
      current <- db()
      if (!is.null(i) && i <= length(data)) {
        current[[i]] <- NULL
        db(current)
      }
    }) |>
      shiny::bindEvent(input$delete_trigger, ignoreNULL = TRUE)

    return(db)
  })
}

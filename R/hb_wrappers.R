#' Albufera Hydrological Balance
#'
#' Documentation TBD
#'
#' @details
#' The numeric inputs used from the linear storage curve and P-ETP surface were
#' extracted from the CHJ report
#' [Modelo de seguimiento de l’Albufera de Valencia con AQUATOOLDMA.](https://www.chj.es/Descargas/ProyectosOPH/Consulta%20publica/PHC-2015-2021/ReferenciasBibliograficas/HumedalesZonasProtegidas/CHJ,2012.Aquatool_Albufera.pdf)
#'
#'
#' @export
albufera_hb_global <- function(
    outflows_df = albufera_outflows,
    weather_df = albufera_weather,
    storage_curve = linear_storage_curve(intercept = 16.7459 * 1e6,
                                         slope = 23.6577 * 1e6),
    petp_surface = linear_petp_surface(surface_P = 114.225826072 * 1e6,
                                       surface_ETP = 79.360993685 * 1e6)
)
{
  # Just to get intersection of dates
  input <- merge(outflows_df, weather_df, by = "date", sort = TRUE)

  hb_global(
    level = input$level,
    P = input$P,
    ETP = input$ETP,
    outflows = data.frame(pujol = input$pujol,
                          perellonet = input$perellonet,
                          perello = input$perello),
    date = input$date,
    level_is_imputed = input$level_is_imputed,
    outflow_is_imputed = input$outflow_is_imputed,
    storage_curve = storage_curve,
    petp_surface = petp_surface
    )
}


#' Albufera Hydrological Balance
#'
#' Documentation TBD
#'
#' @details
#' The numeric inputs used from the linear storage curve and P-ETP surface were
#' extracted from the CHJ report
#' [Modelo de seguimiento de l’Albufera de Valencia con AQUATOOLDMA.](https://www.chj.es/Descargas/ProyectosOPH/Consulta%20publica/PHC-2015-2021/ReferenciasBibliograficas/HumedalesZonasProtegidas/CHJ,2012.Aquatool_Albufera.pdf)
#'
#'
#' @export
albufera_hydro_balance_local <- function(
    outflows_df = albufera_outflows,
    weather_df = albufera_weather,
    management_df = albufera_management,
    clusters_df = albufera_clusters,
    storage_curve = linear_storage_curve(intercept = 16.7459 * 1e6,
                                         slope = 23.6577 * 1e6),
    petp_surface = linear_petp_surface(surface_P = 114.225826072 * 1e6,
                                       surface_ETP = 79.360993685 * 1e6),
    date_min = NULL,
    date_max = NULL
    )
{
  input <- hb_local_data_prep(outflows_df = outflows_df,
                              weather_df = weather_df,
                              management_df = management_df,
                              clusters_df = clusters_df,
                              storage_curve = storage_curve,
                              petp_surface = petp_surface,
                              date_min = date_min,
                              date_max = date_max
                              )

  res <- hb_local(  # TODO: Use do.call()?
    date = input$date,
    ditch = input$ditch,
    cluster_id = input$cluster_id,
    ideal_height_cm = input$height_cm,
    petp_cm = input$petp_cm,
    irrigation = input$irrigation,
    draining = input$draining,
    area_m2 = input$area,
    total_inflow_lake = input$inflow_total,
    ideal_flow_rate_cm = 5,
    tancat = input$tancat,
    variety = input$variety
    )

  return(res)
}

hb_local_data_prep <- function(
    outflows_df,
    weather_df,
    management_df,
    clusters_df,
    storage_curve,
    petp_surface,
    date_min,
    date_max
    )
{
  res <- albufera_hb_global(outflows_df = outflows_df,
                            weather_df = weather_df,
                            storage_curve = storage_curve,
                            petp_surface = petp_surface
                            )

  res <- data.table::as.data.table(res)

  if(!is.null(date_min)) res <- res[res$date >= date_min, ]
  if(!is.null(date_max)) res <- res[res$date <= date_max, ]

  date_posixlt <- as.POSIXlt(res$date)  # Required by get_* helpers
  res$mm <- get_mm(date_posixlt)
  res$dd <- get_dd(date_posixlt)

  res <- res |>
    merge(y = data.table::as.data.table(management_df),
          by = c("mm", "dd"),
          sort = FALSE,
          allow.cartesian = TRUE
          ) |>
    merge(y = data.table::as.data.table(clusters_df),
          by.x = c("tancat", "variety"),
          by.y = c("tancat", "rice_variety"),
          all.y = TRUE,
          sort = FALSE,
          allow.cartesian = TRUE
          )

  res$petp_cm <- (res$P - res$ETP) / 10

  res <- data.table::setorder(res, date, cluster_id)

  return(res)
}


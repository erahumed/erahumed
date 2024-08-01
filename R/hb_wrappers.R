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
albufera_hb_local <- function(
    outflows_df = albufera_outflows,
    weather_df = albufera_weather,
    management_df = albufera_management,
    clusters_df = albufera_clusters,
    storage_curve = linear_storage_curve(intercept = 16.7459 * 1e6,
                                         slope = 23.6577 * 1e6),
    petp_surface = linear_petp_surface(surface_P = 114.225826072 * 1e6,
                                       surface_ETP = 79.360993685 * 1e6),
    date_min = NULL,
    date_max = NULL,
    ideal_flow_rate_cm = 5
    )
{
  hbl_args <- albufera_hb_local_data_prep(
    outflows_df = outflows_df,
    weather_df = weather_df,
    management_df = management_df,
    clusters_df = clusters_df,
    storage_curve = storage_curve,
    petp_surface = petp_surface,
    date_min = date_min,
    date_max = date_max
    )
  hbl_args <- c(hbl_args, list(ideal_flow_rate_cm = ideal_flow_rate_cm))

  do.call(hb_local, hbl_args)
}

albufera_hb_local_data_prep <- function(
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
                            ) |>
    data.table::as.data.table()

  if(!is.null(date_min)) res <- res[res$date >= date_min, ]
  if(!is.null(date_max)) res <- res[res$date <= date_max, ]

  res$petp_cm <- (res$P - res$ETP) / 10
  res$mm <- get_mm(as.POSIXlt(res$date))
  res$dd <- get_dd(as.POSIXlt(res$date))

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

  res <- data.table::setorder(res, date, cluster_id)

  res <- list(date = res$date,
              ditch = res$ditch,
              cluster_id = res$cluster_id,
              ideal_height_cm = res$height_cm,
              petp_cm = res$petp_cm,
              irrigation = res$irrigation,
              draining = res$draining,
              area_m2 = res$area,
              total_inflow_lake = res$inflow_total,
              tancat = res$tancat,
              variety = res$variety
              )

  return(res)
}


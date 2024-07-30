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
albufera_hydro_balance_global <- function(
    outflows_df = albufera_outflows,
    weather_df = albufera_weather,
    storage_curve = linear_storage_curve(intercept = 16.7459 * 1e6,
                                         slope = 23.6577 * 1e6),
    petp_surface = linear_petp_surface(surface_P = 114.225826072 * 1e6,
                                       surface_ETP = 79.360993685 * 1e6)
)
{
  # Just to select the intersection of dates
  input <- merge(outflows_df, weather_df, by = "date", sort = TRUE)

  # Bind with global hydrological balance variables
  res <- hydro_balance_global(
    level = input$level,
    P = input$P,
    ETP = input$ETP,
    outflows = list(pujol = input$pujol,
                    perellonet = input$perellonet,
                    perello = input$perello),
    storage_curve = storage_curve,
    petp_surface = petp_surface
    )

  input <- input[ , !(colnames(input) %in% colnames(res))]
  res <- cbind(input, res)

  res <- na.omit(res) # Necessary?

  res <- compute_hb_imputed_cols(res)

  # To substitute with a proper class constructor?
  class(res) <- c("hb_global", "data.frame")
  attr(class(res), "package") <- "erahumed"

  return(res)
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
  res <-
    albufera_hydro_balance_global(
      outflows_df = outflows_df,
      weather_df = weather_df,
      storage_curve = storage_curve,
      petp_surface = petp_surface
    ) |>
    hb_local_data_prep(
      management_df = management_df,
      clusters_df = clusters_df,
      date_min = date_min,
      date_max = date_max
      )

  for (i in seq_along(res)) {
    res[[i]] <- simulate_lhb(
      ideal_height_cm = res[[i]]$height_cm,
      petp_cm = res[[i]]$petp_cm,
      irrigation = res[[i]]$irrigation,
      draining = res[[i]]$draining,
      area_m2 = res[[i]]$area,
      capacity_m3_s = res[[i]]$flowpoint,
      date = res[[i]]$date,
      ideal_flow_rate_cm = 5,
      ditch = res[[i]]$ditch
    )
  }

  res <- do.call(c, res) # flatten to single list of data-frames
  res <- data.table::rbindlist(res)
  res <- as.data.frame(res)

  # To substitute with a proper class constructor?
  class(res) <- c("hb_local", "data.frame")
  attr(class(res), "package") <- "erahumed"

  return(res)
}

hb_local_data_prep <- function(
    hb_global, management_df, clusters_df, date_min, date_max
    )
{
  res <- hb_global

  res <- data.table::as.data.table(res)
  management_df <- data.table::as.data.table(management_df)
  clusters_df <- data.table::as.data.table(clusters_df)

  if(!is.null(date_min)) {
    res <- res[res$date >= date_min, ]
  }
  if(!is.null(date_max)) {
    res <- res[res$date <= date_max, ]
  }

  ### Start HB for cluster part. Should break down into components.
  res$mm <- as.numeric(format(res$date, "%m"))
  res$dd <- as.numeric(format(res$date, "%d"))

  res <- res |>
    merge(management_df,
          by = c("mm", "dd"),
          sort = FALSE,
          allow.cartesian = TRUE
          ) |>
    merge(clusters_df,
          by.x = c("tancat", "variety"),
          by.y = c("tancat", "rice_variety"),
          all.y = TRUE,
          sort = FALSE,
          allow.cartesian = TRUE
          )

  res$petp_cm <- (res$P - res$ETP) / 10
  res$plan_delay <- 0

  ditch_inflow_pct <- compute_ditch_inflow_pct(clusters_df)
  res$flowpoint <- res$inflow_total *
    ditch_inflow_pct$inflow_pct[match(res$ditch, ditch_inflow_pct$ditch)]

  res <- data.table::setorder(res, date, cluster_id)

  res <- res |>
    collapse::rsplit(
      by = ~ ditch,
      flatten = FALSE,
      use.names = FALSE,
      simplify = FALSE,
      keep.by = TRUE
    )

  return(res)

}

compute_hb_imputed_cols <- function(df) {
  # Simplify this, just have a column that signals whether a given row has some imputed data

  df$volume_is_imputed <- df$level_is_imputed
  df$data_is_imputed <-
    df$level_is_imputed |
    df$pujol_is_imputed |
    df$perellonet_is_imputed |
    df$perello_is_imputed
  df$volume_change_is_imputed <- df$volume_is_imputed
  df$petp_change_is_imputed <- FALSE
  df$outflow_total_is_imputed <-
    df$pujol_is_imputed |
    df$perellonet_is_imputed |
    df$perello_is_imputed
  df$inflow_total_is_imputed <- df$data_is_imputed
  df$outflow_extra_is_imputed <- df$inflow_total_is_imputed
  df$residence_time_days_is_imputed <- df$data_is_imputed
  return(df)
}

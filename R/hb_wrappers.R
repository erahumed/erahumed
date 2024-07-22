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
  hb_global <- albufera_hydro_balance_global(
    outflows_df = outflows_df,
    weather_df = weather_df,
    storage_curve = storage_curve,
    petp_surface = petp_surface
    )

  res <- hb_local_data_prep(hb_global = hb_global,
                            management_df = management_df,
                            clusters_df = clusters_df,
                            date_min = date_min,
                            date_max = date_max)

  n_ditches <- length(unique(res$ditch))
  n_dates <- length(unique(res$date))

  res <- as.data.frame(res)

  res <- res |>
    collapse::rsplit(
      by = ~ ditch + date,
      flatten = FALSE,
      use.names = FALSE,
      simplify = FALSE,
      keep.by = TRUE
    )
  for (i in 1:n_ditches) {
    for (j in 1:n_dates) {
      res[[i]][[j]] <- compute_hb_daily(
        current = unclass(res[[i]][[j]]),
        previous = if (j == 1) res[[i]][[j]] else unclass(res[[i]][[j - 1]])
        )
      class(res[[i]][[j]]) <- "data.frame"
    }
    # The class()-unclass() trick above can lower the execution time of
    # propagate ditch by a constant factor (roughly of order 2). This has a
    # price to pay in that the code becomes more prone to bugs (e.g. we don't
    # have data-frame column automatic length checks)
  }

  res <- do.call(c, res) # flatten to single list
  res <- data.table::rbindlist(res)


  as.data.frame(res)
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

  res <- merge(res,
               management_df,
               by = c("mm", "dd"),
               sort = FALSE,
               allow.cartesian = TRUE
               )
  res <- res |>
    split(by = c("tancat", "variety")) |>
    lapply(\(df){
      df$lag_height_cm <- c(
        ifelse(df$tancat[1], 20, 0), df$height_cm[1:(nrow(df)-1)]
      )
      df
    })

  res <- data.table::rbindlist(res)

  res <- res |>
    merge(clusters_df,
          by.x = c("tancat", "variety"),
          by.y = c("tancat", "rice_variety"),
          all.y = TRUE,
          sort = FALSE,
          allow.cartesian = TRUE
    )
  res$height_diff_cm <- res$height_cm - res$lag_height_cm
  res$petp <- res$P - res$ETP
  res$petp_cm <- res$petp / 10
  res$petp_m <- res$petp / 1e3
  res$petp_m_s <- res$petp_m / s_per_day()
  res$petp_m3_s <- res$petp_m_s * res$area
  res$inflow <- res$irrigation *
    (res$draining * 5 +
       (1 - res$draining) * (res$height_diff_cm - pmin(res$petp_cm, 0))
    )

  # TODO: can we avoid all these preallocations? They are totally useless from
  # the efficiency POV
  res$outflow <- pmax(res$inflow + res$petp_cm - res$height_diff_cm, 0)
  res$outflow_m3 <- (res$outflow / 100) * res$area / s_per_day()

  res$accum_drain <- 0
  res$accum_rain <- 0
  res$accum_flux <- 0
  res$Evap_mismatch <- 0
  res$real_outflow_rain <- 0
  res$real_outflow_drain <- 0
  res$real_outflow_flux <- 0
  # res$corrected <- FALSE
  # res$.lag_accum_rain <- 0
  # res$lag_accum_drain <- 0
  # res$lag_accum_rain <- 0
  # res$lag_accum_rain_cm <- 0
  # res$Evap_mismatch <- 0
  #res$condition <- FALSE

  ditch_inflow_pct <- compute_ditch_inflow_pct(clusters_df)
  res$flowpoint <- res$inflow_total *
    ditch_inflow_pct$inflow_pct[match(res$ditch, ditch_inflow_pct$ditch)]

  res <- data.table::setorder(res, date, cluster_id)
  res
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

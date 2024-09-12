#' Albufera Local Hydrological Balance
#'
#' @description
#' Wrapper around \link{hb_local}, used to run the local hydrological balance
#' simulation algorithm with the data for the Albufera lake packed up in the
#' `data.frame`s exported by `erahumed`.
#'
#' @inheritParams hb_local
#' @inheritParams hb_global
#' @param outflows_df,petp_df,management_df,clusters_df `data.frame`s, whose
#' structures follow the templates of
#' \link{albufera_outflows}, \link{albufera_petp}, \link{albufera_management}
#' and \link{albufera_clusters}, respectively.
#' @param date_min,date_max date range for the simulation.
#'
#' @return Same as \link{hb_local}.
#'
#' @details
#' The numeric inputs used from the linear storage curve and P-ETP surface were
#' extracted from the CHJ report
#' [Modelo de seguimiento de l’Albufera de Valencia con AQUATOOLDMA.](https://www.chj.es/Descargas/ProyectosOPH/Consulta%20publica/PHC-2015-2021/ReferenciasBibliograficas/HumedalesZonasProtegidas/CHJ,2012.Aquatool_Albufera.pdf)
#'
#' @export
albufera_hb_local <- function(
    outflows_df = erahumed::albufera_outflows,
    petp_df = erahumed::albufera_petp,
    management_df = erahumed::albufera_management,
    clusters_df = erahumed::albufera_clusters,
    storage_curve = linear_storage_curve(intercept = 16.7459 * 1e6,
                                         slope = 23.6577 * 1e6),
    petp_surface = linear_petp_surface(surface_P = 114.225826072 * 1e6,
                                       surface_ETP = 79.360993685 * 1e6),
    date_min = NULL,
    date_max = NULL,
    ideal_flow_rate_cm = 5
    )
{

  res_precomputed <- albufera_hb_local_precomputed(formals = formals(),
                                                   call = match.call())
  if (!is.null(res_precomputed))
    return(res_precomputed)

  albufera_hb_local_argcheck(outflows_df,
                             petp_df,
                             management_df,
                             clusters_df,
                             date_min,
                             date_max,
                             ideal_flow_rate_cm
                             )

  hbl_args <- albufera_hb_local_data_prep(
    outflows_df = outflows_df,
    petp_df = petp_df,
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
    petp_df,
    management_df,
    clusters_df,
    storage_curve,
    petp_surface,
    date_min,
    date_max
    )
{
  res <- albufera_hb_global(outflows_df = outflows_df,
                            petp_df = petp_df,
                            storage_curve = storage_curve,
                            petp_surface = petp_surface
                            ) |>
    data.table::as.data.table()

  if(!is.null(date_min)) res <- res[res$date >= date_min, ]
  if(!is.null(date_max)) res <- res[res$date <= date_max, ]
  if (nrow(res) == 0) {
    msg <- paste(
      "No data was found in the specified date range.",
      "Please check 'date_min' and 'date_max' arguments."
      )
    stop(msg)
  }

  res$petp_cm <- (res$rain_mm - res$evapotranspiration_mm) / 10
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

  res <- data.table::setorderv(res, c("date", "cluster_id"))

  res <- list(date = res$date,
              ditch = res$ditch,
              cluster_id = res$cluster_id,
              ideal_height_cm = res$height_cm,
              petp_cm = res$petp_cm,
              seed_day = res$seed_day,
              ideal_irrigation = res$irrigation,
              ideal_draining = res$draining,
              area_m2 = res$area,
              total_inflow_lake = res$inflow_total,
              tancat = res$tancat,
              variety = res$variety
              )

  return(res)
}

albufera_hb_local_argcheck <- function(
    outflows_df,
    petp_df,
    management_df,
    clusters_df,
    date_min,
    date_max,
    ideal_flow_rate_cm
    )
{
  tryCatch(
    {
      assert_data.frame(outflows_df, template = erahumed::albufera_outflows)
      assert_data.frame(petp_df, template = erahumed::albufera_petp)
      assert_data.frame(management_df, template = erahumed::albufera_management)
      assert_data.frame(clusters_df, template = erahumed::albufera_clusters)
      assert_positive_number(ideal_flow_rate_cm)

      if (is.null(date_min)) date_min <- as.Date(-Inf)
      if (is.null(date_max)) date_max <- as.Date(Inf)
      assert_valid_date_range(c(date_min, date_max))

    },
    error = function(e) {
      class(e) <- c("albufera_hb_local_argcheck_error", class(e))
      stop(e)
    }
  )
}

albufera_hb_local_precomputed <- function(formals, call) {
  file_path <- system.file("parquet",
                           "albufera_hb_local.parquet",
                           package = "erahumed")
  if (!file.exists(file_path))
    return(NULL)

  if (! as.logical(Sys.getenv("erahumed_use_precomputed", "TRUE")) )
    return(NULL)

  call <- as.list(call)[-1]
  for (arg in names(formals)) {
    if (!arg %in% names(call))
      next
    provided_hash <- digest::digest(call[[arg]])
    default_hash <- digest::digest(formals[[arg]])
    if (provided_hash != default_hash)
      return(NULL)
  }

  return(arrow::read_parquet(file_path))
}

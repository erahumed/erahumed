# Data from Soria et al., currently not used. The corresponding description
# entry is provided below.

months_es <- c(
  "Enero", "Febrero", "Marzo",
  "Abril", "Mayo", "Junio",
  "Julio", "Agosto", "Septiembre",
  "Octubre", "Noviembre", "Diciembre"
)

ditch_inflow_pcts <- readRDS("data-raw/raw/pct_soria_obs.rds") |>
  rename(month = mes) |>
  mutate(month = match(month, months_es))

names(ditch_inflow_pcts) <- gsub("acq", "d", names(ditch_inflow_pcts))

usethis::use_data(ditch_inflow_pcts, overwrite = TRUE)



#' Ditch Inflow Percents
#'
#' Percents of total inflows to the Albufera Lake from individual ditches, by
#' month. Based on observational data.
#'
#' @format ## `ditch_inflow_pcts`
#' A dataframe with one row per month, and columns:
#' \describe{
#'   \item{month}{Integer. Month number (1 = January, 2 = February, *etc.*).}
#'   \item{dXX}{Double. Percent of total inflow contributed by ditch number XX.}
#' }
#' @source <https://www.mdpi.com/2306-5338/8/1/37>
"ditch_inflow_pcts"

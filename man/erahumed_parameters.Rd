% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/docs--setup-parameters.R, R/meta--exposure.R,
%   R/meta--hydrology.R, R/meta--risk.R
\name{erahumed_parameters}
\alias{erahumed_parameters}
\alias{setup_functions}
\alias{setup_exposure}
\alias{setup_hydrology}
\alias{setup_risk}
\title{Setup parameters of ERAHUMED simulations}
\usage{
setup_exposure(
  simulation,
  ca_schedules_df = erahumed::albufera_ca_schedules,
  drift = 0,
  covmax = 0.5,
  jgrow = 152,
  SNK = 0,
  dact_m = 0.1,
  css_ppm = 50,
  foc = 0.17,
  bd_g_cm3 = 1.5,
  qseep_m_day = 0,
  wilting = 0.24,
  fc = 0.35
)

setup_hydrology(
  simulation,
  outflows_df = erahumed::albufera_outflows,
  weather_df = erahumed::albufera_weather,
  variety_prop = c(J.Sendra = 0.8, Bomba = 0.1, Clearfield = 0.1),
  storage_curve = function(level) 25.58 * 1e+06 + level * 63.086 * 1e+06,
  petp_function = function(p, etp) 53.9 * 1000 * (p - etp),
  management_df = erahumed::albufera_management,
  ideal_flow_rate_cm = 5,
  height_thresh_cm = 0.5,
  ditch_level_m = 1,
  seed = 840
)

setup_risk(simulation)
}
\arguments{
\item{simulation}{\code{[}\link{erahumed_simulation}\verb{]} \cr
The simulation object being modified.}

\item{ca_schedules_df}{\verb{[data.frame]} \cr A dataset that provides the list of scheduled chemical applications, along the template of \link{albufera_ca_schedules}.}

\item{drift}{\verb{[numeric(1)]} \cr A number between \code{0} and \code{1}. Percentage of chemical applications lost to drift.}

\item{covmax}{\verb{[numeric(1)]} \cr A number between \code{0} and \code{1}. Interception potential of foliage at crop maturation.}

\item{jgrow}{\verb{[numeric(1)]} \cr A positive integer. Length (in days) of crop maturation cycle.}

\item{SNK}{\verb{[numeric(1)]} \cr A number between \code{0} and \code{1}. (TODO).}

\item{dact_m}{\verb{[numeric(1)]} \cr A positive number. Active sediment layer depth, expressed in meters.}

\item{css_ppm}{\verb{[numeric(1)]} \cr A positive number. Suspended sediment concentration, expressed in parts per million.}

\item{foc}{\verb{[numeric(1)]} \cr A number between \code{0} and \code{1}. Fraction of organic content.}

\item{bd_g_cm3}{\verb{[numeric(1)]} \cr A positive number. Bulk density of the sediment, expressed in grams per cubic centimeter.}

\item{qseep_m_day}{\verb{[numeric(1)]} \cr A positive number. Seepage rate, expressed in meters per day.}

\item{wilting}{\verb{[numeric(1)]} \cr A number between \code{0} and \code{1}. Wilting point.}

\item{fc}{\verb{[numeric(1)]} \cr A number between \code{0} and \code{1}. Field capacity.}

\item{outflows_df}{\verb{[data.frame]} \cr Time-series dataset that provides the observational hydrological data on the Albufera lake, along the template of \link{albufera_outflows} (the default value).}

\item{weather_df}{\verb{[data.frame]} \cr A dataset that provides the relevant metereological time series, along the template of \link{albufera_weather} (the default value).}

\item{variety_prop}{\verb{[numeric(3)]} \cr Vector of \code{3} positive values. Controls the proportions of field surface allocated to the cultivation of the J. Sendra, Bomba, and Clearfield varieties, respectively (positive vectors that do not sum to one are allowed, with entries being understood as relative proportions).}

\item{storage_curve}{\verb{[function]} \cr A function that converts lake levels into lake \emph{volumes}. The function should take a single numeric vector as input, and should return a numeric vector of the same length as output. The numeric values for the default (linear) storage curve are taken from the CHJ report \href{https://www.chj.es/Descargas/ProyectosOPH/Consulta\%20publica/PHC-2015-2021/ReferenciasBibliograficas/HumedalesZonasProtegidas/CHJ,2012.Aquatool_Albufera.pdf}{\emph{Modelo de seguimiento de l’Albufera de Valencia con AQUATOOLDMA}}.}

\item{petp_function}{\verb{[function]} \cr A function that converts values of precipitation and evapotranspiration per unit area (assumed to be expressed in millimiters) into volume changes for the Albufera lake (in cube meters when the inputs are given in the expected units). This function should take two numeric vectors of common length as inputs (representing the precipitation and evapotranspiration time series, respectively), and should return a numeric vector of the same length. The numeric values used in the default (linear) function were computed by the package authors, and correspond to the total study area (consisting of the lake and irrigation ditches) and the flooded fraction, that are relevant for precipitation and evapotranspiration volume changes, respectively.}

\item{management_df}{\verb{[data.frame]} \cr Dataset that provides the yearly schedule for irrigation and draining, along the template of \link{albufera_management} (the default value).}

\item{ideal_flow_rate_cm}{\verb{[numeric(1)]} \cr Ideal inflow/outflow of a cluster, for days in which the cluster is scheduled to be in flux (\emph{i.e.} when being simultaneously irrigated and drained). Expressed in centimeters per day.}

\item{height_thresh_cm}{\verb{[numeric(1)]} \cr A positive number. Height threshold for water levels, below which a cluster is considered to be emptied.}

\item{ditch_level_m}{\verb{[numeric(1)]} \cr Constant water depth in ditches.}

\item{seed}{\verb{[numeric(1)]} \cr Seed for random number generation used by the simulation algorithms.}
}
\value{
A \link{erahumed_simulation}.
}
\description{
These functions are used to set up the input parameters of
ERAHUMED simulations.
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hba--main.R
\name{hba}
\alias{hba}
\alias{setup_hba}
\title{HBA: Hydrological Balance of the Albufera Lake}
\usage{
setup_hba(
  simulation,
  storage_curve = function(level) 16.7459 * 1e+06 + level * 23.6577 * 1e+06,
  petp_function = function(p, etp) 114.226 * 1000 * p - 79.361 * 1000 * etp
)
}
\arguments{
\item{simulation}{\code{[}\link{erahumed_simulation}\verb{]} \cr
The simulation object being modified.}

\item{storage_curve}{\verb{[function]} \cr A function that converts lake levels into lake \emph{volumes}. The function should take a single numeric vector as input, and should return a numeric vector of the same length as output. The numeric values for the default (linear) storage curve are taken from the CHJ report \href{https://www.chj.es/Descargas/ProyectosOPH/Consulta\%20publica/PHC-2015-2021/ReferenciasBibliograficas/HumedalesZonasProtegidas/CHJ,2012.Aquatool_Albufera.pdf}{\emph{Modelo de seguimiento de l’Albufera de Valencia con AQUATOOLDMA}}.}

\item{petp_function}{\verb{[function]} \cr A function that converts values of precipitation and evapotranspiration per unit area (assumed to be expressed in millimiters) into volume changes for the Albufera lake (in cube meters when the inputs are given in the expected units). This function should take two numeric vectors of common length as inputs (representing the precipitation and evapotranspiration time series, respectively), and should return a numeric vector of the same length. The numeric values used in the default (linear) function were computed by the package authors, and correspond to the total study area (consisting of the lake and irrigation ditches) and the flooded fraction, that are relevant for precipitation and evapotranspiration volume changes, respectively.}
}
\value{
An objects of class \link{erahumed_simulation}.
}
\description{
This simulation layer completes the partial hydrological balance information
provided as input data, by computing the total inflow to the Albufera lake,
as well as, if relevant, the amount of unaccounted outflow - usually
attributed to \emph{tancats} that suck water from the lake.
}
\details{
The output data.frame of the calculated HBA layer contains a copy of the
input data, as well as the following additional columns:
\itemize{
\item \code{volume} Volume time series, obtained from the storage curve.
\item \code{volume_change} Differenced time series of volume. The \eqn{n}-th is given
by \eqn{\Delta V _n \equiv V_{n+1}-V_n}, where \eqn{V_n} is volume at time
step \eqn{n}.
\item \code{petp_change} Time series. The amount \eqn{\Delta V _n ^\text{P-ETP}} of
volume change due to precipitation and evapotranspiration, obtained from the
P-ETP surface.
\item \code{outflow_total} Time series of total outflows, measured in cube meters per
second. This is the sum
\eqn{\sum _i O_i} of time series passed through the \code{outflows} argument,
plus an extra term \eqn{O_R} that estimates the outflow due to water
recirculation (see TODO #60).
\item \code{outflow_recirculation} Time series. Estimate of outflow due to water
recirculation.
\item \code{inflow_total} Time serie of total inflows, in cube meters per second.
This is computed as \eqn{I = \sum _{i} O_i + \delta O + \frac{\Delta V _n - \Delta V _n ^\text{P-ETP}}{24 \times 60 \times 60}}.
\item \code{residence_time_days}. Residence time, as simulationed by \link{hba_residence_time}.
}
}
\seealso{
Other simulation layers: 
\code{\link{ca}},
\code{\link{ct}},
\code{\link{hbp}},
\code{\link{inp}}
}
\concept{simulation layers}

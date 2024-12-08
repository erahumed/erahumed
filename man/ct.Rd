% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ct--main.R
\name{ct}
\alias{ct}
\alias{setup_ct}
\title{CT: Chemical Transport}
\usage{
setup_ct(
  simulation,
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
}
\arguments{
\item{simulation}{An object of class \link{erahumed_simulation}.}

\item{drift}{A number between \code{0} and \code{1}. Percentage of chemical
applications lost to drift.}

\item{covmax}{A number between \code{0} and \code{1}. Interception potential of
foliage at crop maturation.}

\item{jgrow}{A positive integer. Length (in days) of crop maturation cycle.}

\item{SNK}{A number between \code{0} and \code{1}. (TODO).}

\item{dact_m}{A positive number. Active sediment layer depth, expressed in
meters.}

\item{css_ppm}{A positive number. Suspended sediment concentration, expressed
in parts per million.}

\item{foc}{A number between \code{0} and \code{1}. Fraction of organic content.}

\item{bd_g_cm3}{A positive number. Bulk density of the sediment, expressed in
grams per cubic centimeter.}

\item{qseep_m_day}{A positive number. Seepage rate, expressed in meters per
day.}

\item{wilting}{A number between \code{0} and \code{1}. Wilting point.}

\item{fc}{A number between \code{0} and \code{1}. Field capacity.}
}
\value{
An objects of class \link{erahumed_simulation}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
This simulation layer computes the evolution of chemicals applied to rice
paddy clusters, based on the previously computed simulations for
hydrological balance and chemicals application.
The result is a set of time series of chemical masses, one for each
applied chemical and for the three compartments: foliage, water and sediment.
}
\seealso{
Other simulation layers: 
\code{\link{ca}},
\code{\link{hba}},
\code{\link{hbp}},
\code{\link{inp}}
}
\concept{simulation layers}

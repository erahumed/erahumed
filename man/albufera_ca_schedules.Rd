% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/docs--data.R
\docType{data}
\name{albufera_ca_schedules}
\alias{albufera_ca_schedules}
\title{Albufera Chemical Application Schedules}
\format{
\subsection{\code{albufera_ca_schedules}}{

The cardinality of data is given by: one row per scheduled application.The dataset features the following columns:\describe{\item{day}{numeric. Scheduled day, counted starting from the sowing day, for the application under consideration.}\item{rice_variety}{character. Rice variety for this specific application.}\item{chemical}{character. Name of applied chemical.}\item{kg_per_ha}{numeric. Amount of chemical applied, in kilograms per hectare.}\item{application_type}{either \code{"ground"} or \code{"aerial"}. Application mode of the chemical to rice paddies.}}
}
}
\source{
Martínez-Megías, C., Mentzel, S., Fuentes-Edfuf, Y., Moe, S. J., &
Rico, A. (2023). Influence of climate change and pesticide use practices on
the ecological risks of pesticides in a protected Mediterranean wetland: A
Bayesian network approach. \emph{Science of The Total Environment, 878},
163018. \url{https://doi.org/10.1016/j.scitotenv.2023.163018}
}
\usage{
albufera_ca_schedules
}
\description{
Data on application schedules of chemicals used in rice paddies in the
Albufera National Park. For further details on how this information was obtained, see
the original source
\href{https://doi.org/10.1016/j.scitotenv.2023.163018}{Martínez-Megías et al. (2023)}.
}
\keyword{datasets}

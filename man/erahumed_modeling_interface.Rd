% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meta--docs-modeling-interface.R
\name{erahumed_modeling_interface}
\alias{erahumed_modeling_interface}
\alias{erahumed_model_component}
\title{ERAHUMED modeling interface}
\description{
The programming interface of the \code{{erahumed}} package reflects the sequential
structure of the ERAHUMED modeling chain. Each step of the sequence is
referred across this documentation as a model "component" (or, sometimes,
informally as a modeling "layer"). This documentation page focuses on
describing the set of abstractions provided by \code{{erahumed}} to deal with
sequential modeling and model components.

The first abstraction is provided by \link{erahumed_model} objects. These
objects, technically implemented as S3 classes, are simple containers for
model components - that is, a \code{erahumed_model} is a list that stores a
sequence of model components. The way components of a model are populated is
described subsequently.

We define the following model components, whose technical implementation is
discussed extensively in the linked documentation pages:
\itemize{
\item \link{inp}: INPut data.
\item \link{hba}: Hydrological Balance of the Albufera lake.
\item \link{hbp}: Hydrological Balance of rice Paddy clusters.
\item \link{ca}: Chemical Applications.
The order of model components in the list above is the logical one. Each
component depends on the previous ones (referred to as "upstream"),
and is a dependency of the subsequent ones (referred to as "downstream").
}

Each of these components has associated:
\enumerate{
\item A \verb{compute_*()} function, that takes a \link{erahumed_model} as input and
returns a model with the desired component computed on top as output.
\item An extractor function of the form \verb{*()} (the asterisk standing for the
actual component name), that extracts the desired component from a
\link{erahumed_model}.
\item An S3 class following the naming scheme \verb{erahumed_*}, that inherits from
\code{erahumed_model_component}, with ad-hoc \link{print} and \link{plot} methods.
Components can be re-computed (that is, the input model to \verb{compute_*()}) can
have a previous computation of the same components. In this case, in order to
avoid confusing results, any existing downstream dependency of the component
to be recomputed is erased from the output model, and must be recomputed if
required.
}

The example code below illustrates typical operations with model components,
}
\examples{
### Computation workflow
m <- erahumed_model()  # Initialize a blank model

m <- m |> compute_inp() |> compute_hba()  # Compute INP and HBA components

m
inp(m)
hba(m)
hbp(m)  # NULL because not yet computed

m <- m |> compute_inp()  # Recompute INP layer
hba(m)  # NULL because of previous line

# The code below results in an error, because the HBP component depends
# on the HBA component, which is still NULL (see above).
\dontrun{
compute_hbp(m)
}

### Extracting and plotting outputs
m <- m |> compute_inp() |> compute_hba()

component_output(inp(m)) |> head()  # 'erahumed_model_component' method
component_output(m, "hba") |> head()  # 'erahumed_model' method

\dontrun{
# Returns an interactive plotly plot, run manually.
plot(hba(m), variable = "outflow_total")
}

}
\author{
Valerio Gherardi
}

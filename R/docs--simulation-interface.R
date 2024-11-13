#' @title ERAHUMED simulation interface
#' @name erahumed_simulation_interface
#' @aliases erahumed_simulation_layer
#'
#' @examples
#' # Simulation workflow
#'
#' ### Initialize a new simulation
#' s <- erahumed_simulation()
#'
#' ### Fine-tune layers
#' s <- s |>
#'   setup_hbp(ideal_flow_rate_cm = 2.5) |>
#'   setup_ct(dact_m = 0.1)
#'
#' ### Run simulation until a given layer
#' s <- run_simulation(s, layer = "hba")
#'
#' ### Extract layers
#' get_layer(s, "inp")
#' get_layer_output(s, "hba") |> head()
#' get_layer_output(s, "hbp")  # NULL because not yet computed
#' get_layer_parameters(s, "ca")  # Layer parameters set during initialization
#'
#' ### Reconfigure a layer
#' s <- s |> setup_hba(storage_curve = \(level) level + 1)
#' get_layer_output(s, "hba")  # NULL because invalidated by previous line...
#' s <- run_simulation(s, layer = "hba")  # ... which requires to re-run layer
#' get_layer_output(s, "hba") |> head()
#'
#' \dontrun{
#' # Returns an interactive plotly plot, run manually.
#' s |> get_layer("hba") |> plot(variable = "outflow_total")
#' }
#'
#' @description
#' The programming interface of the `{erahumed}` package reflects the sequential
#' structure of the ERAHUMED simulation chain. Each step of the sequence is
#' referred across this documentation as a simulation "layer".
#' This documentation page focuses on describing the set of abstractions
#' provided by `{erahumed}` to deal with simulation layers. For further
#' information, you can consult [the main package vignette](https://erahumed.github.io/erahumed/articles/erahumed-workflow.html).
#'
#' The first abstraction is provided by \link{erahumed_simulation} objects.
#' These objects, technically implemented as S3 classes, are simple containers
#' for simulation layers - that is, a `erahumed_simulation` is a list that
#' stores simulation layers. In turn, layers are implemented as S3 classes
#' following the naming scheme `erahumed_*`, that inherit from
#' `erahumed_simulation_layer`, with ad-hoc \link{print} and \link{plot}
#' methods.
#'
#' Creating a new `erahumed_simulation` object will setup, but not run, a
#' fresh new simulation with default settings for all the above layers. Each
#' layer has associated a `setup_*()` function that allows to modify
#' layer-specific configurations. In order to run (*i.e* compute the actual
#' results) of a simulation, one would use \link{run_simulation}.
#'
#' We define the following layers, whose technical implementation is discussed
#' extensively in the linked documentation pages:
#' * \link{inp}: INPut data.
#' * \link{hba}: Hydrological Balance of the Albufera lake.
#' * \link{hbp}: Hydrological Balance of rice Paddy clusters.
#' * \link{ca}: Chemical Applications.
#' * \link{ct}: Chemical Transport.
#'
#' The order of simulation layers in the list above is the logical one. Each
#' layer depends on the previous ones (referred to as "upstream"),
#' and is a dependency of the subsequent ones (referred to as "downstream").
#' The list of ERAHUMED layers can also be obtained through the
#' \link{erahumed_layers} helper.
#'
#' The example code below illustrates typical operations with simulation layers.
#'
NULL

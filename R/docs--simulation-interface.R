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
#'   setup_hbc(ideal_flow_rate_cm = 2.5) |>
#'   setup_ctc(dact_m = 0.1)
#'
#' ### Run simulation until a given layer
#' s <- run_simulation(s, layer = "hbl")
#'
#' ### Extract layers
#' get_layer(s, "inp")
#' get_layer_output(s, "hbl") |> head()
#' get_layer_output(s, "hbc")  # NULL because not yet computed
#' get_layer_parameters(s, "ca")  # Layer parameters set during initialization
#'
#' ### Reconfigure a layer
#' s <- s |> setup_hbl(storage_curve = \(level) level + 1)
#' get_layer_output(s, "hbl")  # NULL because invalidated by previous line...
#' s <- run_simulation(s, layer = "hbl")  # ... which requires to re-run layer
#' get_layer_output(s, "hbl") |> head()
#'
#' \dontrun{
#' # Returns an interactive dygraphs plot, run manually.
#' s |> get_layer("hbl") |> plot(variable = "outflow_total")
#' }
#'
#' @description
#' The programming interface of the `{erahumed}` package reflects the sequential
#' structure of the ERAHUMED simulation chain. Each step of the sequence is
#' referred across this documentation as a simulation "layer".
#' This documentation page focuses on describing the set of abstractions
#' provided by `{erahumed}` to deal with simulation layers. For further
#' information, you can consult
#' [the main package vignette](https://erahumed.github.io/erahumed/articles/erahumed-workflow.html).
#' A schematic view of the various layers of the simulation pipeline, described
#' below, is also available
#' [on the package website](https://erahumed.github.io/erahumed/articles/pipeline-scheme.html).
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
#' We define the following layers (see also
#' [here](https://erahumed.github.io/erahumed/articles/pipeline-scheme.html)
#' for a schematic view), whose technical implementation is discussed
#' extensively in the linked documentation pages:
#' * \link{inp}: INPut data.
#' * \link{hbl}: Hydrological Balance of the Albufera lake.
#' * \link{hbc}: Hydrological Balance of rice Paddy clusters.
#' * \link{ca}: Chemical Applications.
#' * \link{ctc}: Chemical Transport.
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

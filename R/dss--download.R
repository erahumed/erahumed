dss_download <- function(filename, simulation, format = c("csv", "xlsx")) {
  format <- match.arg(format)

  if (!is_erahumed_simulation(simulation)) {
    stop("Simulation is not ready yet.")
  }

  components <- c("hydrology", "exposure", "risk")
  elements <- c("lake", "ditch", "cluster")

  temp_dir <- tempfile("download_tempdir_")
  results_dir <- file.path(temp_dir, "results")
  dir.create(results_dir, recursive = TRUE)

  for (component in components) {
    for (element in elements) {
      if (format == "xlsx" && element == "cluster") {
        next
      }

      df <- get_results(simulation, component = component, element = element)
      file_basename <- paste0(component, "-", element)
      out_file <- switch(format,
                         csv = file.path(results_dir, paste0(file_basename, ".csv")),
                         xlsx = file.path(results_dir, paste0(file_basename, ".xlsx"))
      )
      switch(format,
             csv = readr::write_csv(df, out_file),
             xlsx = writexl::write_xlsx(df, out_file)
      )
    }
  }

  if (format == "xlsx") {
    shiny::showNotification(
      "Note: Cluster-level data is excluded from Excel downloads due to Excel row limits.",
      type = "warning", duration = 10
    )
  }

  readme_path <- file.path(temp_dir, "README.txt")

  writeLines(download_readme_lines(), readme_path)

  zip::zip(zipfile = filename, files = c("results", "README.txt"), root = temp_dir)

  unlink(temp_dir, recursive = TRUE)
}


download_readme_lines <- function() {
  c(
    "ERAHUMED DSS – Simulation Output Archive",
    "========================================",
    "",
    paste0("This archive was generated with the R package {erahumed}, version ", utils::packageVersion("erahumed")),
    "(https://erahumed.github.io/erahumed/). It contains tabular outputs from a simulation",
    "of the ecological and hydrological dynamics of the Albufera Natural Park, produced",
    "by the ERAHUMED decision support system.",
    "",
    "Each file contains daily time series data generated for three key components:",
    "  - Hydrology: water inflow, outflow, storage, and related volumes.",
    "  - Exposure: pesticide concentrations in water and sediment compartments.",
    "  - Risk: ecotoxicological risk levels expressed as Potentially Affected Fraction (PAF).",
    "",
    "Outputs are disaggregated at three spatial levels:",
    "  - lake: aggregated data for the Albufera lake.",
    "  - ditch: aggregated data for each of the 26 drainage ditches.",
    "  - cluster: detailed data for each rice field cluster (up to several thousand records).",
    "",
    "Due to Excel file size limitations, cluster-level data are excluded from XLSX exports.",
    "All data are included in CSV format.",
    "",
    "For documentation, methodology, and source code, please refer to:",
    "https://erahumed.github.io/erahumed/"
  )
}



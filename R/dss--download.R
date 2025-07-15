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


  zip::zip(zipfile = filename, files = "results", root = temp_dir)

  unlink(temp_dir, recursive = TRUE)
}

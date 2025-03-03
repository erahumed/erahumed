dss_download <- function(filename, simulation) {
  components <- c("hydrology", "exposure", "risk")
  elements <- c("lake", "ditch", "cluster")

  temp_dir <- tempdir()
  dir.create(file.path(temp_dir, "results"))
  for (component in components) {
    for (element in elements) {
      csv_file_name <- paste0(component, "-", element, ".csv")
      temp_file <- file.path(temp_dir, "results", csv_file_name)
      df <- get_results(simulation, component = component, element = element)
      readr::write_csv(df, temp_file)
    }
  }

  zip::zip(zipfile = filename, "results", root = temp_dir)

  unlink(temp_dir)
}

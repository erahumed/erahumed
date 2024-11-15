renv::use(
  cellranger  = "cellranger@1.1.0",
  cli         = "cli@3.6.3",
  collapse    = "collapse@2.0.16",
  cpp11       = "cpp11@0.5.0",
  crayon      = "crayon@1.5.3",
  fansi       = "fansi@1.0.6",
  glue        = "glue@1.8.0",
  hms         = "hms@1.1.3",
  lifecycle   = "lifecycle@1.0.4",
  magrittr    = "magrittr@2.0.3",
  pillar      = "pillar@1.9.0",
  pkgconfig   = "pkgconfig@2.0.3",
  prettyunits = "prettyunits@1.2.0",
  progress    = "progress@1.2.3",
  R6          = "R6@2.5.1",
  Rcpp        = "Rcpp@1.0.13",
  readxl      = "readxl@1.4.3",
  rematch     = "rematch@2.0.0",
  renv        = "renv@1.0.11",
  rlang       = "rlang@1.1.4",
  tibble      = "tibble@3.2.1",
  utf8        = "utf8@1.2.4",
  vctrs       = "vctrs@0.6.5"
)

albufera_ct_parameters <- readxl::read_excel(
  "data-raw/raw/ct_parameters.xlsx", sheet = 1
  ) |>
  as.data.frame() |>
  collapse::rsplit(  # This rather than 'base::split()' for simpler syntax
    by =  ~ chemical,
    flatten = FALSE,
    use.names = TRUE,
    simplify = TRUE
  ) |>
  lapply(as.list)

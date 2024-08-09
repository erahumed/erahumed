# How is code organized?

The goal of this document is to provide an in-depth description of the 
`{erahumed}` code base. Whether you need to modify the 
package, or simply want to browse through its code (*e.g.* for a qualitative 
check of the correctness of the various algorithms it implements), it is 
fundamental that you understand the various elements that make up this software. 

In order to avoid confusions, let me also clarify what this document is *not*: 

* This is not a user documentation for `{erahumed}`. If you need help using 
the package or simply the DSS dashboard, you should rather consult the official 
documentation: including the package `README`, vignettes, and R documentation 
page accessible through `?` from the R console.
* This is not a scientific presentation of the various algorithms implemented 
by the package, for which you should rather consult ...TODO.... The focus of the 
present document is on the container, not on the content itself.
* This is not a guide to R package or Shiny development. While I've struggled to
make this documentation as self-contained as possible, I need to assume some 
proficiency in these matters from the side of the reader. A few resources that
could help: [R Packages](https://r-pkgs.org/) by Hadley Wickham and Jennifer 
Bryan; the [Shiny Website](https://shiny.posit.co/), which has a lot of 
introductory content; [ChatGPT](https://chatgpt.com/), which can be an extremely 
helpful coding companion for R package development.

## Bird's eye view

The diagram below provides a highly schematic view of the internal data 
processing performed by the various components of the `{erahumed}` R package. 
The main function of `{erahumed}` is to produce the interactive dashboard of the 
[ERAHUMED DSS](https://www.erahumed.com/decision-support-system/), which is 
represented by the "User Interface" block at the bottom of the diagram. The 
remaining solid blocks represent various intermediate outputs, that are supposed 
to be computed in real time while the user is navigating through the 
dashboard, by internal functions of the package. User interactions allow the 
user to tweak the models' inputs, both at the level of data and parameters, 
which explains the necessity of being able to recompute in real time these 
solid blocks.

In contrast to what we just said, the "Raw Data" block on the top of the 
diagram, and its functional connection to the "Processed Data" block (
"data cleaning/imputation") are not required to be computed/executed in real 
time and are relegated to one-time scripts whose execution is left to the 
package developer whenever the data needs to be updated (this is standard 
practice in R package development, see *e.g.* 
[the "Data" chapter](https://r-pkgs.org/data.html) of the "R Packages" online 
book).

![{erahumed} data stack](erahumed_data_stack.png)

## Developer tools and frameworks

The development and testing of the software relies on several external tools and
frameworks, which I list below in approximate order of importance. After each 
item you can find a list of relevant references. 

* **R packages.** We obviously rely on this general framework, which I 
adopted to make the software easy to deploy and test, mainly. 
  - [R Packages](https://r-pkgs.org/)
  - [Advanced R](https://adv-r.hadley.nz/)
* **`{roxygen2}`.** The mechanism used for R package documentation.
  - [`{roxygen2}` website](https://roxygen2.r-lib.org/)
* **Shiny.** The framework for creating the interactive dashboard.
  - [The Shiny Website](https://shiny.posit.co/)
* **`{testthat}` and derivatives.** This R package was used to build the 
software testing infrastructure.
  - [`{testthat}` documentation](https://r-pkgs.org/)
* **Codecov.** For generating package coverage reports. This was implemented 
through the [{covr}](https://covr.r-lib.org/) R package.
  - [Codecov official documentation](https://docs.codecov.com/docs/quick-start)
  - [{covr} R package website](https://covr.r-lib.org/)
* **Git and Github.** For code versioning.
  - [GitHub official documentation](https://docs.github.com/)
* **Github Actions.** For continuous testing.
  - [Github official documentation](https://github.com/features/actions)
* **Semantic versioning.** The rationale I used to name software versions.
  - [Semantic Versioning Manifesto](https://semver.org/)

Finally, while this is not really a dependence, if you are going to actively 
work on the code for `{erahumed}` I strongly suggest using RStudio and the 
packages `{devtools}` and `{usethis}`, that automatize a lot of the mechanical 
work required to get an R package working smoothly.

## Folder structure of the repository

The repository contains the files and folders shown in the tree below. The 
objects marked with an asterisk are those that are used for R package building 
(*i.e.* those that are not matched by the contents of the `.Rbuildignore` file).

It is worth noticing that most of the content shown below is automatically 
generated by the tools mentioned in the previous section, and you will never 
need to directly interact with it. In fact, if you are going to work on the R 
code behind `{erahumed}`, you will be mostly working within the `R/` and 
`tests/` folder, and possibly on the scripts contained in the `data-raw/` 
folder. These three important directories are described in more detail below.

```
.
| .gitignore        Ignored files for Git
| .Rbuildignore   * Ignored files for R package building
| codecov.yml       Configuration file for Codecov
| DESCRIPTION     * Package description and metadata
| erahumed.Rproj    RStudio project file
| LICENSE.md        Project license
| NAMESPACE       * R package namespace definition
| NEWS.md         * Project changelog
| README.md       * Project README
| README.Rmd        README source in .Rmd format, used to generate README.md
|
+--- .github/       GitHub configurations and workflows
|
+--- benchmarks/    R scripts for benchmarking algorithm performances
|
+--- data/        * Data files used in the project
|
+--- data-raw/      Raw data and R scripts to generate the content of data/
|
+--- docs/          Documentation for developers, contains this guide
|
+--- man/         * R manual pages
|
+--- R/           * R functions definitions
|
+--- tests/       * Unit and integration tests
```

### The `R/` folder

The `R/` folder is the exclusive place where the functions exported by the
`{erahumed}` package are defined. Alongside with function definitions, you will
find extensive commentary test, where comment lines start with a `#'` 
sign; this is really `{roxygen2}` code used to generate the package 
documentation (the one that you access through `?` from the R console), and to
give R directives about what should be exported to the `{erahumed}` namespace.
How `{roxygen2}` works is described extensively 
[here](https://r-pkgs.org/man.html).

Below you can see the content of the `R/` folder at the time of writing, while
the core of `{erahumed}` is still under development. By the time you read this
document, the content of this folder will likely be much richer, but the logical
structure will be the same as described in this Section.

Each `.R` file contains the definition of some functions, plus some roxygen 
documentation and tags. In particular, functions that are not preceded by the
roxygen tag `#' @export` won't be accessible to the user through 
`library(erahumed); function_name()` or `erahumed::function_name()` (they will
still be accessible through `erahumed:::function_name()`).

Code is conceptually organized in "modules", and we recognize here four of them:

* `hb`, standing for Hydrological Balance, is the part of the code that is 
devoted to this kind of calculations.
* `aux` is a collection of auxiliary functions that are used extensively 
throughout the whole code.
* The `shiny` module contains the main utilities used to orchestrate the user 
interface of the ERAHUMED dashboard (pieces of UI relative to specific modules
are still defined within the modules, *e.g.* `hb_shiny.R`).
* The `data` module contains everything that has to do with *input* data.

If you browse through these files, you will notice that two of them do not 
contain any function definition. These are the `data.R` and `erahumed-package.R`
files. The purpose of these two files is explained in the "R Packages" book, 
[here](https://r-pkgs.org/data.html#sec-documenting-data) and
[here](https://r-pkgs.org/man.html#sec-man-package-doc).

```
.
|
| ...
|
+---R/
|       aux_assertions.R
|       aux_plot.R
|       aux_utils.R
|       data.R
|       data_shiny.R
|       erahumed-package.R
|       hb_global.R
|       hb_global_s3_class.R
|       hb_global_units.R
|       hb_global_wrapper.R
|       hb_helpers.R
|       hb_local.R
|       hb_local_s3_class.R
|       hb_local_units.R
|       hb_local_wrapper.R
|       hb_plot.R
|       hb_shiny.R
|       shiny_main.R
|
| ...
```

### The `tests/` folder

The `tests/` folder is where the various tests for R code functionality are 
defined. The general testing framework we employ is 
[`{testthat}`](https://r-pkgs.org/), while for specific Shiny related testing
we use [`shiny::testServer()`](https://shiny.posit.co/r/articles/improve/server-function-testing/) (for server function testing) and [`{shinytest2}`](https://rstudio.github.io/shinytest2/) (for snapshot based
testing).

Below is shown the content of the `tests/` folder. 
The `tests/testthat.R` and `tests/testthat/setup-shinytest2.R` are setup files, 
while the `snaps/` folder is automatically generated; you don't need to worry
about none of these. Focusing on the remaining `tests/testthat/test-*.R` files,
you will notice that the structure mirrors the content of the `R/` folder. The 
logic is simple: `test-<NAME>.R` contains the tests for the functions defined
in `R/<NAME>.R`.

An exception is given by the `test-shinytest2.R`. This file contains 
[`{shinytest2}`](https://rstudio.github.io/shinytest2/) tests, that simulate 
certain user interactions with the Shiny app and check that the resulting output 
is predictable.

```
.
|
| ...
|
+---tests/
|   |   testthat.R
|   |
|   \---testthat/
|       |   setup-shinytest2.R
|       |   test-aux_plot.R
|       |   test-aux_utils.R
|       |   test-hb_global.R
|       |   test-hb_global_s3_class.R
|       |   test-hb_global_units.R
|       |   test-hb_global_wrappers.R
|       |   test-hb_local_s3_class.R
|       |   test-hb_local_units.R
|       |   test-hb_local_wrappers.R
|       |   test-hb_plot.R
|       |   test-hb_shiny.R
|       |   test-shinytest2.R
|       |
|       \---_snaps/
|           |   albufera_hb_local.md
|           |
|           \---shinytest2/
|                   ERAHUMED-001.json
|                   ERAHUMED-001_.png
|
| ...
```

### The `data-raw/` folder

This folder contains the one-time scripts that are used to generate the 
pre-processed data contained in the `data/` folder. These scripts are supposed 
to be manually executed by the package maintainer, whenever the `{erahumed}` 
datasets need to be updated, because some raw input, or the 
pre-processing logic, or both, have changed. These updates should be accompanied
with a new release of the `{erahumed}` package, and you will need to update 
`{erahumed}` to the latest version in order for the changes to have effect on 
your local R installation.

It is worth stressing that the `data-raw/` folder is *ignored* during the 
building of `{erahumed}`, and the only input to the package is the content of 
`data/`. The advantage of scripting the data generation process is that it makes 
it easy to reproduce and modify it, whenever needed.

The content of the folder is shown below: there some R scripts that extract raw 
data from the `data-raw/raw/` folder, do some pre-processing, and export the 
resulting data-frames to the `data/` folder in `.rda` format (ready to be 
included in the `{erahumed}` namespace at the next installation). The structure
of these scripts is to a large extent arbitrary; I usually employ 
`usethis::use_data(overwrite = TRUE)` to handle the final export to the `data/` 
folder, and I would recommend sticking to it if you don't have any strong reason
for doing otherwise.

```
.
|
| ...
|
+---data-raw
|   |   albufera_clusters.R
|   |   albufera_data.R
|   |   albufera_ditches_inflow_pcts.R
|   |   albufera_management.R
|   |
|   \---raw
|           CHJ.csv
|           meteo_beni_2023.xlsx
|           mod_corr_perellonet_hi.rds
|           mod_corr_pujol_hi.rds
|           mod_corr_pujol_low.rds
|           mod_corr_water_level_low.rds
|           mod_perello.rds
|           mod_perellonet.rds
|           mod_pujol.rds
|           mod_waterlevel_change.rds
|           mod_water_level.rds
|           paddysdef.dbf
|           paddysdef.prj
|           paddysdef.shp
|           paddysdef.shx
|           paddy_management_farmer_account.csv
|           pct_soria_obs.rds
|
| ...
```

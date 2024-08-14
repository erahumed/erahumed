# Modifying `{erahumed}`

This guide describes the steps required to setup the local development 
environment required to modify the `{erahumed}` R package, which are to some 
extent universal for R packages designed following standard patterns. If you're
looking for a detailed description of how the `{erahumed}` code is structured,
you may want to consult [this other guide](how_is_code_organized.md).

## Cloning the repository

The source code for `{erahumed}` lives in a Git repository, which is at the time
of writing remotely hosted on the (private)  [erahumed/erahumed](https://github.com/erahumed/erahumed/) Github repository.
Since you are reading these notes, I assume that you have access to this remote
repository.

In order to work locally on `{erahumed}`, you will need a local clone of the 
repository, for which you can follow [Github's official guide](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository).

In general, Github is a very good option for hosting an R package repository 
online, with an [extensive documentation](https://docs.github.com/), and I 
would encourage to keep using it for later developments on this package
(either push on the original repo or fork it/create a new repo). 
If you have a hard time understanding the documentation, 
[ChatGPT](https://chatgpt.com/) can also be of great help here.

## Installing the dependencies

In order to work locally on `{erahumed}`, you will need its software 
dependencies to be installed on your machine. Luckily, all `{erahumed}` 
dependencies are just R packages, that you should be able to install without too
much trouble from within R, using `install.packages()` or 
`devtools::install_*()`.

The first step would be installing
the `{erahumed}` package itself, for which you can consult the [Installing `{erahumed}`](installing_erahumed.md) guide. If you follow the instructions 
given there (in particular, if you use 
`remotes::install_github(dependencies = TRUE)`), this should already take care 
of all hard dependencies (that are strictly required to locally run the 
software), as well as soft dependencies (that are only required for 
development).

If, for whatever reason, the above procedure does not work for you, notice 
in general that package dependencies are listed in the package DESCRIPTION file 
(found in the root folder of the package repository), under the fields 
`Depends`, `Imports` and `Suggests` - the first two corresponding to hard 
dependencies. These fields are lists of R packages that are available on the 
CRAN public repository, and you can individually handle the installation 
of each missing dependency using `install.packages()`.

## Modifying the code

While you may or may not decide to continue hosting the package source code on
Github, I definitely recommend to continue using Git for tracking the 
incremental improvements to the `{erahumed}` software. For a gentle introduction
to these concepts, geared towards RStudio users, I can recommend this 
[short free online course](https://www.coursera.org/learn/data-scientists-tools?specialization=jhu-data-science) from the John's Hopkins University, which will take you
about one afternoon or less. Also, again, [ChatGPT](https://chatgpt.com/) can be 
a great coding companion (for beginners and beyond).

In order to understand how the R code is organized, you 
should consult the dedicated guide: 
[How is code organized?](how_is_code_organized.md). When making modifications to 
the code, you can (and should) leverage on the well 
developed testing infrastructure to check that the package keeps working as 
expected - see [Testing `{erahumed}`](testing_erahumed.md) for further 
information.


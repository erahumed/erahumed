# Installing `{erahumed}`

## Using `remotes::install_github()`

If you are accessing these documentation pages, it is likely that you have 
access to the GitHub repository that hosts the `{erahumed}` package project 
(at the time of  writing this is 
[vgherard/erahumed](https://github.com/vgherard/erahumed/), but the owner 
GitHub account may change in future).

Assuming you have access to the GitHub repository, the simplest and safest 
option for installing the package is to use:

```r
remotes::install_github("vgherard/erahumed", dependencies = TRUE)
```

The `dependencies = TRUE` argument will install all package dependencies, and is recommended; see also the [Modifying `{erahumed}`](modifying_erahumed.md) guide.

In order for this to succeed, though, you need a GitHub PAT (with read access
for private repositories) to be stored in the local git credential store. If you
have no idea about what any of this means, you can have a look at 
`?remotes::install_github()` and, in particular, at the documentation for the
`auth_token` argument.

## Cloning the repository and installing locally from the RStudio IDE

Again assuming you have access to the private GitHub repository hosting the
`{erahumed}` source code, another option would be to [clone the repository](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository) and use the RStudio IDE to install the package. Cloning the 
repository is also something you will need to do if you are eventually 
interested in [modifying the package](modifying_erahumed.md).

In order to install `{erahumed}` from RStudio, open its RStudio project (*i.e.*
the `erahumed.RProj` file contained in the root directory of the repository) 
and, from the menu of RStudio, choose "Build > Install Package".

## Installing from compressed source files

A different possibility is that you only have access to the source files, 
usually compressed in a .tar.gz archive. Notice that this would be also the 
guide to provide if you want to distribute this software to a user that does not
have access to the `{erahumed}` repo. 

In this case, I suggest to use `devtools::install()` (rather than the built-in 
`install.packages()`) to install from source, because this will automatically
take care of dependencies. The following code shows how to do this on a 
Windows platform, where R has utilities for selecting files interactively.

```r
install_from_tar <- function(path) {
  td <- tempdir()
  untar(path, exdir = td)
  pkg <- file.path(td, "erahumed")

  devtools::install(pkg, dependencies = TRUE)
}

# Choose file interactively. This can be done on Windows. On other platforms,
# you would substitute the following with `path <- "[... actual path ...]"`
path <- choose.files() 
install_from_tar(path = choose.files())  
```

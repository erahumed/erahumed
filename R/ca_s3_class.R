make_erahumed_ca <- function(df)
{
  make_erahumed_ca_argcheck(df)

  class(df) <- c("erahumed_ca", class(df))
  attr(class(df), "package") <- "erahumed"
  return(df)
}


make_erahumed_ca_argcheck <- function(df) {
  return(invisible(NULL))
}

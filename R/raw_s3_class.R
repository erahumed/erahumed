make_raw <- function(df)
{
  make_raw_argcheck(df)
  class(df) <- c("erahumed_raw", class(df))
  return(df)
}

make_raw_argcheck <- function(df)  # Not implemented
{
  tryCatch(
    {
      assert_data.frame(df)
    },
    error = function(e) {
      class(e) <- c("make_raw_argcheck_error", class(e))
      stop(e)
    })
}

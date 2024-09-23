make_inp <- function(df)
{
  make_inp_argcheck(df)
  class(df) <- c("erahumed_inp", class(df))
  return(df)
}

make_inp_argcheck <- function(df)  # Not implemented
{
  tryCatch(
    {
      assert_data.frame(df)
    },
    error = function(e) {
      class(e) <- c("make_inp_argcheck_error", class(e))
      stop(e)
    })
}

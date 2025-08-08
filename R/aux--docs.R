strip_roxy_macros <-  function(x)
  gsub("\\\\link(?:\\[[^]]+\\])?\\{([^}]+)\\}", "`\\1`", x = x)

wrap_bcktcks <- function(x)
  paste0("`", x, "`")

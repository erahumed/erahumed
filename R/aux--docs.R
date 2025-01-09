strip_roxy_macros <-  function(x)
  gsub("\\\\link(?:\\[[^]]+\\])?\\{([^}]+)\\}", "`\\1`", x = x)

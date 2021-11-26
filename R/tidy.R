assess_tidyable <- function(ms)
{
  rn <- rownames(ms)
  cn <- colnames(ms)

  if (is.null(rn) || is.null(cn))
    stop("tidy functions work only with defined dimnames", call. = FALSE)

  invisible(NULL)
}



assess_all_vars <- function(expr, names, env)
{
  vars <- all.vars(expr)
  vars_in <- vars %in% names
  if (!all(vars_in)) {
    not_in <- vars[!vars_in]
    env_vars <- ls(env)
    not_found <- !(not_in %in% env_vars)
    if (any(not_found)) {
      stop(paste("could not find:",
                 paste(encodeString(not_in[not_found], quote = "'"),
                       collapse = ", ")),
           call. = FALSE)
    }
  }
}



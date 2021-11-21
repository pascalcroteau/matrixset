assess_tidyable <- function(ms)
{
  rn <- rownames(ms)
  cn <- colnames(ms)

  if (is.null(rn) || is.null(cn))
    stop("tidy functions work only with defined dimnames", call. = FALSE)

  invisible(NULL)
}

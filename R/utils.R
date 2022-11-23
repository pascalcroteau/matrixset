is_matrixish <- function(x)
{
  is.matrix(x) || is(x, "Matrix")
}



times <- cli::symbol$times

._NULL_ <- logical()
attr(._NULL_, ".__NULL__") <- logical()

is_null_obj <- function(x) !is.null(attr(x, ".__NULL__"))


style_grey0.8 <- crayon::make_style(grDevices::grey(0.8), grey = TRUE)
style_dim <- function(nms, dimmed)
{
  if (dimmed) return(style_grey0.8(nms))
  crayon::blue(nms)
}


round2 <- function(x) UseMethod("round2")
round2.default <- function(x) x
round2.numeric <- function(x) round(x, 2)



enclose <- function(str, pre = "<", end = ">") paste0(pre, str, end)



#' @importFrom rlang .data
make_unique <- function(names)
{
  nm_df <- tibble::enframe(names, name = NULL)
  nm_df <- dplyr::mutate(dplyr::group_by(nm_df, .data$value), idx=dplyr::row_number())
  dplyr::mutate(nm_df,
                n = dplyr::n(),
                new_nm = ifelse(.data$n > 1,
                                paste(.data$value, .data$idx, sep = dots_for_names),
                                .data$value))[["new_nm"]]
}



rep_unique <- function(name, n)
{
  if (n > 1) paste0(name, dots_for_names, seq_len(n)) else sub("", paste0(dots_for_names, "1"), name)
}



make_names <- function(obj, .name)
{
  nms <- names(obj)
  n <- length(obj)

  if (is.null(nms)) return (rep_unique(.name, n))

  nms_empty <- nms == ""
  if (any(nms_empty))
  {
    nb <- rep_unique("", n)
    nb[!nms_empty] <- nms[!nms_empty]
    # return(nb)
    nms <- nb
  } #else make_unique(nms)
  nms_out <- make_unique(nms)

  chg <- nms_out != nms
  if (any(chg)) {
    warning(paste("name changes occured:",
                  paste(nms[chg], nms_out[chg], sep = " -> ", collapse = ", ")))
  }

  nms_out
}


concat <- function(x, y, sep = " ")
{
  pst <- paste(x, y, sep = sep)
  gsub(" $", "", pst)
}



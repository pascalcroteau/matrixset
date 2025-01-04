


`%.==.%` <- function(x, y)
{
  if (is.null(x)) FALSE else if (is.null(y)) FALSE else x == y
}





is_matrixish <- function(x)
{
  is.matrix(x) || is(x, "Matrix")
}





flatten_or <- function(str)
{
  stringr::str_flatten(sQuote(str), collapse = ", ", last = " or ")
}




#' Determines tag value
#'
#' Determines and return the pronoun available to use in the apply* function.
#'
#' @param margin    One of 0 (no margin; whole matrix), 1 (row) or 2 (column)
#'
#' @returns
#' string, the pronoun
#'
#' @noRd
get_var_tag <- function(margin)
{
  if (is.null(margin) || margin == 0) {
    var_lab_mat
  } else if (margin == 1) {
    var_lab_row
  } else {
    var_lab_col
  }
}



#' Flatten and set names of a list
#'
#' @description
#' Removes a layer to a list. Once flattened, sets the names of the flattened
#' list, possibly using the list/vector `nm` to do so.
#'
#' @details
#' If provided, `nm` is used to concatenate its values to the original names of
#' `x`.
#'
#' @param x    list to flatten
#' @param nm   vector or list of names to set the names of the flattened list.
#'
#'
#' @noRd
flatten_and_name <- function(x, nm) {

  n <- length(x)
  nms <- names(x)
  r <- c()
  for (i in 1:n) {
    nmsi <- if (is.null(nm[[i]])) nms[i] else paste(nms[i], nm[[i]])
    r <- c(r, setNames(x[[i]], nmsi))
  }
  r

}




match_option <- function(x, opts)
{
  if (!x %in% opts) {
    msg <- stringr::str_glue("'{x}' is not valid. It must be one of {OPTS}.",
                             OPTS = flatten_or(opts))
    stop(msg)
  }
  x
}



is_null_obj <- function(x) !is.null(attr(x, ".__NULL__"))


style_grey0.8 <- crayon::make_style(grDevices::grey(0.8), grey = TRUE)
style_dim <- function(nms, dimmed)
{
  if (dimmed) return(style_grey0.8(nms))
  crayon::blue(nms)
}


round2 <- function(x) UseMethod("round2")
#' @export
round2.default <- function(x) x
#' @export
round2.numeric <- function(x) round(x, 2)



enclose <- function(str, pre = "<", end = ">") paste0(pre, str, end)


`%OR%` <- function(x, y) {
  if (is.character(x) && is.character(y)) {
    if (any(x == "")) {
      x[x == ""] <- y[x == ""]
      x
    } else x
  } else rlang::`%||%`(x, y)
}



#' @importFrom rlang .data
# make_unique <- function(names)
# {
#   nm_df <- tibble::enframe(names, name = NULL)
#   nm_df <- dplyr::mutate(dplyr::group_by(nm_df, .data$value), idx=dplyr::row_number())
#   dplyr::mutate(nm_df,
#                 n = dplyr::n(),
#                 new_nm = ifelse(.data$n > 1,
#                                 paste(.data$value, .data$idx, sep = dots_for_names),
#                                 .data$value))[["new_nm"]]
# }


make_unique <- function(names)
{
  id <- unique_id(names, length(names))
  nms <- paste(names, id, sep = dots_for_names)
  if (any(no_chg <- id == 0L)) nms[no_chg] <- names[no_chg]
  nms
}



rep_unique <- function(name, n)
{
  if (n > 1) paste0(name, dots_for_names, seq_len(n)) else sub("", paste0(dots_for_names, "1"), name)
}



make_names <- function(obj, .name, null_is_null = FALSE)
{
  nms <- names(obj)
  n <- length(obj)

  if (is.null(nms)) {
    if (null_is_null) return (NULL) else return (rep_unique(.name, n))
  }

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



is_vec <- function(x) {
  is.vector(x) && !is.list(x)
}



list_row <- function(v)
{
  l <- lapply(v, function(x) if (is_vec(x)) x else list(x))
  names(l) <- names(v)
  l
}


quoted <- function(x)
{
  encodeString(x, quote = "`")
}


warn_if <- function(lgl1, lgl2, env = rlang::caller_env())
{
  lgl1_str <- rlang::as_name(rlang::enquo(lgl1))
  lgl2_str <- rlang::as_name(rlang::enquo(lgl2))
  if (lgl1 && lgl2)
    warning(paste0(quoted(lgl2_str), " is TRUE but so is ", quoted(lgl1_str),
                   ". ", quoted(lgl2_str), " will be ignored."),
            call. = FALSE, immediate. = TRUE)
}







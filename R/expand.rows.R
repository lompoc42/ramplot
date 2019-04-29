#' @title Data handler. Internal only.
#'
#' @description Expands vector in rows
#' @param dat xts object or matrix
#' @return NULL
#'
expand.rows = function(dm, n){
  t(expand.cols(dm, n))
}

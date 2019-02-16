#' @title Data handler. Internal only.
#'
#' @description Logical output function for minute data.
#' @param dat xts object or matrix
#' @return NULL
#'
expand.cols = function(dm, n){
  matrix(rep(dm, n), ncol = n)
}

#' @title ReSolve Plot Preparation Function
#'
#' @description Helps add line break in labels
#'
#' @usage addLine(...)
#' @param ... User input.
#'
#' @return NULL
#'
addline_format = function(x,...){
  gsub('\\s','\n',x)
}

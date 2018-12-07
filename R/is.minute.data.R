#' @title ReSolve minute data detection
#'
#' @description Logical output function for minute data
#' @param dat Input data source
#' @return NULL
#'
is.minute.data = function(dat){
  if(is.xts(dat)) dat = index(dat)
  !all(is.na(as.character(strptime(dat, "%Y-%m-%d %H:%M"))))
}

#' @title ReSolve drawkeyline function. Internal.
#'
#' @description Do not call separately
#' @param data Input
#' @param params Parameters
#' @param size Sizes
#' @return NULL
#'
draw_key_line <- function(data, params, size) {
  data$linetype[is.na(data$linetype)] <- 0

  grid::segmentsGrob(0.1, 0.5, 0.9, 0.5,
                     gp = grid::gpar(
                       col = scales::alpha(data$colour, data$alpha),
                       lwd = data$size * .pt,
                       lty = data$linetype,
                       lineend = "butt"
                     )
  )
}

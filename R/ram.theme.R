#' @title ReSolve ggplot branding function
#'
#' @description Returns a ReSolve branded theme for ggplot2 objects.
#' @return NULL
#'
ram.theme = function(){
  theme(

    # Basic white background
    plot.title = NULL,
    panel.background = element_blank(),  # Clears defaults
    legend.position = "none", # Turns off legend by default
    legend.title = element_text(size=8),
    legend.key = element_rect(fill = 'white'),

    # Text specs
    text = element_text(family='Helvetica'),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 10),

    # Axis specs
    axis.line = element_line(
      color = '#023858',  # ReSolve specific color
      size = 1.5,
      lineend = 'round'),

    # Tick specs
    axis.ticks = element_blank(),     # Clears defaults
    axis.ticks.y = element_line(
      color = 'white',  # ReSolve specific color
      size = 1.5,
      linetype = 'solid',
      lineend = 'square'
    ),
    axis.ticks.length = unit(2,'mm')

  )
}

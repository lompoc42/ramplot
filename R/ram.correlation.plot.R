#' @title ReSolve Correlation Plotting Function
#'
#' @description Base Plotting Functions: Correlation and Density
#' @usage ram.correlation.plot(dat,...)
#'
#' @param dat Data is assumed to be a matrix or vector of returns.
#' @param ... Complete argument details are located in \code{\link[RAMplot:ram.arguments]{ram.arguments}}
#'
#' @examples
#'
#' # The function will call the correlation plot by default.
#' # The density plot is called as follows
#'
#' ram.bar.plot(dat,emphasis=list(density=T))
#'
#' @return NULL
#' @export
#'
ram.correlation.plot = function(

  dat,
  x.attributes = list(
    breaks = NULL,
    labs = NULL,
    labs.tilt = F
  ),

  y.attributes = list(
    breaks = NULL,
    labs = NULL,
    show.values = F
  ),

  titles = list(
    main = NULL,
    subtitle = NULL,
    legend.labels = NULL,
    legend.rows = NULL,
    caption = NULL,
    x = NULL,
    y = NULL
  ),

  emphasis = list(
    emph.column = NULL, # Can be numeric or character
    hline = NULL,
    hline.size = NULL,
    hline.color = NULL,
    primary.column = 1,
    secondary.column = NULL,
    show.best.fit = T,
    density = T
  )

){


  # Plot Setup --------------------------------------------------------------

  ## Plot: RAM colors
  if(!emphasis$density){
    cols = c('#89d2ff','#00488d','#62c3ff')
  } else {
    cols = ram.colors(length(unique(dat$variable)))
  }

  ## Plot: Base plot build
  if(!emphasis$density){
    p = ggplot(dat,aes(Var2, Var1, fill = value)) +
      geom_tile(color = "white")+
      scale_fill_gradient2(
        low = cols[1], high = cols[2], mid =  cols[3],
        midpoint = 0, limit = c(-1,1), name="Pearson\nCorrelation") +
      coord_fixed() +
      ram.theme() +
      geom_text(aes(Var2, Var1, label = value),
                color = "black", size = 4) +
      theme(
        legend.justification = c(1.025, -0.2),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal",
        axis.ticks.y = element_blank()
      )

  } else {
    p = ggplot(dat, aes(value, fill = variable)) +
      geom_density(alpha=(0.5)) +
      ram.theme()
  }


  # Titles and tilt -----------------------------------------------------


  ## Argument: update theme x.attributes$labs.tilt
  if(x.attributes$labs.tilt){
    p = p +
      theme(
        axis.text.x = element_text(angle = 45, hjust=1)
      )
  }

  ## Argument: titles()
  main.title = titles$main ## main
  main.sub = titles$subtitle ## subtitle
  main.caption = titles$caption ## caption
  x.title = titles$x ## x title
  y.title = titles$y ## y title

  ## Plot: Add titles and legend
  p = p +
    labs(
      title = as.character(main.title),
      subtitle = as.character(main.sub),
      caption = main.caption,
      x = as.character(x.title),
      y = as.character(y.title)
    )

  if(!emphasis$density){
    p = p + guides(fill = guide_colorbar(
      barwidth = 7,
      barheight = 1,
      title.position = "top",
      title.hjust = 0.5))
  } else {

    ## Plot: Add colors
    if(length(titles$legend.labels)==0){
      leg = scale_fill_manual(values=cols)
    } else {
      leg = scale_fill_manual(values=cols, labels = legend.labels)
    }

    p = p +
      theme(legend.position = 'top', legend.title = element_blank()) +
      leg

  }

  if(any(sapply(titles,length)!=0)){
    p = p + theme(
      plot.title = element_text(size=12, hjust=0.5),
      plot.subtitle = element_text(size=10, hjust=0.5,vjust=-1)
    )
  }

  ## Plot: Print Plot
  suppressWarnings(print(p))

}

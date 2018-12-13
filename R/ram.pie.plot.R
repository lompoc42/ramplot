#' @title ReSolve Pie Plotting Function
#'
#' @description Base Plotting Functions: Pie and Ring
#' @usage ram.pie.plot(dat,...)
#'
#' @param dat Data is assumed to be a matrix or vector of weights
#' @param ... Complete argument details are located in \code{\link[RAMplot:ram.arguments]{ram.arguments}}
#'
#' @examples
#'
#' # The function will call the pie plot by default.
#' # The ring plot is called as follows
#'
#' ram.bar.plot(dat,emphasis=list(ring=T))
#'
#' @return NULL
#' @export
#'
ram.pie.plot = function(

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
    y = NULL,
    rounding = 1
  ),

  emphasis = list(
    emph.column = NULL, # Can be numeric or character
    hline = NULL,
    hline.size = NULL,
    hline.color = NULL,
    primary.column = 1,
    secondary.column = NULL,
    show.best.fit = T,
    waterfall = F
  )

){


  # Plot Setup --------------------------------------------------------------


  ## Plot: RAM colors
  cols = ram.colors(nrow(dat))

  ## Plot: Base build

  if(!emphasis$ring){
    p = ggplot(dat, aes(x = 1, weight = weights, fill = tickers)) +
      geom_bar(width = 1, size=1,colour = "white") +
      geom_text(x = 1.15, aes(y = center, label = paste(dat$tickers,paste0(weights, "%"),sep="\n"), colour = tickers,size=1.15,fontface ='bold'))
  } else {
    p = ggplot(dat, aes(x = 1, weight = weights, fill = tickers,ymin=ymin,ymax=ymax,xmin=2.5,xmax=4.5)) +
      geom_rect(size=1,colour = "white") +
      geom_text(x = 3.5, aes(y = (ymin+ymax)/2, label = paste(tickers,paste0(round(weights,2), "%"),sep="\n"), colour = tickers,size=0.35,fontface ='bold'))
  }

  p = p + coord_polar(theta = "y") +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank()
    ) +
    theme(legend.position="none")

  # X and Y axis ------------------------------------------------------------

  # Not applicable in pie plot

  # Titles ----------------------------------------------------------------



  ## Plot: Titles
  ## Argument: titles()
  main.title = titles$main ## main
  main.sub = titles$subtitle ## subtitle
  main.caption = titles$caption ## caption
  x.title = titles$x ## x title
  y.title = titles$y ## y title


  ## Plot: Add titles

  p = p +
    labs(
      title = as.character(main.title),
      subtitle = as.character(main.sub),
      caption = main.caption,
      x = as.character(x.title),
      y = as.character(y.title)
    )

  if(any(sapply(titles,length)!=0)){
    p = p + theme(
      plot.title = element_text(size=12, hjust=0.5),
      plot.subtitle = element_text(size=10, hjust=0.5,vjust=-1)
    )
  }

  ## Plot: Add colors
  p = p +
    scale_fill_manual(values = cols) +
    scale_color_manual(values = rep('white',length(cols)))

  ### Legend currently disabled
  print(p)


}

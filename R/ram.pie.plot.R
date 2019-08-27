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
  ),
  raw.out = F

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

  if(raw.out) return(list(plotObject=p,dat=dat))

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

ram.efmap.plot = function(
  dat,
  x.attributes = list(
    breaks = NULL,
    labs = NULL,
    labs.tilt = F,
    text.labs = 12
  ),

  y.attributes = list(
    breaks = NULL,
    labs = NULL,
    show.values = F,
    text.labs = 12
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
    ef.order = 'sharpe'
  ),
  raw.out = F
){



  # Colors, Line Sizes, and Legend Titles -----------------------------------

  ## Plot: Colors and line sizes
  cols = ram.colors(length(unique(as.character(dat$Var2))))


  # Begin Plot --------------------------------------------------------------


  ## Plot: Build base plot
  p = ggplot(dat, aes(x=Rank, y=value, fill=Var2)) +
    geom_area(position = 'stack') +
    ram.theme(
      text.xaxis = x.attributes$text.labs,
      text.yaxis = y.attributes$text.labs
    )

  if(raw.out) return(list(plotObject=p,dat=dat))


  # X and Y axis ------------------------------------------------------------


  ## Argument: x.attributes()
  x.breaks = x.attributes$breaks
  x.labels = x.attributes$labs
  tilt = x.attributes$labs.tilt

  ## Plot: add x.attributes
  if(toupper(emphasis$ef.order)=="RISK"|toupper(emphasis$ef.order)=="RETURN"){
    p = p +
      scale_x_continuous(labels=percent,expand = c(0.05,0.025))
  } else if (!is.null(x.breaks)&!is.null(x.labels)) {
    p = p +
      scale_x_continuous(breaks = x.breaks,
                         expand = c(0.05,0.025), # Gives margin
                         labels = x.labels)
  } else {
    p = p + scale_x_continuous()
  }

  ## Argument: update theme x.attributes$labs.tilt
  if(tilt){
    p = p +
      theme(
        axis.text.x = element_text(angle = 45, hjust=1)
      )
  }

  ## Argument: y.attributes
  if(!is.null(y.attributes$breaks)){
    p = p + scale_y_continuous(breaks = y.attributes$breaks,
                               labels= y.attributes$labs)
  } else {
    p = p + scale_y_continuous(labels = scales::percent)
  }


  # Titles ----------------------------------------------------------------


  ## Plot: Titles
  ## Argument: titles()
  main.title = titles$main ## main
  main.sub = titles$subtitle ## subtitle
  main.caption = titles$caption ## caption
  x.title = titles$x ## x title
  y.title = titles$y ## y title
  if(is.null(y.title))y.title = 'Portfolio Weight'

  ## Argument: legend.rows
  lr = ifelse(is.null(titles$legend.rows),1,titles$legend.rows)
  legend.labels = as.character(titles$legend.labels)

  if(length(legend.labels)==0){
    legend.labels = as.character(unique(dat$Var2))
  }


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

  if(lr>1){
    p = p + guides(colour = guide_legend(nrow = lr))
  }


  # Emphasis ----------------------------------------------------------------


  ## Plot: hline attributes
  # Argument: emphasis()
  if(!is.null(emphasis$hline)){ # hline

    # hline
    hlv = as.numeric(emphasis$hline)[1]
    # hline.size
    hls = as.numeric(ifelse(is.null(emphasis$hline.size),0.6,emphasis$hline.size))
    # hline.color
    hlc = ifelse(is.null(emphasis$hline.color),'black',emphasis$hline.color)

    ## Plot: Add hline to graph
    p = p +
      geom_hline(yintercept = hlv,
                 color = hlc,
                 size = hls)

  }

  ## Final plot out
  if(lr==0){
    p = p +
      scale_fill_manual(values=cols)
  } else {
    p = p +
      scale_fill_manual(values=cols, labels = legend.labels) +
      theme(legend.position = 'top', legend.title = element_blank())
  }

  return(p)

}

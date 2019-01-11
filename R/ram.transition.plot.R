#' @title ReSolve Transition Plotting Function
#'
#' @description Base Plotting Functions: Transition
#' @usage ram.transition.plot(dat,...)
#'
#' @param dat Data is assumed to be a matrix or vector of rolling weights
#' @param ... Complete argument details are located in \code{\link[RAMplot:ram.arguments]{ram.arguments}}
#'
#' @return NULL
#' @export
#'
ram.transition.plot = function(
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
    show.best.fit = T
  )
){



  # Colors, Line Sizes, and Legend Titles -----------------------------------

  ## Plot: Colors and line sizes
  cols = ram.colors(ncol(dat)-1)

  ## Argument: titles$legend.labels
  if(!is.null(titles$legend.labels) & length(titles$legend.labels)==(ncol(dat)-1)){
    tmp = dat[,-ncol(dat)]
    names(tmp) = titles$legend.labels
    tmp = cbind(tmp,dat[,ncol(dat)])
  }



  # Begin Plot --------------------------------------------------------------

  dat$idx = 1:nrow(dat)
  dat = melt(dat,id='idx')

  ## Plot: Build base plot
  p = ggplot(dat, aes(x=idx,y=value,fill=variable)) +
    geom_area(position = 'stack') +
    ram.theme(
      text.xaxis = x.attributes$text.labs,
      text.yaxis = y.attributes$text.labs
    )

  # X and Y axis ------------------------------------------------------------


  ## Argument: x.attributes()
  x.breaks = x.attributes$breaks
  x.labels = x.attributes$labs
  tilt = x.attributes$labs.tilt

  ## Plot: add x.attributes
  p = p +
    scale_x_continuous(breaks = x.breaks,
                       expand = c(0.05,0.025), # Gives margin
                       labels = x.labels)

  ## Argument: update theme x.attributes$labs.tilt
  if(tilt){
    p = p +
      theme(
        axis.text.x = element_text(angle = 45, hjust=1)
      )
  }

  ## Argument: y.attributes
  p = p + scale_y_continuous(breaks = y.attributes$breaks,
                             labels= y.attributes$labs)



  # Titles ----------------------------------------------------------------


  ## Plot: Titles
  ## Argument: titles()
  main.title = titles$main ## main
  main.sub = titles$subtitle ## subtitle
  main.caption = titles$caption ## caption
  x.title = titles$x ## x title
  y.title = titles$y ## y title

  ## Argument: legend.rows
  lr = ifelse(is.null(titles$legend.rows),1,titles$legend.rows)
  legend.labels = as.character(titles$legend.labels)

  if(length(legend.labels)==0){
    legend.labels = as.character(unique(dat$variable))
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

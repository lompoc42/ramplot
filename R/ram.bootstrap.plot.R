#' @title ReSolve Bootstrap Plotting Function
#'
#' @description Base Plotting Functions: Bootstrap
#' @usage ram.bootstrap.plot(dat,...)
#'
#' @param dat Data is assumed to be a matrix or vector of returns
#' @param ... Complete argument details are located in \code{\link[RAMplot:ram.arguments]{ram.arguments}}
#'
#' @return NULL
#' @export
#'
ram.bootstrap.plot = function(
  dat,
  x.attributes = list(
    breaks = NULL,
    labs = NULL,
    labs.tilt = T,
    text.labs = 12
  ),

  y.attributes = list(
    breaks = NULL,
    labs = NULL,
    show.values = F,
    text.labs = 12,
    text.vals = 2
  ),

  titles = list(
    main = NULL,
    subtitle = NULL,
    legend.labels = NULL,
    legend.rows = 2,
    caption = NULL,
    caption.size = 10,
    caption.justify = 'right',
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
    waterfall = F,
    ring = F,
    density = F,
    alpha = 0.25,
    ef.order = 'sharpe'
  ),

  ef = list(
    custom.mean=NULL,
    custom.covar=NULL,
    plot.assets=T,
    plot.CML=T,
    asset.label.offset=0.0025,
    CML.label.offset=c(-0.01,0.06),
    asset.color='#89d2ff',
    CML.color = '#9c3000',
    ef.color = '#00488d',
    rf=0,
    scale = NULL,
    t.horizon=10,
    resampled=F,
    n.samples = 1000,
    lw=0.5
  )
){

  # Colors, Line Sizes, Legend Titles, Empghasis ----------------------------

  ## First find out what we're doing

  pct.plot = em.plot = F
  wh = c()

  ## Check for percentile columns
  if(!is.null(emphasis$pct.column)){

    ## Argument: emphasis$pct.column
    if(is.numeric(emphasis$pct.column)){
      wh1 = as.numeric(emphasis$pct.column)
    } else {
      wh1 = which(colnames(dat)%in%as.character(emphasis$pct.column))
    }
    wh = c(wh,wh1)
    d1 = dat[,wh1,drop=F]
    pct.plot = T

  }

  ## Check for emphasis columns
  if(!is.null(emphasis$emph.column)){

    ## Argument: emphasis$emph.column
    if(is.numeric(emphasis$emph.column)){
      wh2 = as.numeric(emphasis$emph.column)
    } else {
      wh2 = which(colnames(dat)%in%as.character(emphasis$emph.column))
    }
    wh = c(wh,wh2)
    d2 = dat[,wh2,drop=F]
    em.plot = T
  }


  # Begin Plot -----------------------------------------------------------


  d0 = dat[,which(!1:ncol(dat)%in%c(wh,ncol(dat))),drop=F]
  n = ncol(d0)
  rankers = matrixStats::rowMedians(as.matrix(d0))
  rankers = as.numeric(colSums(d0 - expand.cols(rankers,ncol(d0)))^2)
  dorder = order(rankers,decreasing = T)

  namer = names(d0)
  d0 = ram.preplot(d0,'melt')
  d0$variable = factor(d0$variable,levels = namer[dorder])
  n = nrow(d0)/length(unique(d0$variable))

  cols1 = grDevices::colorRampPalette(col2hcl( # Gradient function
    ram.colors(lookup = c('dark.blue','cian'))))
  cols1 = as.character(cols1(length(unique(d0$variable))))
  cols1 = as.character(sapply(cols1,function(z)rep(z,n)))
  alf = ifelse(is.null(emphasis$alpha),1,emphasis$alpha)

  p = ggplot() +
    geom_line(data = d0,aes(x=idx,y=value,group=variable),
              color = cols1,
              size = 0.25,
              alpha = alf)


  # Add percentiles ---------------------------------------------------------


  if(pct.plot){
    d1 = ram.preplot(dat[,wh1,drop=F],'melt')
    n = nrow(d1)/length(unique(d1$variable))
    cols2 = as.character(sapply(ram.colors(length(unique(d1$variable))),function(z)rep(z,n)))
    p = p + geom_line(data = d1,aes(x=idx,y=value,group=variable),
                      color = cols2,
                      size = 0.6)
  }


  # Add emphasis columns ----------------------------------------------------


  if(em.plot){
    d2 = ram.preplot(dat[,wh2,drop=F],'melt')

    ## em-specific items. Can be made arguments
    em.cols = ram.colors(length(unique(d2$variable)))
    em.labs = as.character(unique(d2$variable))
    em.lines = rep(1.25,length(unique(d2$variable)))

    p = p + geom_line(data = d2,aes(x=idx,y=value,color=variable),
                      size = em.lines,
                      show.legend = T)
  }

  ## Plot: theme
  p = p +
    ram.theme(
      text.xaxis = x.attributes$text.labs,
      text.yaxis = y.attributes$text.labs,
      text.legend = titles$text.legend
    )

  ## Argument: titles$legend.labels
  if(ifelse(em.plot,length(titles$legend.labels)==length(em.labs),F)){
    em.labs = titles$legend.labels
  }


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
  lr = ifelse(is.null(titles$legend.rows),0,titles$legend.rows)

  ## Plot: Add titles
  p = p +
    labs(
      title = as.character(main.title),
      subtitle = as.character(main.sub),
      caption = main.caption,
      x = as.character(x.title),
      y = as.character(y.title)
    )

  ## Caption additions
  if(titles$caption.justify=='right'){
    titles$caption.justify = 1
  } else if (titles$caption.justify=='center') {
    titles$caption.justify = 0.5
  } else if (titles$caption.justify=='left') {
    titles$caption.justify = 0
  }

  ## Title sizes and justification
  p = p + theme(
    plot.title = element_text(size=12, hjust=0.5),
    plot.subtitle = element_text(size=10, hjust=0.5,vjust=-1),
    plot.caption = element_text(
      hjust = titles$caption.justify,
      size = titles$caption.size))


  # H lines -----------------------------------------------------------------


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
  if(em.plot){
    p = p +
      scale_color_manual(values = em.cols,labels=em.labs)
  }

  ## Argument: legend.rows
  if(lr>0){
    ## Add legend
    p = p +
      guides(color=guide_legend(nrow=lr,
                                override.aes = list(size = em.lines))) +
      theme(legend.position = 'top',legend.title = element_blank())
  }

  return(p)
}


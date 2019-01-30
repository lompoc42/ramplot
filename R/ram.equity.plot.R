#' @title ReSolve Equity Plotting Function
#'
#' @description Base Plotting Functions: Correlation and Density
#' @usage ram.equity.plot(dat,...)
#'
#' @param dat Data is assumed to be a matrix or vector of prices
#' @param ... Complete argument details are located in \code{\link[RAMplot:ram.arguments]{ram.arguments}}
#'
#' @examples
#'
#' ram.equity.plot(dat,...)
#'
#' @return NULL
#' @export
#'
ram.equity.plot = function(
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

  ## Plot: Colors and line sizes
  cols = ram.colors(ncol(dat)-1)

  ## Plot: Line sizes
  line.sizes = rep(0.6,ncol(dat)-1)

  ## Data: Naming order
  namer = namer.raw = names(dat)[-ncol(dat)]

  ## Argument: titles$legend.labels
  if(!is.null(titles$legend.labels) & length(titles$legend.labels)==(ncol(dat)-1)){
    tmp = dat[,-ncol(dat)]
    names(tmp) = titles$legend.labels
    tmp = cbind(tmp,dat[,ncol(dat)])
  } else if (!is.null(titles$legend.labels) & length(titles$legend.labels)!=ncol(dat)){
    warning('Attribute set to default: number of legend names must match dat',
            call. = FALSE, domain = NA)
  }


  # Column Emphasis ---------------------------------------------------------


  ## Plot: Line and color emphasis for input
  if(!is.null(emphasis$emph.column)){

    ## Argument: emphasis()

    # emph.column
    if(is.numeric(emphasis$emph.column)){
      ew = as.numeric(emphasis$emph.column)
    } else {
      ew = which(colnames(dat)%in%as.character(emphasis$emph.column))
    }

    # emph.color and line.sizes
    n = length(cols)
    cols[c(ew,1)] = cols[c(1,ew)]
    line.sizes[ew] = 1.5

    tmp.order = 1:length(namer)
    names(tmp.order) = namer
    tmp.order[c(ew,n)] = tmp.order[c(n,ew)]
    line.sizes = line.sizes[tmp.order]
    names(line.sizes) = namer
    cols = cols[tmp.order]
    namer = namer[tmp.order]
    dat = dat[,c(tmp.order,ncol(dat))]

  }


  # Begin Plot --------------------------------------------------------------


  datplot = ram.preplot(dat,'melt')
  datplot$variable = factor(datplot$variable,levels = namer)
  n = nrow(datplot)/length(unique(datplot$variable))

  ## Plot: Base build
  p = ggplot(datplot, aes(x = idx,y=value,color=variable)) +
    ram.theme(
      text.xaxis = x.attributes$text.labs,
      text.yaxis = y.attributes$text.labs,
      text.legend = titles$text.legend
    ) +
    geom_line(size=rep(line.sizes,each=n))


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
    legend.labels = namer.raw
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

  if(lr>=1){
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
      scale_color_manual(values=cols)
  } else {
    if(is.null(emphasis$emph.column)){
      tmp.order=1:length(namer)
    }
    p = p +
      scale_color_manual(values=cols,
                         labels = legend.labels, breaks = namer.raw) +
      theme(legend.position = 'top', legend.title = element_blank()) +
      guides(color = guide_legend(override.aes = list(size = line.sizes[tmp.order])))
  }

  return(p)
}

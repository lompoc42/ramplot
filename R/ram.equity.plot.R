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
    bootstap = F,
    bootstrap.probs = c(.01,.05,.5,.95,.99),
    bootstrap.alpha = 0.25,
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

  cols = as.character(na.omit(ram.colors(ncol(dat)-1)))

  if(emphasis$bootstrap==T){


    tmp = as.matrix(dat[,-ncol(dat)])
    a1 = ncol(tmp)

    cols = grDevices::colorRampPalette(col2hcl(
      ram.colors(lookup = c('dark.blue','cian'))))
    cols = as.character(cols(ncol(tmp)))

    wh = matrixStats::rowMedians(tmp)
    wh = as.numeric(colSums(tmp - expand.cols(wh,ncol(tmp)))^2)
    dorder = order(wh,decreasing = T)

    quantiles = as.data.frame(
      rowQuantiles(tmp,probs = emphasis$bootstrap.probs))
    a2 = ncol(quantiles)
    tmp = cbind(tmp,quantiles)
    names(tmp) = c(names(dat[,-ncol(dat)]),names(quantiles))
    dat = as.data.frame(cbind(tmp,dat$idx))
    names(dat) = c(names(dat)[-ncol(dat)],'idx')

    dorder = c(dorder,max(dorder)+(1:ncol(quantiles)))
    cols = c(cols,ram.colors(ncol(quantiles)))
    a1 = rep(emphasis$bootstrap.alpha,a1)
    a2 = rep(1,a2)

  } else if (length(cols)!=(ncol(dat)-1)) {
    cols = rep(cols,ncol(dat)-1)
    cols = cols[1:(ncol(dat)-1)]
  }

  ## Plot: Line sizes
  line.sizes = line.sizes.raw = rep(0.6,ncol(dat)-1)
  if(emphasis$bootstrap){
    line.sizes = line.sizes.raw = rep(0.1,ncol(dat)-(ncol(quantiles)+1))
    line.sizes = c(line.sizes,rep(0.5,ncol(quantiles)))
  }

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
  if(!is.null(emphasis$emph.column)&!emphasis$bootstrap){

    ## Argument: emphasis()

    # emph.column
    if(is.numeric(emphasis$emph.column)){
      ew = as.numeric(emphasis$emph.column)
    } else {
      ew = which(colnames(dat)%in%as.character(emphasis$emph.column))
    }

    # emph.color and line.sizes
    corder = c(ew,which(!1:length(namer)%in%ew))
    corder = match(1:length(namer),corder)

    dorder = c(which(!1:length(namer)%in%ew),rev(ew))
    cols = (cols[corder])[dorder]

    line.sizes[ew] = 1.5
    line.sizes.raw = line.sizes
    line.sizes = line.sizes[dorder]

  } else if (!emphasis$bootstrap) {
    dorder = 1:length(namer)
  }


  # Begin Plot --------------------------------------------------------------


  datplot = ram.preplot(dat,'melt')
  datplot$variable = factor(datplot$variable,levels = namer[dorder])
  n = nrow(datplot)/length(unique(datplot$variable))

  ## Plot: Base build
  if(!emphasis$bootstrap){
    p = ggplot(datplot, aes(x = idx,y=value,color=variable)) +
      ram.theme(
        text.xaxis = x.attributes$text.labs,
        text.yaxis = y.attributes$text.labs,
        text.legend = titles$text.legend
      ) +
      geom_line(size=rep(line.sizes,each=n))
  } else {
    alf = c(rep(a1,n),rep(a2,n))
    p = ggplot(datplot, aes(x = idx,y=value,color=variable)) +
      ram.theme(
        text.xaxis = x.attributes$text.labs,
        text.yaxis = y.attributes$text.labs,
        text.legend = titles$text.legend
      ) +
      geom_line(size=rep(line.sizes,each=n),alpha=alf)
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
  lr = ifelse(is.null(titles$legend.rows),1,titles$legend.rows)
  if(emphasis$bootstrap) lr = 0
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
  if(lr==0|emphasis$bootstrap){
    p = p +
      scale_color_manual(values=cols)
  } else {
    p = p +
      scale_color_manual(values=cols,labels=legend.labels,breaks=namer) +
      guides(color=guide_legend(nrow=lr,
                                override.aes = list(size = line.sizes.raw))) +
      theme(legend.position = 'top',legend.title = element_blank())
  }

  p

  return(p)
}

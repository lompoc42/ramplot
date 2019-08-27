#' @title ReSolve Efficient Frontier Plotting Function
#'
#' @description Base Plotting Functions: Correlation and Density
#' @usage ram.ef.plot(dat,...)
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
ram.ef.plot = function(
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
    show.best.fit = T
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
  ),
  raw.out = F

){



  # Colors, Line Sizes, Legend Titles, Empghasis ----------------------------

  ## Plot: Colors and line sizes
  cols = as.character(na.omit(ram.colors(20)))

  ## Plot: Line sizes
  line.sizes = rep(0.6,ncol(dat$dat)-1)

  ## Argument: titles$legend.labels
  if(!is.null(titles$legend.labels) & length(titles$legend.labels)==(ncol(dat$dat)-1)){
    tmp = dat$dat[,-ncol(dat$dat)]
    names(tmp) = titles$legend.labels
    tmp = cbind(tmp,dat$dat[,ncol(dat$dat)])
  } else if (!is.null(titles$legend.labels) & length(titles$legend.labels)!=ncol(dat$dat)){
    warning('Attribute set to default: number of legend names must match dat$dat',
            call. = FALSE, domain = NA)
  }


  # Begin Plot --------------------------------------------------------------


  ia = dat$ia
  af = dat$af
  dat = dat$dat
  arc.length = sqrt(dat$Risk^2+dat$Return^2)
  dist. = cumsum(arc.length)
  dist.delta = max(dist./10)
  dist.points = seq(min(dist.),max(dist.),dist.delta)
  idx = sapply(dist.points,function(x) which.min(abs(dist.-x)))

  ## Plot: Base build
  p = ggplot(dat, aes(Risk,Return), color=ef.inner.color) +
    geom_point(size=ef$lw) +
    ram.theme(
      text.xaxis = x.attributes$text.labs,
      text.yaxis = y.attributes$text.labs
    )

  if(raw.out) return(list(plotObject=p,dat=dat))


  ##########################################################################
  #### Plot Individual Assets
  ##########################################################################
  if(ef$plot.assets & !ef$resampled){
    df2  = data.frame(A.Risk = ia$risk*sqrt(af),A.Return = ia$expected.return*af)
    p=p+geom_point(data=df2, aes(A.Risk, A.Return), color=ef$asset.color,size=ef$lw*3,shape=15)
    df2 = df2+ ef$asset.label.offset
    p=p+geom_text(data=df2,aes(A.Risk, A.Return),label = rownames(df2),size = 3.5,
                  na.rm = T)

  }

  ##########################################################################
  #### Plot CML
  ##########################################################################
  if(ef$plot.CML){
    ia2=ia
    ia2$expected.return=ia2$expected.return-ef$rf/af
    if(ef$resampled){
      out = mclapply(1:ef$n.samples,function(i){
        set.seed(i)
        ret.r = MASS::mvrnorm(nrow(ret),ia$expected.return,ia$cov)
        ia.tmp = create.ia(ret.r)
        ia.tmp$expected.return = ia.tmp$expected.return-ef$rf/af
        w.msr = max.sharpe.portfolio('long-only')(ia.tmp,constraints)
      },mc.cores=detectCores())
      w.msr =colMeans(do.call(rbind,out))
    }
    else
      w.msr = max.sharpe.portfolio('long-only')(ia2,constraints)
    names(w.msr)=names(ia$risk)
    p.risk = as.numeric(sqrt(w.msr%*%ia$cov%*%w.msr)*sqrt(af))
    p.return = sum(w.msr*ia$expected.return)*af
    ln=nrow(dat)
    max.risk = max(dat$Risk)+0.05
    max.return = max(ia$expected.return)*af
    m = (p.return-ef$rf)/p.risk
    x=seq(0,max.risk,max.risk/(ln-1))
    y = m*x+ef$rf
    CML=data.frame(CML.Risk = x,CML.Return=y)

    cml.label = CML[round(ln/4),]
    ef$CML.label.offset=rep(ef$CML.label.offset,length(cml.label))
    cml.label[1]=cml.label[1]+ef$CML.label.offset[1]
    cml.label[2]=cml.label[1]+ef$CML.label.offset[2]
    rownames(cml.label)="Capital Market Line"

    p = p +
      geom_line(data = CML,aes(CML.Risk,CML.Return), color=ef$CML.color,size=ef$lw/2,
                na.rm = T)

  }

  dr = c(dat$Risk,df2$A.Risk)
  dr2 = c(dat$Return,df2$A.Return)


  # X and Y axis ------------------------------------------------------------


  ## Argument: x.attributes()
  x.breaks = x.attributes$breaks
  x.labels = x.attributes$labs
  tilt = x.attributes$labs.tilt

  xl = range(pretty(dr))*c(ifelse(all(dr>0),0.95,1.05),1.05)
  yl = range(pretty(dr2))*c(ifelse(all(dr2>0),0.95,1.05),1.05)

  p = p +
    scale_x_continuous(limits = xl) +
    scale_y_continuous(limits = yl)

  # ## Plot: add x.attributes
  # p = p +
  #   scale_x_continuous(breaks = x.breaks,
  #                      expand = c(0.05,0.025), # Gives margin
  #                      labels = x.labels)
  #
  # ## Argument: update theme x.attributes$labs.tilt
  # if(tilt){
  #   p = p +
  #     theme(
  #       axis.text.x = element_text(angle = 45, hjust=1)
  #     )
  # }
  #
  # ## Argument: y.attributes
  # p = p + scale_y_continuous(breaks = y.attributes$breaks,
  #                            labels= y.attributes$labs)


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
    legend.labels = names(dat)[-which(names(dat)%in%'idx')]
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
    p = p +
      scale_color_manual(values=cols, labels = legend.labels) +
      theme(legend.position = 'top', legend.title = element_blank())
  }

  return(p)
}

#' @title ReSolve Umbrella Plotting Function
#'
#' @description The umbrella plotting function for the RAMplot package. Data is assumed to be transformed appropriately for one of the following base plotting functions:
#'\describe{
#'  \item{\emph{:: equity}}{}
#'  \item{\emph{:: scatter}}{}
#'  \item{\emph{:: transition}}{}
#'  \item{\emph{:: waterfall}}{}
#'  \item{\emph{:: correlation}}{}
#'  \item{\emph{:: density}}{}
#'  \item{\emph{:: pie}}{}
#'  \item{\emph{:: ring}}{}
#'  \item{\emph{:: efmap}}{}
#'  \item{\emph{:: ef}}{}
#'}
#'
#' @usage ram.plot(dat,'plot.type',...)
#'
#' See examples for argument use cases.
#'
#' @param dat Data input transformed for corresponding plot type.
#' @param plot.type Selection: 'equity','scatter','bar','waterfall','correlation','transition','pie','ring','density'
#' @param start.date R formatted date input: Example '2000-12-31'
#' @param end.date R formatted date input: Example '2000-12-31'
#' @param render Logical and TRUE by default. If FALSE the plot will not appear in R.
#' @param ... Complete argument details are located in \code{\link[RAMplot:ram.arguments]{ram.arguments}}
#'
#' @examples
#'
#' ## Arguments can be called individually within a list.
#' ram.plot(dat,'equity',x.attributes=list(show.day=T))
#'
#' @return NULL
#' @export
#'
ram.plot = function(

  dat,
  plot.type = 'equity',
  start.date = NULL,
  end.date = NULL,
  render = T,

  x.attributes = list(
    show.day = F,
    breaks = NULL,
    labs = NULL,
    labs.tilt = F,
    text.labs = 12
  ),

  y.attributes = list(
    start.val = 100,
    breaks = NULL,
    labs = NULL,
    trans.log = T,
    trans.percent = F,
    show.values = F,
    currency = '$',
    text.labs = 12,
    text.vals = 4
  ),

  titles = list(
    main = NULL,
    subtitle = NULL,
    legend.labels = NULL,
    legend.rows = 2,
    caption = NULL,
    caption.size = 10,
    caption.justify = 'left',
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
    ring=F,
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
  ),

  output = list(
    file.name = NULL,
    file.type = 'pdf',
    width = 10,
    height = 5
  )

){


  # Internal Functions ------------------------------------------------------


  ram.scaler = function(dat,sval=100,ln=T){

    b1 = scale.one(dat)

    if(is.null(sval)){
      sval = 1
      ln = F
    }

    b2 = as.numeric(na.omit(as.numeric(b1)))
    comp = b2*sval

    if(ln){
      b3 = as.numeric(pretty(log2(b2)))
      b4 = 2^(b3)*sval
      b4 = b4[b4>min(comp)&b4<=max(comp)]

      breaks = log2(b4)
      labs = as.numeric(round(b4))


      if(!is.na(sval)) b1 = sval * b1
      b1 = log2(b1)
    } else {
      b3 = as.numeric(pretty(b2))
      b4 = b3*sval
      b4 = b4[b4>min(comp)&b4<=max(comp)]
      if(!sval%in%b4) b4 = sort(c(sval,b4))

      breaks = b4
      if(all(range(na.omit(as.numeric(as.matrix(dmat))))<2)){
        labs = as.numeric(round(b4,titles$rounding))
      } else {
        labs = as.numeric(round(b4))
      }

      if(!is.na(sval)) b1 = sval * b1
    }

    return(list(dat=b1,breaks=breaks,labs=labs))

  }


  # Argument Cleanup --------------------------------------------------------


  dat.raw = dat
  args.final = ram.arguments(type = 'umbrella',user.args = list(x.attributes,y.attributes,titles,emphasis,ef,output))
  x.attributes = args.final$x.attributes
  y.attributes = args.final$y.attributes
  titles = args.final$titles
  emphasis = args.final$emphasis
  ef = args.final$ef
  output = args.final$output


  # Data --------------------------------------------------------------------


  dat = dat.raw = ram.dat.standard(dat)

  ## Data: Date specific handling
  if(is.timeBased(index(dat))){

    ## Intraday data not supported
    if(is.minute.data(dat))stop('Intraday data not currently supported', call. = FALSE, domain = NA)

    ## Data: End dates
    if(!is.null(end.date)){
      if(is.null(dat[as.Date(end.date)])){
        stop('end.date not found in data. Date format is `2000-12-31`', call. = FALSE, domain = NA)
      } else {
        dat = dat[paste0('::',as.character(end.date))]
      }
    }

    ## Data: Start dates
    if(!is.null(start.date)){
      if(is.null(dat[as.Date(start.date)])){
        stop('end.date not found in data. Date format is `2000-12-31`', call. = FALSE, domain = NA)
      } else {
        dat = dat[paste0(as.character(start.date),'::')]
      }
    }
  }



  # Initial Data Handling ---------------------------------------------------



  ### Data transform for Equity and Scatter Plots
  if(plot.type%in%c('equity','scatter','transition')){

    ## Data: preplot data transformation
    dat = ram.preplot(dat, type = 'standard')

    ## Data: Scatterplot specific transforms
    if(plot.type=='scatter'){

      ## Check for return matrix
      rmat = dat[,-which(names(dat)%in%'idx')]
      if(all(na.omit(rmat>0))){
        rmat[] = ifna(as.matrix(rmat/mlag(rmat)-1),0)
      }
      dat[,-which(names(dat)%in%'idx')] = rmat

      # Arguments
      primary.column = emphasis$primary.column
      secondary.column = emphasis$secondary.column
      show.best.fit = emphasis$show.best.fit

      # Data: Ready dat to be analyzed
      # Argument: primary.column and secondary.column
      singular = is.null(secondary.column) | ncol(dat)==1 # Will need later.
      if(singular){
        dat$comp = dat[,primary.column]
        dat[,primary.column] = 1:nrow(dat)
        secondary.column = primary.column
      } else {
        dat$comp = as.numeric(dat[,secondary.column])
      }

      if(show.best.fit){
        l.model=lm(dat[,'comp']~dat[,primary.column])
        dat$`Line of Best Fit` =
          dat[,primary.column] *
          l.model$coefficients[2] +
          l.model$coefficients[1]
      }

      emphasis$primary.column = primary.column
      emphasis$secondary.column = secondary.column

    }

    ### Data transform for Bar and Waterfall Plots
  } else if (plot.type%in%c('bar','waterfall')) {

    ## Data: Preplot trasnforms per plot
    if(plot.type=='bar'){

      cn = rownames(as.data.frame(dat))
      c1 = suppressWarnings(inherits(try(as.Date(cn),T),'try-error'))
      c2 = suppressWarnings(try(is.integer(as.integer(cn)),T))
      c3 = ncol(dat.raw)>=2

      if((c1&c2)|c3){
        dat = ram.preplot(dat,'bar2')
      } else {
        dat = ram.preplot(dat,'standard')
      }
    } else {
      dat = ram.preplot(dat,'waterfall')
    }

  } else if (plot.type%in%c('correlation','density')) {

    emphasis$show.best.fit = F
    if(plot.type=='correlation'){
      dat = ram.preplot(cor(dat),'correlation')
    } else {
      dat = ram.preplot(dat,'melt')
      emphasis$density = T
      plot.type = 'correlation'
    }


  } else if (plot.type%in%c('pie','ring')) {
    dat = ram.preplot(dat,'pie')
    dat$weights = round(dat$weights,titles$rounding)
    if(plot.type=='ring') {
      emphasis$ring = T
      plot.type = 'pie'
    }
  } else if (plot.type%in%c('efmap','ef')) {

    if(plot.type=='efmap'){
      dat = ram.preplot(dat,'efmap',ef.order=emphasis$ef.order)
    } else {
      dat = ram.preplot(dat,'ef',ef=ef)
    }

  }



  # X Axis Attributes -----------------------------------------------------



  ## Argument: x.attributes
  show.day = x.attributes$show.day

  x.breaks = x.attributes$breaks
  if(!is.numeric(x.breaks)&!is.timeBased(x.breaks)) x.breaks = NULL

  x.labels = x.attributes$labs
  if(!is.null(x.labels)){
    n = length(x.labels)
    if(n==length(x.breaks)|n==nrow(dat.raw)){
      x.labels = as.character(x.labels)
    } else {
      x.labels = NULL
    }
  }

  ## Data: Determine plot type and format accordingly
  if(plot.type%in%c('equity','scatter','transition')){


    ## Condition 1: No valid breaks or labels
    if(is.null(x.breaks)&is.null(x.labels)){

      ## Argument: x.attributes$breaks
      x.breaks = pretty(1:nrow(dat))
      x.breaks = x.breaks[x.breaks>0&x.breaks<nrow(dat)]

      ## Data: Format for time based object
      if(is.timeBased(dat$idx)){
        x.labels = dat$idx[x.breaks]
        x.format = ifelse(show.day,"%d %b %Y","%b %Y")
        x.labels = as.character(format(x.labels, x.format))
      }

      ## Condition 2: Custom breaks
    } else if (!is.null(x.breaks)) {

      ## Argument: x
      if(is.timeBased(x.breaks)){

        if(is.null(x.labels)){
          x.labels = x.breaks
          x.format = ifelse(show.day,"%d %b %Y","%b %Y")
          x.labels = as.character(format(x.labels, x.format))
        }
        x.breaks = as.numeric(which(dat$idx%in%as.Date(x.breaks)))

      } else {
        x.labels = as.character(dat$idx)[x.breaks]
      }

    }

    x.attributes$breaks = x.breaks
    x.attributes$labs = x.labels
    x.attributes$show.day = NULL

  } else if (plot.type%in%c('bar','waterfall')) {

    ## This section deals with use-case of a data-frame input.
    if(!is.data.frame(dat.raw)&ncol(dat.raw)==1){

      ## Argument: x.attributes()
      x.breaks = 1:nrow(dat) # Manual x.breaks not supported in barplot

      if(is.null(x.labels)){
        if(plot.type=='bar'){
          if(length(unique(data.table::year(dat$idx)))==nrow(dat)){
            x.labels = as.character(data.table::year(dat$idx))
          } else {
            x.labels = format(dat$idx,'%Y-%b')
          }
        } else if (plot.type=='waterfall') {
          x.labels = as.character(dat$ids)
        }
      } else if (length(x.labels)==nrow(dat)) {
        x.labels = as.character(x.labels)
      } else {
        x.labels = as.character(x.breaks)
      }
    } else {

      ## Argument: x.attributes()
      if(plot.type=='bar'){
        x.labels = as.character(rownames(dat.raw)) # labs
        x.breaks = 1:nrow(dat.raw) # Manual x.breaks not supported in barplot
      } else {
        x.labels = as.character(dat$ids) # labs
        x.breaks = dat$idx # Manual x.breaks not supported in barplot
      }

    }

    x.attributes$breaks = x.breaks
    x.attributes$labs = x.labels

  } else if (plot.type%in%c('efmap','ef')) {

    if(!is.null(x.attributes$breaks)){
      x.labels = as.character(x.attributes$breaks)
    }

    x.attributes$breaks = x.breaks
    x.attributes$labs = x.labels

  }


  # Y Axis Attributes -----------------------------------------------------


  ## Argument: y.attributes$trans.log
  # The log transform is only valid for price series

  if(plot.type%in%c('equity')){

    dmat = dat[,-which(names(dat)%in%'idx')]

    ## Argument: y.attributes
    if(!y.attributes$trans.percent&all(na.omit(as.numeric(as.matrix(dmat)))>0)){

      start.val = y.attributes$start.val
      scaler = ram.scaler(dat.raw,start.val,ln=y.attributes$trans.log)
      y.breaks = scaler$breaks
      y.labs = scaler$labs
      dmat[] = coredata(scaler$dat)

      ## Data: Adjust hline settings for log
      if(!is.null(emphasis$hline)&y.attributes$trans.log){
        emphasis$hline = log2(emphasis$hline)
      }

      ## Argument: y.attributes$trans.percent
    } else if (y.attributes$trans.percent) {

      dm2 = na.omit(as.numeric(as.matrix(dmat)))

      if(all(as.numeric(dm2)>=0&as.numeric(dm2)<=1)){
        y.breaks = c(0,0.25,0.5,0.75,1)
        y.labs = paste0(comma(y.breaks* 100), "%")
      } else {
        y.breaks = y.labs = pretty(as.numeric(as.matrix(dm2)))
        y.labs = paste0(comma(y.breaks* 100), "%")
      }

    } else {

      n = as.numeric(as.matrix(dmat))
      y.breaks = y.labs = pretty(as.numeric(as.matrix(dmat)))

      ## Use case. If over half the data is below 1 we boost rounding.
      if((length(n[n<1])/length(n))>0.5){
        y.labs = y.breaks
      }

    }

    dat[,-which(names(dat)%in%'idx')] = dmat

    ## Argument: y.attributes$currency
    if(!is.null(y.attributes$currency)&!y.attributes$trans.percent){
      y.labs = paste0(
        as.character(y.attributes$currency),
        formatC(y.labs, format="f", digits=0, big.mark=",")
      )
    }

    ## Argument: update y.attribute settings
    y.attributes$breaks = y.breaks
    y.attributes$labs = y.labs

  } else if (plot.type%in%c('scatter')) {

    y.breaks = pretty(as.numeric(rmat[,primary.column],na.rm = T))
    y.labs = paste0(comma(round(y.breaks* 100,titles$rounding)), "%")

    ## Argument: update y.attribute and currency settings
    y.attributes$currency = NULL
    y.attributes$breaks = y.breaks
    y.attributes$labs = y.labs

  } else if (plot.type%in%c('bar','waterfall','transition')){

    if(plot.type=='bar'){
      if(!'idx'%in%names(dat)){
        y.breaks = as.numeric(pretty(sort(round(dat$value,4))))
        y.labs = paste0(comma(y.breaks* 100), "%")
      } else {
        y.breaks = as.numeric(pretty(sort(round(dat[,1],4))))
        y.labs = paste0(comma(y.breaks* 100), "%")
      }

    } else if (plot.type%in%c('waterfall','transition')){

      ## Waterfall use cases
      if(dat[nrow(dat),1]==1|plot.type=='transition'){
        y.breaks = c(0,0.25,0.5,0.75,1)
        y.labs = paste0(comma(y.breaks* 100), "%")
      } else {
        y.breaks = as.numeric(pretty(sort(round(dat$end,4))))
        y.labs = paste0(comma(y.breaks* 100), "%")
      }

    }

    y.attributes$breaks = y.breaks
    y.attributes$labs = y.labs
    y.attributes$show.values = T

    emphasis$waterfall = ifelse(plot.type=='waterfall',T,F)

    ## This is here because it needs to call the bar function.
    if(plot.type=='waterfall') plot.type = 'bar'

  } else if (plot.type%in%c('efmap','ef')){

    if(!is.null(y.attributes$breaks)){
      y.breaks = y.labels = y.attributes$breaks
    } else {
      y.labels = y.breaks = NULL
    }

    y.attributes$breaks = y.breaks
    y.attributes$labs = y.labels

  }



  # Titles ------------------------------------------------------------------



  ## Argument: titles$legend.labels
  if(!is.null(titles$legend.labels) & length(titles$legend.labels)==ncol(dat.raw)){
    tmp = dat[,-ncol(dat)]
    names(tmp) = titles$legend.labels
    tmp = cbind(tmp,dat[,ncol(dat)])
  } else if (!is.null(titles$legend.labels) & length(titles$legend.labels)!=ncol(dat.raw)){
    warning('Attribute set to default: number of legend names must match dat',
            call. = FALSE, domain = NA)
  }


  # Final Arguments ---------------------------------------------------------


  args.final = ram.arguments(type = 'base',user.args = list(x.attributes,y.attributes,titles,emphasis,ef))
  x.attributes = args.final$x.attributes
  y.attributes = args.final$y.attributes
  titles = args.final$titles
  emphasis = args.final$emphasis

  ## Final Plot Call
  plot.out = getFunction(paste0('ram.',plot.type,'.plot'))
  out = plot.out(dat=dat,x.attributes,y.attributes,titles,emphasis)
  out = out +
    theme(
      text = element_text(family='Helvetica Neue')
    )


  # Output ------------------------------------------------------------------


  ## Checks to see if there's a filename specified
  if(!is.null(output$file.name)){

    ## Makes sure filename is correct
    filename = as.character(output$file.name)
    fn = strsplit(output$file.name,'')[[1]]
    wh = which(fn=='.')
    if(length(wh)==0) filename = paste0(filename,'.pdf')

    ggsave(
      filename = filename,
      width = as.numeric(output$width),
      height = as.numeric(output$height),
      dpi = 300,
      plot = out,
      units = 'in'
    )

  }

  if(render){
    print(out)
  } else {
    out = out
  }

}

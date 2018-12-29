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

  x.attributes = list(
    show.day = F,
    breaks = NULL,
    labs = NULL,
    labs.tilt = F
  ),

  y.attributes = list(
    start.val = 100,
    breaks = NULL,
    labs = NULL,
    trans.log = T,
    trans.percent = F,
    show.values = F,
    currency = '$'
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
    waterfall = F,
    ring=F
  )

){


  # Internal Functions ------------------------------------------------------


  ram.scaler = function(dat,sval=100){

    b1 = scale.one(dat)

    if(is.na(sval)) sval = 1

    b2 = as.numeric(na.omit(as.numeric(b1)))
    comp = b2*sval
    b3 = as.numeric(pretty(log2(b2)))
    b4 = 2^(b3)*sval
    b4 = b4[b4>min(comp)&b4<=max(comp)]

    breaks = log2(b4)
    labs = as.numeric(round(b4))


    if(!is.na(sval)) b1 = sval * b1
    b1 = log2(b1)

    return(list(dat=b1,breaks=breaks,labs=labs))

  }


  # Argument Cleanup --------------------------------------------------------


  dat.raw = dat
  args.final = ram.arguments(type = 'umbrella',user.args = list(x.attributes,y.attributes,titles,emphasis))
  x.attributes = args.final$x.attributes
  y.attributes = args.final$y.attributes
  titles = args.final$titles
  emphasis = args.final$emphasis


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
      dat = ram.preplot(dat,'standard')
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
  }



  # X Axis Attributes -----------------------------------------------------



  ## Argument: x.attributes
  show.day = x.attributes$show.day
  x.breaks = x.attributes$breaks
  x.labs = x.attributes$labs

  if(plot.type%in%c('equity','scatter','transition')){
    if(
      ## Condition: Checks out x.breaks and x.labs.
      ifelse(is.null(x.breaks),T,
             ifelse(is.null(x.labs),F,
                    ifelse(length(x.labs)==length(x.breaks),F,T)))
    ){

      ## Argument: x.attributes$breaks
      x.breaks = pretty(1:nrow(dat))
      x.breaks = x.breaks[x.breaks>0&x.breaks<nrow(dat)]

      ## Data: Format for time based object
      if(is.timeBased(dat$idx)){
        x.labs = dat$idx[x.breaks]
        x.format = ifelse(show.day,"%d %b %Y","%b %Y")
        x.labs = as.character(format(x.labs, x.format))
      }

      x.attributes$breaks = x.breaks
      x.attributes$labs = x.labs
      x.attributes$show.day = NULL

    } else if (ifelse(!is.null(x.breaks)&is.null(x.labs),is.numeric(x.breaks)|is.timeBased(x.breaks),F)) {

      if(is.timeBased(x.breaks)){
        x.labs = x.breaks
        x.breaks = as.numeric(which(dat$idx%in%x.breaks))
      } else if (is.numeric(x.breaks)) {
        x.labs = as.character(dat$idx)[x.breaks]
      }

      ## Data: Format for time based object
      if(is.timeBased(dat$idx)){
        x.format = ifelse(show.day,"%d %b %Y","%b %Y")
        x.labs = as.character(format(x.labs, x.format))
      }

      x.attributes$breaks = x.breaks
      x.attributes$labs = x.labs
      x.attributes$show.day = NULL

    }

    ## Auto tilt x-axis labels when there's more than four breaks.
    if(length(x.breaks)>4) x.attributes$labs.tilt = T

  } else if (plot.type%in%c('bar','waterfall')){

    ## This section deals with use-case of a data-frame input.
    if(!is.data.frame(dat.raw)){

      ## Argument: x.attributes()
      x.breaks = 1:nrow(dat) # Manual x.breaks not supported in barplot
      x.labels = x.attributes$labs # labs

      if(is.null(x.labels)){
        if(plot.type=='bar'){
          if(length(unique(year(dat$idx)))==nrow(dat)){
            x.labels = as.character(year(dat$idx))
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
      x.labels = as.character(rownames(dat.raw)) # labs
      x.breaks = dat$idx # Manual x.breaks not supported in barplot

    }

    x.attributes$breaks = x.breaks
    x.attributes$labs = x.labels

  }


  # Y Axis Attributes -----------------------------------------------------


  ## Argument: y.attributes$trans.log
  # The log transform is only valid for price series

  if(plot.type%in%c('equity')){

    dmat = dat[,-which(names(dat)%in%'idx')]

    ## Argument: y.attributes$trans.log
    if(y.attributes$trans.log&!y.attributes$trans.percent){

      start.val = y.attributes$start.val
      if(is.null(start.val)) start.val = NA

      scaler = ram.scaler(dat.raw,start.val)
      y.breaks = scaler$breaks
      y.labs = scaler$labs
      dmat[] = coredata(scaler$dat)

      ## Argument: y.attributes$trans.percent
    } else if (y.attributes$trans.percent) {

      y.breaks = c(0,0.25,0.5,0.75,1)
      y.labs = paste0(comma(y.breaks* 100), "%")

    } else {

      dat.range = range(dmat,na.rm = T)
      y.breaks = c(seq(dat.range[1],dat.range[2],length.out = 5),
                   ifelse(!is.null(y.attributes$start.val),
                          y.attributes$start.val,
                          NULL))
      y.labs = round(y.breaks)

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
      y.breaks = as.numeric(pretty(sort(round(dat[,1],4))))
      y.labs = paste0(comma(y.breaks* 100), "%")
    } else if (plot.type%in%c('waterfall','transition')){
      y.breaks = c(0,0.25,0.5,0.75,1)
      y.labs = paste0(comma(y.breaks* 100), "%")
    }

    y.attributes$breaks = y.breaks
    y.attributes$labs = y.labs
    y.attributes$show.values = T

    emphasis$waterfall = ifelse(plot.type=='waterfall',T,F)

    ## This is here because it needs to call the bar function.
    if(plot.type=='waterfall') plot.type = 'bar'

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


  args.final = ram.arguments(type = 'base',user.args = list(x.attributes,y.attributes,titles,emphasis))
  x.attributes = args.final$x.attributes
  y.attributes = args.final$y.attributes
  titles = args.final$titles
  emphasis = args.final$emphasis

  ## Final Plot Call
  plot.out = getFunction(paste0('ram.',plot.type,'.plot'))
  plot.out(dat=dat,x.attributes,y.attributes,titles,emphasis)

}

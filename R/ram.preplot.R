#' @title ReSolve Plot Preparation Function
#'
#' @description Transforms input into a specific format for use with base plotting functions.
#'
#' @usage ram.preplot(dat, type)
#'
#' @param dat User input.
#' @param type The complete user list of preplot transforms:
#'
#'\enumerate{
#'  \item{'standard'}{}
#'  \item{'melt'}{ # Corresponds to the density plot}
#'  \item{'waterfall'}{}
#'  \item{'correlation'}{}
#'  \item{'pie'}{}
#'}
#'
#' @return NULL
#' @export
#'
ram.preplot = function(dat, type='standard'){

  if(type%in%c('standard','melt')){

    t1 = is.xts(dat)
    out = as.data.frame(matrix(NA,ncol=ncol(dat),nrow=nrow(dat)))
    out[] = coredata(dat)
    names(out) = names(dat)

    if(t1){
      out$idx = as.Date(index(dat))
    } else {
      out$idx = 1:nrow(dat)
    }

    if(type=='melt'){
      out = melt(out,id='idx')
    }

  } else if (type=='waterfall') {

    if(all(dim(dat)!=1)&!is.vector(dat)){
      stop('Formatting error. Expecting a comparison vector.',
           call. = FALSE, domain = NA)
    } else {

      if(ifelse(is.null(dim(dat)[1]),F,dim(dat)[1]==1)){
        dat = as.data.frame(t(dat))
      } else {
        dat = round(as.data.frame(t(t(dat))),4)
      }
      cn = rownames(dat)
    }

    names(dat)='val'
    wh = order(dat[,1],decreasing = T)
    dat[] = dat[wh,]
    dat$end = cumsum(dat$val)

    dat$begin = as.numeric(ifna(mlag(dat$end),0))
    dat = rbind(dat,c(sum(dat$val),sum(dat$val),0))
    dat$idx = 1:nrow(dat)
    dat$ids = row.names(dat) = c(cn[wh],'Portfolio')
    out = dat

  } else if (type=='correlation') {

    ## Plot: Base build
    cormat = upper.tri(dat,diag = T)
    cormat[!cormat] = NA
    cormat = cormat * dat

    # Melt the correlation matrix
    melted_cormat = melt(cor(dat))
    melted_cormat$value = round(as.numeric(as.matrix(cormat)),2)
    out = na.omit(melted_cormat)

  } else if (type=='pie') {

    out = data.frame(
      weights=as.numeric(dat)*100,
      tickers=names(dat)
    )

    out = out %>%
      mutate(tickers = factor(tickers, levels = tickers)) %>%
      arrange(desc(tickers)) %>%
      mutate(ymax = round(cumsum(weights),2),
             center = ymax - weights / 2,
             ymin = c(0,head(ymax,-1)))
  } else if (type=='bar2') {
    ids = as.character(rownames(dat))
    tmp = as.data.frame(cbind(ids,dat))
    out = melt(tmp,id.vars = 'ids')
  }

  return(out)
}

ram.theme = function(
  text.xaxis=12,
  text.yaxis=12
){

  # ## Install the font and make sure it's loaded
  # sysfonts::font_add(family= 'Helvetica Neue', regular = system.file("fonts", "HelveticaNeue.ttf", package="RAMplot"))

  theme(

    # Basic white background
    plot.title = NULL,
    panel.background = element_blank(),  # Clears defaults
    legend.position = "none", # Turns off legend by default
    legend.title = element_text(size=8),
    legend.key = element_rect(fill = 'white'),

    # Text specs
    text = element_text(family='Helvetica Neue'),
    axis.text.x = element_text(size = text.xaxis),
    axis.text.y = element_text(size = text.yaxis),
    legend.text = element_text(size = 10),

    # Axis specs
    axis.line = element_line(
      color = '#023858',  # ReSolve specific color
      size = 1.5,
      lineend = 'round'),

    # Tick specs
    axis.ticks = element_blank(),   # Clears defaults
    axis.ticks.y = element_line(
      color = 'white',  # ReSolve specific color
      size = 1.5,
      linetype = 'solid',
      lineend = 'square'
    ),
    axis.ticks.length = unit(2,'mm')

  )
}

#' @title ReSolve data preplot function
#'
#' @description Internal function which specifically formats an input for use base functions
#' @param dat Input data source
#' @param type Selection: 'standard', 'waterfall', and 'correlation' are valid options
#' @return NULL
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
    } else if (ifelse(is.null(dim(dat)[1]),F,dim(dat)[1]==1)) {
      dat = as.data.frame(t(dat))
      cn = rownames(dat)
    } else if (is.vector(dat)) {
      dat = round(as.data.frame(t(t(dat))),4)
      cn = rownames(dat)
    }

    names(dat)='val'
    dat[] = dat[order(dat[,1],decreasing = T),]
    dat$end = cumsum(dat$val)
    dat$begin = as.numeric(ifna(mlag(dat$end),0))
    dat = rbind(dat,c(sum(dat$val),sum(dat$val),0))
    dat$idx = 1:nrow(dat)
    dat$ids = row.names(dat) = c(cn,'Portfolio')
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
  }

  return(out)
}

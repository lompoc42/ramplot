#' @title ReSolve Color Function
#'
#' @description Takes a numerical or lookup input and returns color values for R
#' @param n Number of colors to output. Max is 19.
#' @param pcolor Primary Color. Default is ReSolve "dark blue."
#' @param lookup Character input to lookup ReSolve specific color by chart name.
#' @return NULL
#' @export
#'
ram.colors = function(n=1,pcolor = '#00488d',lookup = NULL){

  if(!is.null(lookup)){
    n = min(n,20)

    expand.cols = function(dm, n){
      matrix(rep(dm, n), ncol = n)
    }

    expand.rows = function(dm, n){
      t(expand.cols(dm, n))
    }
  }

  ac = c(
    dark.blue = '#00488d',
    cian = '#00abff',
    cian.tint1 = '#62c3ff',
    cian.tint2 = '#89d2ff' ,
    grey2 = '#81aec6',
    prune = '#7e768c',
    rose = '#e3745f',
    mustard = '#e6c245',
    purple = '#6f4596',
    aqua1 = '#00b08b',
    aqua2 = '#59c6b3',
    green = '#63cc77',
    orange = '#f29400',
    rich.yellow = '#fbba00',
    rich.olive.green = '#d3d800',
    green2 = '#b3d852',
    grey1 = '#293645',
    black = '#000000',
    grey3 = '#88949e',
    slate = '#698fa3'

  )

  if(is.null(lookup)){
    mat = t(col2rgb(ac))

    ## Argument: intial color
    c1 = ifelse(pcolor%in%ac,pcolor,as.character(ac[pcolor]))
    wh = which(ac%in%c1)
    b1 = sqrt(rowSums((expand.rows(mat[wh,],19) - mat[-wh,])^2))
    c2 = ac[which.max(b1)+1]
    wh = which.min(abs(b1-(b1[which.max(b1)]/2)))
    c3 = ac[wh]

    cols = c(c1,c2,c3)

    ## Spits out two colors at a time. Color list has to have even number.
    for(i in 1:8){
      b1 = mat[wh,]
      ac = ac[!ac%in%cols]
      mat = t(col2rgb(ac))

      b2 = sqrt(rowSums((expand.rows(b1,nrow(mat)) - mat)^2))
      co1 = ac[which.max(b2)+1]
      wh = which.min(abs(b2-(b2[which.max(b2)]/2)))
      co2 = ac[wh]
      cols = c(cols,co1,co2)
    }

    cols = c(cols,ac[which(!as.character(ac)%in%as.character(cols))])
    cols = head(as.character(cols),n)
  } else {
    if(!all(lookup%in%names(ac))){
      stop('No color by that name or number.', call. = FALSE, domain = NA)
    }
    cols = na.omit(as.character(ac[lookup]))
  }
  return(cols)
}

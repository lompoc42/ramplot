#' @title ReSolve Argument Check
#'
#' @description Internal function to check argument validity across plotting functions.
#' @param type selection: 'base' and 'umbrella' are the options
#' @param user.args list: user argument list to check against for validity.
#' @return NULL
#'

ram.arguments = function(type='base',user.args){

  names(user.args) = c('x.attributes','y.attributes','titles','emphasis')

  if(type=='base'){

    arg.list = arg.out = list(
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
        density = F
      )
    )

    for(arg in names(arg.list)){
      b0 = arg.list[[arg]]
      b1 = modifyList(b0,user.args[[arg]],keep.null = T)
      arg.out[[arg]] = b1[names(b0)]
    }

  } else if (type=='umbrella') {

    arg.list = arg.out = list(
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
        ring = F,
        density = F
      )
    )

    for(arg in names(arg.list)){
      b0 = arg.list[[arg]]
      b1 = modifyList(b0,user.args[[arg]],keep.null = T)
      arg.out[[arg]] = b1[names(b0)]
    }
  }
  return(arg.out)
}

#' @title ReSolve Argument Check
#'
#' @description This is the arguments module for the plotting functions. The user passes on arguments under the 'umbrella' type which are then passed to the base plot function being called. This function also serves to guard against invalid argument parameters with a few basic user cases.
#'
#' @usage ram.arguments('type', ...)
#'
#' @param type selection: 'base' and 'umbrella' are the options
#' @param ... The complete user list of arguments:
#'
#'\describe{
#'
#'  \item{\emph{x.attributes}}{
#'   \enumerate{
#'     \item{show.day = F}{}
#'     \item{breaks = NULL}{}
#'     \item{labs = NULL}{}
#'     \item{labs.tilt = F}{}
#'   }
#'  }
#'
#'  \item{\emph{y.attributes}}{
#'   \enumerate{
#'     \item{start.val = 100}{}
#'     \item{breaks = NULL}{}
#'     \item{labs = NULL}{}
#'     \item{trans.log = T}{}
#'     \item{trans.percent = F}{}
#'     \item{show.values = F}{}
#'     \item{currency = '$'}{}
#'   }
#'  }
#'
#'  \item{\emph{titles}}{
#'   \enumerate{
#'     \item{main = NULL}{}
#'     \item{subtitle = NULL}{}
#'     \item{legend.labels = NULL}{}
#'     \item{legend.rows = NULL}{}
#'     \item{caption = NULL}{}
#'     \item{x = NULL}{}
#'     \item{y = NULL}{}
#'     \item{rounding = 1}{}
#'   }
#'  }
#'
#'  \item{\emph{emphasis}}{
#'   \enumerate{
#'     \item{emph.column = NULL}{}
#'     \item{hline = NULL}{}
#'     \item{hline.size = NULL}{}
#'     \item{hline.color = NULL}{}
#'     \item{primary.column = 1}{}
#'     \item{secondary.column = NULL}{}
#'     \item{show.best.fit = NULL}{}
#'     \item{waterfall = F}{}
#'     \item{ring = F}{}
#'     \item{density = F}{}
#'   }
#'  }
#'
#'  \item{\emph{output}}{
#'   \enumerate{
#'     \item{file.name = NULL}{}
#'     \item{file.type = 'pdf'}{}
#'     \item{width = 10}{}
#'     \item{height = 5}{}
#'   }
#'  }
#'
#'}
#'
#' @return NULL
#' @export
#'
#'
ram.arguments = function(type='base',user.args){


  if(type=='base'){

    names(user.args) = c('x.attributes','y.attributes','titles','emphasis')

    arg.list = arg.out = list(
      x.attributes = list(
        breaks = NULL,
        labs = NULL,
        labs.tilt = F,
        text.labs = 12
      ),

      y.attributes = list(
        breaks = NULL,
        labs = NULL,
        show.values = F,
        text.labs = 12
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

    names(user.args) = c('x.attributes','y.attributes','titles','emphasis','output')

    arg.list = arg.out = list(
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
        text.labs = 12
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
      ),

      output = list(
        file.name = NULL,
        file.type = 'pdf',
        width = 10,
        height = 5
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

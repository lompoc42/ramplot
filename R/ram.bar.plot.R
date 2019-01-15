#' @title ReSolve Bar Plotting Function
#'
#' @description Base Plotting Functions: Bar and Waterfall
#' @usage ram.bar.plot(dat,...)
#'
#' @param dat Data is assumed to be a matrix or vector of returns.
#' @param ... Complete argument details are located in \code{\link[RAMplot:ram.arguments]{ram.arguments}}
#'
#' @examples
#'
#' ## The function will call the bar plot by default.
#' ## The waterfall plot is called as follows
#'
#' ram.bar.plot(dat,emphasis=list(waterfall=T))
#'
#' @return NULL
#' @export
#'
ram.bar.plot = function(

  dat,
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
    text.labs = 12,
    text.vals = 2
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
    waterfall = F
  )

){


  # Plot Setup --------------------------------------------------------------


  ## Waterfall plots and single item bar plots
  if(ncol(dat)==2|emphasis$waterfall){
    ## Plot: Base build
    p = ggplot(dat, aes(x=1:nrow(dat))) +
      ram.theme(
        text.xaxis = x.attributes$text.labs,
        text.yaxis = y.attributes$text.labs
      )

    ## Argument: emphasis$waterfall
    if(!emphasis$waterfall){
      cols = rep(ram.colors(1),nrow(dat))
      # Standard bar chart
      p = p + geom_rect(
        stat='identity',
        aes(
          xmin=(1:nrow(dat))-0.45,
          xmax=(1:nrow(dat))+0.45,
          ymin=ifelse(dat[,1]<0,dat[,1],0),
          ymax=ifelse(dat[,1]<0,0,dat[,1]),
          fill=as.character(idx)
        ))
    } else if(emphasis$waterfall) {
      ## Waterfall Barchart
      ## Plot: RAM colors
      if(sum(dat[-nrow(dat),1])==1){
        cols = ram.colors(lookup = c('dark.blue','cian'))
        cols = as.character(c(rep(cols[2],nrow(dat)-1),cols[1]))
        cols = cols[order(row.names(dat))]
      } else {
        v = as.numeric(dat$val[-nrow(dat)])
        cols = ram.colors(lookup = c('cian','dark.orange','dark.blue'))
        cols = as.character(c(rep(cols[1],length(v[v>=0])),rep(cols[2],length(v[v<0])),cols[3]))
        cols = cols[order(row.names(dat))]
      }

      p = p + geom_rect(
        stat='identity',
        aes(
          xmin=idx-0.45,
          xmax=idx+0.45,
          ymin=begin,
          ymax=end,
          fill=ids
        ))
    }
  } else {

    p = ggplot(dat,
               aes(x=ids,
                   fill=variable,
                   y=value,
                   label=value)) +
      ram.theme(
        text.xaxis = x.attributes$text.labs,
        text.yaxis = y.attributes$text.labs
      )

    cols = ram.colors(length(unique(dat$variable)))
    # Standard bar chart
    p = p + geom_bar(
      position = 'dodge',
      stat='identity')
  }


  # Emphasis --------------------------------------------------------------



  ## Plot: Line and color emphasis for input
  if(!is.null(emphasis$emph.column)&(ncol(dat)==2|emphasis$waterfall)){

    ## Argument: emphasis()

    # emph.column
    if(is.numeric(emphasis$emph.column)){
      ew = as.numeric(emphasis$emph.column)
    } else {
      ew = which(rownames(dat)%in%as.character(emphasis$emph.column))
    }

    # Data: modify colors and line sizes according to input
    cols[ew] = '#e6c245'

  }


  # X and Y axis ------------------------------------------------------------


  ## Argument: x.attributes()
  if(ncol(dat)==2|emphasis$waterfall){
    x.breaks = 1:nrow(dat) # Manual x.breaks not supported in barplot
  } else {
    x.breaks = x.attributes$breaks
  }

  x.labels = x.attributes$labs
  tilt = x.attributes$labs.tilt

  ## Plot: add x.attributes
  if(ncol(dat)==2|emphasis$waterfall){
    p = p +
      scale_x_continuous(breaks = x.breaks,
                         expand = c(0.05,0.025), # Gives margin
                         labels = x.labels)
  }

  ## Plot: ammend x-axis bar in case of negative bar values
  if(any(y.attributes$breaks<0)&!emphasis$waterfall){
    p = p + theme(
      axis.line.x = element_line(
        color = 'white',
        lineend = 'square'
      ))
    p = p +
      geom_hline(yintercept = 0,
                 color = '#023858',
                 size = 1.5)
  }

  ## Plot: add y.attributes
  if(ncol(dat)==2|emphasis$waterfall){
    if(sum(dat[-nrow(dat),1])==1){
      y.limits = c(0,max(dat[,1]*1.1))
      p = p +
        scale_y_continuous(
          breaks = y.attributes$breaks,
          expand = c(0,0),
          limits = y.limits,
          labels = y.attributes$labs
        )
    } else if (!emphasis$waterfall) {
      p = p +  scale_y_continuous(
        breaks = y.attributes$breaks,
        labels = y.attributes$labs
      )
    } else {
      p = p + scale_y_continuous(labels = percent)
    }
  } else {
    p = p +
      scale_y_continuous(
        labels = percent,
        limits = range(dat$value*1.1)
      )
  }

  ## Argument: update theme x.attributes$labs.tilt
  if(tilt){
    p = p +
      theme(
        axis.text.x = element_text(angle = 45, hjust=1)
      )
  }



  # Titles ----------------------------------------------------------------



  ## Plot: Titles

  ## Argument: titles()
  main.title = titles$main ## main
  main.sub = titles$subtitle ## subtitle
  main.caption = titles$caption ## caption
  x.title = titles$x ## x title
  y.title = titles$y ## y title
  show.values = y.attributes$show.values


  ## Plot: Add titles
  if(show.values){

    ## Sum to 1 portfolio use case
    if(emphasis$waterfall){
      bar.val.locs = dat$end
      bar.val.labs = paste0(round(dat$val*100,titles$rounding),'%')
    } else {
      if(ncol(dat)==2){
        bar.val.locs = dat[,1]
        bar.val.labs = paste0(round(dat[,1]*100,titles$rounding),'%')
      } else {
        bar.val.locs = dat$value
        bar.val.labs = paste0(round(dat$value*100,titles$rounding),'%')
      }
    }

    if(ncol(dat)==2|emphasis$waterfall){
      p = p + geom_text(aes(
        y=bar.val.locs,
        label=bar.val.labs),
        vjust=ifelse(dat[,1]>0,-0.5,1.35),
        # position = position_dodge(width = 0.9),
        size = y.attributes$text.vals
      )
    } else {
      p = p +
        geom_text(
          data=dat,
          position=position_dodge(width= .9),
          aes(y=value, label=bar.val.labs,vjust=ifelse(dat$value>0,-0.5,1.35))
        )

    }
  }

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

  ## Plot: Add colors and legends

  ## Argument: legend.rows
  lr = ifelse(!is.null(titles$legend.rows),titles$legend.rows,0)
  legend.labels = titles$legend.labels

  if(is.null(legend.labels)&lr>0){
    if(ncol(dat)==2){
      legend.labels = as.character(dat$ids)
    } else {
      legend.labels = as.character(unique(dat$variable))
    }
  }

  ## Plot: Add colors and legend
  if(length(legend.labels)==0){
    p = p + scale_fill_manual(values=cols)
  } else {
    p = p + scale_fill_manual(values=cols, labels = legend.labels) +
      theme(legend.position = 'top', legend.title = element_blank())
  }

  ## Plot: Add legend rows
  if(lr>1){
    p = p + guides(fill = guide_legend(nrow = lr))
  }

  return(p)

}

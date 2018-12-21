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
    waterfall = F
  )

){


  # Plot Setup --------------------------------------------------------------


  ## Plot: RAM colors
  cols = ram.colors(ncol(dat)-1)

  ## Plot: Base build
  p = ggplot(dat, aes(x=1:nrow(dat))) +
    ram.theme()

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
  } else if(emphasis$waterfall){
    ## Waterfall Barchart
    cols = ram.colors(lookup = c('dark.blue','cian'))
    cols = as.character(c(rep(cols[2],nrow(dat)-1),cols[1]))
    cols = cols[order(row.names(dat))]
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



  # Emphasis --------------------------------------------------------------



  ## Plot: Line and color emphasis for input
  if(!is.null(emphasis$emph.column)){

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
  x.breaks = 1:nrow(dat) # Manual x.breaks not supported in barplot
  x.labels = x.attributes$labs
  tilt = x.attributes$labs.tilt

  if(!emphasis$waterfall){
    if(any(dat[,1]<0)){
      y.limits = c(min(-0.05,min(dat[,1])*1.85),max(dat[,1]*1.2))
    } else {
      y.limits = c(0,max(dat[,1]*1.2))
    }
  } else if (emphasis$waterfall) {
    y.limits = c(0,max(dat[,1]*1.1))
  }

  ## Plot: add x.attributes
  p = p +
    scale_x_continuous(breaks = x.breaks,
                       expand = c(0.05,0.025), # Gives margin
                       labels = x.labels)

  ## Plot: ammend x-axis bar in case of negative bar values
  if(any(y.attributes$breaks<0)){
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
  p = p +
    scale_y_continuous(
      breaks = y.attributes$breaks,
      expand = c(0,0),
      limits = y.limits,
      labels = y.attributes$labs
    )

  ## Argument: update theme x.attributes$labs.tilt
  if(tilt){
    p = p +
      theme(
        axis.text.x = element_text(angle = 45, hjust=1)
      )
  }



  # Titles ----------------------------------------------------------------



  ## Plot: Titles
  ## Argument: legend.rows
  lr = ifelse(!is.null(titles$legend.rows),titles$legend.rows,0)

  ## Argument: titles()
  main.title = titles$main ## main
  main.sub = titles$subtitle ## subtitle
  main.caption = titles$caption ## caption
  x.title = titles$x ## x title
  y.title = titles$y ## y title
  legend.titles = as.character(titles$legend)
  show.values = y.attributes$show.values


  ## Plot: Add titles

  if(show.values){
    if(!emphasis$waterfall){
      vals = dat[,1]
      bar.val.locs = vals
      bar.val.labs = paste0(round(dat[,1]*100,titles$rounding),'%')
    } else if (emphasis$waterfall){
      bar.val.locs = dat$end
      bar.val.labs = paste0(round(dat$val*100,titles$rounding),'%')
    }

    p = p + geom_text(aes(
      y=bar.val.locs,
      label=bar.val.labs),
      vjust=ifelse(bar.val.locs>0,-0.5,1.35),
      position = position_dodge(width = 0.9))

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

  ## Plot: Add colors
  if(length(legend.titles)==0 | length(legend.titles)>2){
    leg = scale_fill_manual(values=cols)
  } else {
    leg = scale_fill_manual(values=cols, labels = legend.titles)
  }

  ## Plot: Add colors
  ### Legend currently disabled
  # theme(legend.position = 'top', legend.title = element_blank()) +

  print(
    p + leg
  )

}

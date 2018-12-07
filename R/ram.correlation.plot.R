#' @title ReSolve Correlation Heatmap Plotting Function
#'
#' @description Base plotting function.
#' @param dat Input
#' @param x.attributes List: x axis modifiers
#' @param y.attributes List: y axis modifiers
#' @param titles List: plot titles
#' @param emphasis List: emphasis arguments
#' @return NULL
#' @export
#'
ram.correlation.plot = function(

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
    show.best.fit = T,
    density = T
  )

){


  # Plot Setup --------------------------------------------------------------

  ## Plot: RAM colors
  if(!emphasis$density){
    cols = c('#89d2ff','#00488d','#62c3ff')
  } else {
    cols = ram.colors(length(unique(dat$variable)))
  }

  ## Plot: Base plot build
  if(!emphasis$density){
    p = ggplot(dat,aes(Var2, Var1, fill = value)) +
      geom_tile(color = "white")+
      scale_fill_gradient2(
        low = cols[1], high = cols[2], mid =  cols[3],
        midpoint = 0, limit = c(-1,1), name="Pearson\nCorrelation") +
      coord_fixed() +
      ram.theme() +
      geom_text(aes(Var2, Var1, label = value),
                color = "black", size = 4) +
      theme(
        legend.justification = c(1.025, -0.2),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal",
        axis.ticks.y = element_blank()
      )

  } else {
    p = ggplot(dat, aes(value, fill = variable)) +
      geom_density(alpha=(0.5)) +
      ram.theme()
  }


  # Titles and tilt -----------------------------------------------------


  ## Argument: update theme x.attributes$labs.tilt
  if(x.attributes$labs.tilt){
    p = p +
      theme(
        axis.text.x = element_text(angle = 45, hjust=1)
      )
  }

  ## Argument: titles()
  main.title = titles$main ## main
  main.sub = titles$subtitle ## subtitle
  main.caption = titles$caption ## caption
  x.title = titles$x ## x title
  y.title = titles$y ## y title

  ## Plot: Add titles and legend
  p = p +
    labs(
      title = as.character(main.title),
      subtitle = as.character(main.sub),
      caption = main.caption,
      x = as.character(x.title),
      y = as.character(y.title)
    )

  if(!emphasis$density){
    p = p + guides(fill = guide_colorbar(
      barwidth = 7,
      barheight = 1,
      title.position = "top",
      title.hjust = 0.5))
  } else {

    ## Plot: Add colors
    if(length(titles$legend.labels)==0){
      leg = scale_fill_manual(values=cols)
    } else {
      leg = scale_fill_manual(values=cols, labels = legend.labels)
    }

    p = p +
      theme(legend.position = 'top', legend.title = element_blank()) +
      leg

  }

  if(any(sapply(titles,length)!=0)){
    p = p + theme(
      plot.title = element_text(size=12, hjust=0.5),
      plot.subtitle = element_text(size=10, hjust=0.5,vjust=-1)
    )
  }

  ## Plot: Print Plot
  suppressWarnings(print(p))

}

ram.transition.plot = function(
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
  )
){



  # Colors, Line Sizes, and Legend Titles -----------------------------------

  ## Plot: Colors and line sizes
  cols = ram.colors(ncol(dat)-1)

  ## Argument: titles$legend.labels
  if(!is.null(titles$legend.labels) & length(titles$legend.labels)==(ncol(dat)-1)){
    tmp = dat[,-ncol(dat)]
    names(tmp) = titles$legend.labels
    tmp = cbind(tmp,dat[,ncol(dat)])
  }



  # Begin Plot --------------------------------------------------------------

  dat$idx = 1:nrow(dat)
  dat = melt(dat,id='idx')

  ## Plot: Build base plot
  p = ggplot(dat, aes(x=idx,y=value,fill=variable)) +
    geom_area(position = 'stack') +
    ram.theme()

  # X and Y axis ------------------------------------------------------------


  ## Argument: x.attributes()
  x.breaks = x.attributes$breaks
  x.labels = x.attributes$labs
  tilt = x.attributes$labs.tilt

  ## Plot: add x.attributes
  p = p +
    scale_x_continuous(breaks = x.breaks,
                       expand = c(0.05,0.025), # Gives margin
                       labels = x.labels)

  ## Argument: update theme x.attributes$labs.tilt
  if(tilt){
    p = p +
      theme(
        axis.text.x = element_text(angle = 45, hjust=1)
      )
  }

  ## Argument: y.attributes
  p = p + scale_y_continuous(breaks = y.attributes$breaks,
                             labels= y.attributes$labs)


  # Titles ----------------------------------------------------------------



  ## Plot: Titles
  ## Argument: legend.rows
  legend.rows = titles$legend.rows
  lr = ifelse(is.null(legend.rows),1,legend.rows)


  ## Argument: titles()
  main.title = titles$main ## main
  main.sub = titles$subtitle ## subtitle
  main.caption = titles$caption ## caption
  x.title = titles$x ## x title
  y.title = titles$y ## y title
  legend.labels = as.character(titles$legend.labels)


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

  ## Plot: Add colors
  if(length(legend.labels)==0){
    leg = scale_fill_manual(values=cols)
  } else {
    leg = scale_fill_manual(values=cols, labels = legend.labels)
  }

  ## Plot: Print Plot
  suppressWarnings(print(
    p +
      theme(legend.position = 'top', legend.title = element_blank()) +
      leg
  ))
}

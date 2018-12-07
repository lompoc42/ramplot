#' @title ReSolve Equity Plotting Function
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
ram.equity.plot = function(
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

  ## Plot: Line sizes
  line.sizes = rep(0.6,ncol(dat)-1)

  ## Argument: titles$legend.labels
  if(!is.null(titles$legend.labels) & length(titles$legend.labels)==(ncol(dat)-1)){
    tmp = dat[,-ncol(dat)]
    names(tmp) = titles$legend.labels
    tmp = cbind(tmp,dat[,ncol(dat)])
  } else if (!is.null(titles$legend.labels) & length(titles$legend.labels)!=ncol(dat)){
    warning('Attribute set to default: number of legend names must match dat',
            call. = FALSE, domain = NA)
  }



  # Begin Plot --------------------------------------------------------------



  ## Plot: Base build
  p = ggplot(dat, aes(x = 1:nrow(dat))) +
    ram.theme()

  for(i in 1:(ncol(dat)-1)){
    p = p +
      geom_line(
        aes_q(
          y=as.name(names(dat)[i]),
          color=as.character(names(dat)[i])
        ),
        size=line.sizes[i]
      )
  }



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
    leg = scale_color_manual(values=cols)
  } else {
    leg = scale_color_manual(values=cols, labels = legend.labels)
  }

  ## Plot: Print Plot
  suppressWarnings(print(
    p +
      scale_size_manual(values = line.sizes) +
      theme(legend.position = 'top', legend.title = element_blank()) +
      leg
  ))
}

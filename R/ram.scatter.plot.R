#' @title ReSolve Scatter Plotting Function
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
ram.scatter.plot = function(

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


  # Plot Setup --------------------------------------------------------------

  ## Plot: RAM colors
  cols = ram.colors(2)

  ## Argument: emphasis$primary.column
  primary.column = emphasis$primary.column
  secondary.column = emphasis$secondary.column

  ## Plot: Base build
  p = ggplot(dat, aes(x=dat[,primary.column])) +
    ram.theme() +
    geom_point(
      aes(y=comp, color=names(dat)[secondary.column]),
      size = 0.25
    )

  ## Argument: emphasis$show.best.fit
  if(emphasis$show.best.fit){
    p = p +
      geom_line(
        aes(y=`Line of Best Fit`, color = 'Line of Best Fit'),
        size = 1,
        linejoin='mitre',
        arrow = arrow(length = unit(2.5, 'mm'))
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



  ## Plot: Add equation
  if(emphasis$show.best.fit){

    l.model=lm(dat[,'comp']~dat[,primary.column])
    dat$`Line of Best Fit` = dat[,primary.column] *
      l.model$coefficients[2] +
      l.model$coefficients[1]

    lm_eqn <- function(m,d.name){
      eq <- substitute(R[italic(y)] ==b %.% R[italic(x)]+a+epsilon*"," ~~italic(r)^2~"="~r2,
                       list(a = paste0(format(coef(m)[1]*100, digits = 2),'%'),
                            b = format(coef(m)[2], digits = 2),
                            y=d.name[2],
                            x=d.name[1],
                            r2 = format(summary(m)$r.squared, digits = 3)))
      as.character(as.expression(eq))
    }

    eqn.lab = lm_eqn(l.model,names(dat)[c(primary.column,secondary.column)])
    eqn.df = data.frame(x = mean(range(dat[,primary.column])), y = max(range(dat[,'comp']))+.01, lab = eqn.lab)

    p = p + geom_text(data=eqn.df,aes(x=x,y=y,label=lab),parse = T)
  }

  ## Plot: Titles
  ## Argument: titles$legend.rows
  lr = ifelse(is.null(titles$legend.rows),1,titles$legend.rows)

  ## Argument: titles()
  main.title = titles$main ## main
  main.sub = titles$subtitle ## subtitle
  main.caption = titles$caption ## caption
  x.title = titles$x ## x title
  y.title = titles$y ## y title
  legend.titles = as.character(titles$legend)

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


  # Emphasis --------------------------------------------------------------


  #### EMPHASIS NOT SUPPORTED IN THIS FUNCTION
  # ## Plot: Line and color emphasis for input
  # if(!is.null(emphasis$emph.column)){
  #
  #   ## Argument: emphasis()
  #
  #   # emph.column
  #   if(is.numeric(emphasis$emph.column)){
  #     ew = as.numeric(emphasis$emph.column)
  #   } else {
  #     ew = which(colnames(dat)%in%as.character(emphasis$emph.column))
  #   }
  #
  #   # emph.color
  #   ec = ifelse(is.null(emphasis$emph.color),cols[ew],emphasis$emph.color)
  #
  #   # Data: modify colors and line sizes according to input
  #   cols[ew] = ec
  #   line.sizes[ew] = 0.9
  #
  # }


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

  GeomPath$draw_key = draw_key_line

  if(length(legend.titles)==0 | length(legend.titles)>2){
    leg = scale_color_manual(values=cols)
  } else {
    leg = scale_color_manual(values=cols, labels = legend.titles)
  }

  ## Plot: Add colors
  print(
    p +
      scale_size_manual(values = line.sizes) +
      theme(legend.position = 'top', legend.title = element_blank()) +
      leg
  )

  GeomPath$draw_key = draw_key_path

}

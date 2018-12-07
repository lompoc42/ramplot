#' @title ReSolve Data Standardizing Function
#'
#' @description Takes an input and conforms it to RAM standards
#' @param dat Input data source
#' @return NULL
#' @export
#'
ram.dat.standard = function(dat){

  ## Inner functions.
  ## Re-formats ReSolve database date formats into R-friendliness
  ram.daterip = function(x){
    daterip = as.Date(paste(
      # year
      substr(x, 1, 4),
      # month
      substr(x, 5, 6),
      # day
      substr(x, 7, 8),
      sep = "-"))
    return(daterip)
  }

  ## Takes an input and attempts to convert into a date.
  ram.date.out = function(dat){

    options(warn=-1)
    ## Strikeout conditions. Any of these and the function fails.
    # Nothing there
    c0 = is.null(dat)
    # Data problem
    c1 = inherits(try(as.Date(as.numeric(dat)),T),'try-error')
    # Data is prices
    c2 = ifelse(c0|c1,F,ifelse('.'%in%strsplit(as.character(dat[1]),'')[[1]],T,F))

    if(c0|c1|c2){
      # No date data detected.
      out = NULL
    } else {

      ## Conditions for standard date formats
      c1 = !all(is.na(as.character(strptime(dat, "%Y-%m-%d %H:%M"))))
      c2 = all(nchar(as.character(dat))==10)
      c3 = !inherits(try(ram.daterip(dat),T),'try-error')

      ## Attempt to extract time data
      if(c1){
        # Is minute data
        out = strptime(dat, "%Y-%m-%d %H:%M")
      } else if (c2) {
        # Standard date data
        out = as.Date(dat)
      } else if (c3) {
        # ReSolve formatted dates
        out = ram.daterip(dat)
      } else {
        # No date data detected.
        out = NULL
      }
    }
    options(warn=0)
    return(out)
  }

  if(is.vector(dat)){

    #### Yes data is a vector.

    ## This attempts to convert the vector names into dates
    idx = ram.date.out(names(dat))
    if(!is.null(idx)){
      out = xts(as.numeric(dat),idx)
    } else {
      out = as.data.frame(as.numeric(dat))
    }

    ### Data is not a vector. Is it a matrix or dataframe?
  } else if (is.matrix(dat) | is.data.frame(dat)){

    ### Make sure dat is not already in acceptable xts format.
    xts.try = try(try.xts(dat),T)

    if(inherits(xts.try,'try-error')){

      ## First let's put it all on even ground.
      dat = as.data.frame(dat)
      cnames = colnames(dat)

      ## Try to identify dates in the columns.
      dates.try = sapply(dat,function(x){!is.null(ram.date.out(x))})

      if(length(which(dates.try))>0){

        ## Dates found in columns
        wh = first(which(dates.try))
        idx = ram.date.out(dat[,wh])
        out = xts(coredata(dat[,-wh]),idx)
        colnames(out) = cnames[-wh]

      } else {
        ## No dates found in columns. Try rownames.
        idx = ram.date.out(rownames(dat))
        if(!is.null(idx)){
          # Dates found in rownames
          out = xts(coredata(dat),idx)
        } else {
          # No dates found. Game over.
          out = as.data.frame(dat)
        }
      }
    } else {
      ## Data is already formatted. Move right along, please.
      out = xts.try
    }
    return(out)

  } else {

    ### Data is not in an acceptable format. Good day, sir.
    stop('Data must be a vector, matrix, or data frame.', call. = FALSE, domain = NA)
  }
}
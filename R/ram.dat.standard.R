#' @title ReSolve Data Standarizing Function
#'
#' @description Transforms input into a standardized form for use with ReSolve specific functions. It is primarily looking for time series data within the input which will return an xts object. If none is found the function will utilize a data.frame instead. If no usable data is found in the input, NA is returned.
#'
#' @usage ram.dat.standard(dat)
#'
#' @param dat User input.
#' @return NULL
#' @export
#'
ram.dat.standard = function(dat){

  print('starting')

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
      c2 = all(nchar(as.character(dat))==10 | nchar(as.character(dat))==8)
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

    print('vector detected')

    #### Yes data is a vector.

    ## This attempts to convert the vector names into dates
    idx = ram.date.out(names(dat))
    if(!is.null(idx)){
      out = xts(as.numeric(dat),idx)
    } else {
      cn = names(dat)
      if(length(unique(names(dat)))==length(dat)){
        out = as.data.frame(t(as.numeric(dat)))
        names(out) = names(dat)
      } else {
        out = as.data.frame(as.numeric(dat))
        names(out) = 'values'
      }
    }

    ### Data is not a vector. Is it a matrix or dataframe?
  } else if (is.matrix(dat) | is.data.frame(dat)){

    print('mat or df detected')

    ### Make sure dat is not already in acceptable xts format.
    xts.try = try(try.xts(dat),T)

    if(inherits(xts.try,'try-error')){

      print('dat is not an xts')

      ## First let's put it all on even ground.
      dat = as.data.frame(dat)
      cnames = colnames(dat)

      ## Try to identify dates in the columns.
      print('testing columns for dates')

      dates.try = !sapply(lapply(dat,ram.date.out),is.null)

      if(length(which(dates.try))>0){

        print('dates found in columns')

        ## Dates found in columns
        wh = first(which(dates.try))
        idx = ram.date.out(dat[,wh])
        out = xts(coredata(dat[,-wh]),idx)
        colnames(out) = cnames[-wh]

      } else {
        print('Last chances')

        ## No dates found in columns. Try rownames.
        idx = ram.date.out(rownames(dat))
        if(!any(sapply(idx,is.na))&!is.null(idx)){
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

  } else {

    ### Data is not in an acceptable format. Good day, sir.
    stop('Data must be a vector, matrix, or data frame.', call. = FALSE, domain = NA)
  }

  print('finishing up')
  cn = names(dat)
  if(is.xts(out)){
    print('out is an xts object')
    tmp = as.data.frame(out)
    tmp = tmp[,cn[cn%in%names(tmp)],drop=FALSE]
    out = xts(tmp,index(out))
  } else {
    print('out is NOT an xts object')
    out = out[,cn[cn%in%names(out)],drop=FALSE]
  }

  return(out)

}

#' @title ReSolve Plot Preparation Function
#'
#' @description Transforms input into a specific format for use with base plotting functions.
#'
#' @usage ram.preplot(dat, type)
#'
#' @param dat User input.
#' @param type The complete user list of preplot transforms:
#'
#'\enumerate{
#'  \item{'standard'}{}
#'  \item{'melt'}{ # Corresponds to the density plot}
#'  \item{'waterfall'}{}
#'  \item{'correlation'}{}
#'  \item{'pie'}{}
#'  \item{'efmap'}{}
#'  \item{'ef'}{}
#'}
#'
#' @return NULL
#' @export
#'
ram.preplot = function(dat, type='standard',ef.order='sharpe',ef=list()){

  if(type%in%c('standard','melt')){

    t1 = is.xts(dat)
    out = as.data.frame(matrix(NA,ncol=ncol(dat),nrow=nrow(dat)))
    out[] = coredata(dat)
    names(out) = names(dat)

    if(t1){
      out$idx = as.Date(index(dat))
    } else {
      out$idx = 1:nrow(dat)
    }

    if(type=='melt'){
      out = melt(out,id='idx')
    }

  } else if (type=='waterfall') {

    if(all(dim(dat)!=1)&!is.vector(dat)){
      stop('Formatting error. Expecting a comparison vector.',
           call. = FALSE, domain = NA)
    } else {

      if(ifelse(is.null(dim(dat)[1]),F,dim(dat)[1]==1)){
        dat = as.data.frame(t(dat))
      } else {
        dat = round(as.data.frame(t(t(dat))),4)
      }
      cn = rownames(dat)
    }

    names(dat)='val'
    wh = order(dat[,1],decreasing = T)
    dat[] = dat[wh,]
    dat$end = cumsum(dat$val)

    dat$begin = as.numeric(ifna(mlag(dat$end),0))
    dat = rbind(dat,c(sum(dat$val),sum(dat$val),0))
    dat$idx = 1:nrow(dat)
    dat$ids = row.names(dat) = c(cn[wh],'Portfolio')
    out = dat

  } else if (type=='correlation') {

    ## Plot: Base build
    cormat = upper.tri(dat,diag = T)
    cormat[!cormat] = NA
    cormat = cormat * dat

    # Melt the correlation matrix
    melted_cormat = melt(cor(dat))
    melted_cormat$value = round(as.numeric(as.matrix(cormat)),2)
    out = na.omit(melted_cormat)

  } else if (type=='pie') {

    out = data.frame(
      weights=as.numeric(dat)*100,
      tickers=names(dat)
    )

    out = out %>%
      mutate(tickers = factor(tickers, levels = tickers)) %>%
      arrange(desc(tickers)) %>%
      mutate(ymax = round(cumsum(weights),2),
             center = ymax - weights / 2,
             ymin = c(0,head(ymax,-1)))

  } else if (type=='bar2') {

    ids = as.character(rownames(dat))
    tmp = as.data.frame(cbind(ids,dat))
    out = melt(tmp,id.vars = 'ids')
    out$ids = factor(out$ids,levels = ids)

  } else if (type=='efmap') {

    #Create ia object Using Pairwise Complete
    n.samples=200
    if(all(dat>0)) dat = ifna(dat/mlag(dat)-1,0)
    ia=create.ia(dat)
    n = ia$n

    # 0 <= x.i <= 0.8
    constraints = new.constraints(n, lb = 0, ub = 1)

    # SUM x.i = 1 ( total portfolio weight = 100%)
    constraints = add.constraints(rep(1, n), 1, type = '=', constraints)

    # create efficient frontier consisting of 100 portfolios
    EF.mat = do.call(rbind,lapply(1:n.samples,function(x){
      ia=create.ia(MASS::mvrnorm(252,ia$expected.return,ia$cov))
      ef = portopt(ia, constraints, 100, 'Sample Efficient Frontier')
      if(grepl("SHARPE",toupper(ef.order))) metric =ef$return/ef$risk
      if(grepl("RISK",toupper(ef.order))) metric =ef$risk
      if(grepl("RETURN",toupper(ef.order))) metric =ef$return
      w=ef$weight
      cbind(metric,rank(metric),w)
    }))
    colnames(EF.mat) = c("Metric","Rank",colnames(dat))
    metric.dist = EF.mat[,"Metric"]
    EF.mat=EF.mat[,-1]
    EF.mat=EF.mat[order(EF.mat[,1]),]
    EF.mat[,"Rank"]=metric.dist[order(metric.dist)]
    EF.mat[]=apply(EF.mat,2,function(x) SMA(x,n.samples))
    EF.mat=na.omit(EF.mat)
    EF.mat=EF.mat[seq(1,nrow(EF.mat),n.samples/2),]

    out = melt(EF.mat, id ='Rank')
    out$Rank=EF.mat[,"Rank"]
    out=out[-which(out$Var2=="Rank"),]
  } else if (type=='ef') {
    #Create ia object Using Pairwise Complete
    if(!is.null(dat)){
      ret = ifna(dat/mlag(dat)-1,0)
    } else {
      ret=NULL
    }

    #Annual Factor
    if(is.null(ef$scale)){
      if(is.xts(ret)){
        af=compute.annual.factor(ret)
      } else {
        af=1
      }
    } else {
      af = 1
    }

    if(is.null(ef$scale)){
      if(!is.null(ret)){
        if(is.xts(ret)){
          af=compute.annual.factor(ret)
        } else {
          af=1
        }
      }
    } else {
      af=ef$scale
    }

    if(!is.null(ef$custom.mean)&is.null(ef$custom.covar)){
      ef$custom.covar = cov(ret)
    }
    if(is.null(ef$custom.mean) & !is.null(ef$custom.covar)){
      ef$custom.mean = colMeans(ret)
    }
    if(!is.null(ef$custom.mean) & !is.null(ef$custom.covar)){
      ret = MASS::mvrnorm(n = af*ef$t.horizon, mu=ef$custom.mean/af, Sigma=ef$custom.covar/af, tol = 1e-06, empirical = TRUE, EISPACK = FALSE)
    }
    if(is.null(ef$custom.mean) & is.null(ef$custom.covar)){
      ret = MASS::mvrnorm(n = af*ef$t.horizon, mu=colMeans(ret), Sigma=cov(ret), tol = 1e-06, empirical = TRUE, EISPACK = FALSE)

    }

    #Create ia object
    ia=create.ia(ret)
    n = ia$n
    #Define Constraint
    constraints=constraint.pos(ia)

    if(!ef$resampled){
      #Create Efficient Frontier:Corner Portfolios
      ia.tmp=ia
      ia.tmp$expected.return=ia.tmp$expected.return-ef$rf/af
      #ef.inner = portopt(ia.tmp, constraints, 1000, 'Sample Efficient Frontier')
      retForecast = ia.tmp$expected.return
      covMat=ia.tmp$cov
      tmp = CCLA(covMat, retForecast, maxIter = 100, verbose = FALSE, scale = 252, weightLimit = 1, volThresh = 0)[[1]]
      tmp = tmp[seq(nrow(tmp),1,-1),]

      Ws = do.call(rbind,lapply(2:nrow(tmp),function(j){
        w1 = tmp[j-1,names(ia$risk)]
        w2 = tmp[j,names(ia$risk)]
        t(sapply(seq(0,1,.01),function(a){
          w1*(1-a)+w2*a
        }))
      }))
      ef.inner=list()
      #idx=seq(1,nrow(Ws),(nrow(Ws)-1)/99)
      ef.inner$risk=sqrt(apply(Ws,1,function(x) x%*%ia$cov%*%x)) #covMat
      ef.inner$return=apply(Ws,1,function(x) x%*%ia$expected.return) #retForecast
    } else {
      out = mclapply(1:ef$n.samples,function(i){
        set.seed(i)
        ret.r = MASS::mvrnorm(nrow(ret),ia$expected.return,ia$cov)
        ia.tmp = create.ia(ret.r)
        ia.tmp$expected.return = ia.tmp$expected.return-ef$rf/af
        lambdas =c(10^-4,seq(0.1,1,.1),seq(1,100))
        ws =rbind(t(sapply(lambdas,function(x){
          optim.mean.variance.portfolio(lambda=x)(ia.tmp,constraints)
        }) ),max.return.portfolio(ia.tmp,constraints))
        return(ws)
      },mc.cores=detectCores() )
      out = Reduce("+",out)/length(out)

      risks = sqrt(apply(out,1,function(x) x%*%ia$cov%*%x))
      returns = apply(out,1,function(x) sum(x*ia$expected.return))
      ef.inner=list()
      ef.inner$risk=risks#*sqrt(af)
      ef.inner$return=returns#*af
    }
    idx=which.min(sqrt(diag(ia$cov)))
    w = ia$risk*0
    w[idx]=1
    w.minvar = min.var.portfolio(ia,constraints)

    weights=t(sapply(seq(0,1,0.01),function(a){
      (1-a)*w+a*w.minvar
    }))
    e.return = apply(weights,1,function(x) sum(x*ia$expected.return))
    e.risk = apply(weights,1,function(x) sqrt(x%*%ia$cov%*%x))

    idx=order(ef.inner$risk)
    ef.inner$risk=ef.inner$risk[idx]
    ef.inner$return = ef.inner$return[idx]
    ef.inner$risk=c(e.risk,ef.inner$risk)
    ef.inner$return=c(e.return,ef.inner$return)
    ef.inner$risk=ef.inner$risk*sqrt(af)
    ef.inner$return=ef.inner$return*af

    ef.inner$risk = na.approx(unlist(lapply(ef.inner$risk,function(x) c(x,rep(NA,100)))))
    ef.inner$return = na.approx(unlist(lapply(ef.inner$return,function(x) c(x,rep(NA,100)))))
    dat.frame = cbind(ef.inner$risk,ef.inner$return)
    colnames(dat.frame)=c("Risk","Return")
    out=list(dat = as.data.frame(dat.frame),ia = ia,af=af)
  }

  return(out)
}

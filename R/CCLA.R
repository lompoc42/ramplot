#' @title ReSolve Plot Preparation Function
#'
#' @description CCLA function.
#'
#' @usage CCLA(...)
#' @param ... User input.
#'
#' @return NULL
#'
CCLA = function(covMat, retForecast, maxIter = 10000,
                verbose = FALSE, scale = 252,
                weightLimit = .7, volThresh = .08) {

  #initialize original out/in/up status
  if(length(weightLimit) == 1) {
    weightLimit <- rep(weightLimit, ncol(covMat))
  }
  rankForecast <- length(retForecast) - rank(retForecast,ties.method="random") + 1
  remainingWeight <- 1 #have 100% of weight to allocate
  upStatus <- inStatus <- rep(0, ncol(covMat))
  i <- 1
  while(remainingWeight > 0) {
    securityLimit <- weightLimit[rankForecast == i]
    if(securityLimit < remainingWeight) {
      upStatus[rankForecast == i] <- 1 #if we can't invest all remaining weight into the security
      remainingWeight <- remainingWeight - securityLimit
    } else {
      inStatus[rankForecast == i] <- 1
      remainingWeight <- 0
    }
    i <- i + 1
  }

  #initial matrices (W, H, K, identity, negative identity)
  covMat <- as.matrix(covMat)
  retForecast <- as.numeric(retForecast)
  init_W <- cbind(2*covMat, rep(-1, ncol(covMat)))
  init_W <- rbind(init_W, c(rep(1, ncol(covMat)), 0))
  H_vec <- c(rep(0, ncol(covMat)), 1)
  K_vec <- c(retForecast, 0)
  negIdentity <- -1*diag(ncol(init_W))
  identity <- diag(ncol(init_W))
  matrixDim <- nrow(init_W)
  weightLimMat <- matrix(rep(weightLimit, matrixDim), ncol=ncol(covMat), byrow=TRUE)

  #out status is simply what isn't in or up
  outStatus <- 1 - inStatus - upStatus

  #initialize expected volatility/count/turning points data structure
  expVol <- Inf
  lambda <- 1000
  count <- 0
  turningPoints <- list()
  # sharpeWeights <- list()
  while(lambda > 0 & count < maxIter) {

    #old lambda and old expected volatility for use with numerical algorithms
    oldLambda <- lambda
    oldVol <- expVol

    count <- count + 1

    #compute W, A, B, in ratio, and up ratio (in Kwan.xls, called Ratio(1) and Ratio(2))
    inMat <- matrix(rep(c(inStatus, 1), matrixDim), nrow = matrixDim, byrow = TRUE)
    upMat <- matrix(rep(c(upStatus, 0), matrixDim), nrow = matrixDim, byrow = TRUE)
    outMat <- matrix(rep(c(outStatus, 0), matrixDim), nrow = matrixDim, byrow = TRUE)

    W <- inMat * init_W + upMat * identity + outMat * negIdentity

    #in R, the * operator is an element by element multiplication
    #the %*% is the matrix multiplication operator
    inv_W <- solve(W)
    modified_H <- H_vec - rowSums(weightLimMat* upMat[,-matrixDim] * init_W[,-matrixDim])
    A_vec <- inv_W %*% modified_H
    B_vec <- inv_W %*% K_vec

    #remove the last elements from A and B vectors
    truncA <- A_vec[-length(A_vec)]
    truncB <- B_vec[-length(B_vec)]

    #compute in Ratio (aka Ratio(1) in Kwan.xls)
    inRatio <- rep(0, ncol(covMat))
    inRatio[truncB > 0] <- -truncA[truncB > 0]/truncB[truncB > 0]

    #compute up Ratio (aka Ratio(2) in Kwan.xls)
    upRatio <- rep(0, ncol(covMat))
    upRatioIndices <- which(inStatus==TRUE & truncB < 0)
    if(length(upRatioIndices) > 0) {
      upRatio[upRatioIndices] <- (weightLimit[upRatioIndices] - truncA[upRatioIndices]) / truncB[upRatioIndices]
    }

    #find lambda -- max of up and in ratios
    maxInRatio <- max(inRatio)
    maxUpRatio <- max(upRatio)
    lambda <- max(maxInRatio, maxUpRatio)

    #compute new weights
    wts <- inStatus*(truncA + truncB * lambda) + upStatus * weightLimit + outStatus * 0

    #compute expected return and new expected volatility
    expRet <- t(retForecast) %*% wts
    expVol <- sqrt(wts %*% covMat %*% wts) * sqrt(scale)

    #create turning point data row and append it to turning points
    turningPoint <- cbind(count, expRet, lambda, expVol, t(wts))
    colnames(turningPoint) <- c("CP", "Exp. Ret.", "Lambda", "Exp. Vol.", colnames(covMat))
    turningPoints[[count]] <- turningPoint

    if(oldVol == Inf & expVol < volThresh) {
      turningPoints <- do.call(rbind, turningPoints)
      threshWts <- tail(turningPoints, 1)
      return(list(turningPoints, threshWts))
    } else if(oldVol > volThresh & expVol < volThresh) {
      upLambda <- oldLambda
      dnLambda <- lambda
      meanLambda <- (upLambda + dnLambda)/2
      while(upLambda - dnLambda > .00001) {

        #compute mean lambda and recompute weights, expected return, and expected vol
        meanLambda <- (upLambda + dnLambda)/2
        wts <- inStatus*(truncA + truncB * meanLambda) + upStatus * weightLimit + outStatus * 0
        expRet <- t(retForecast) %*% wts
        expVol <- sqrt(wts %*% covMat %*% wts) * sqrt(scale)

        #if new expected vol is less than threshold, mean becomes lower bound
        #otherwise, it becomes the upper bound, and loop repeats
        if(expVol < volThresh) {
          dnLambda <- meanLambda
        } else {
          upLambda <- meanLambda
        }
      }

      #once the binary search completes, return those weights, and the corner points
      #computed until the binary search. The corner points aren't used anywhere, but they're there.
      threshWts <- cbind(count, expRet, meanLambda, expVol, t(wts))
      colnames(turningPoint) <- colnames(threshWts) <- c("CP", "Exp. Ret.", "Lambda", "Exp. Vol.", colnames(covMat))
      turningPoints[[count]] <- turningPoint
      turningPoints <- do.call(rbind, turningPoints)
      return(list(turningPoints, threshWts))
    }

    #this is only run for the corner points during which binary search doesn't take place
    #change status of security that has new lambda
    if(maxInRatio > maxUpRatio) {
      inStatus[inRatio == maxInRatio] <- 1 - inStatus[inRatio == maxInRatio]
      upStatus[inRatio == maxInRatio] <- 0
    } else {
      upStatus[upRatio == maxUpRatio] <- 1 - upStatus[upRatio == maxUpRatio]
      inStatus[upRatio == maxUpRatio] <- 0
    }
    outStatus <- 1 - inStatus - upStatus
  }

  #we only get here if the volatility threshold isn't reached
  #can actually happen if set sufficiently low
  turningPoints <- do.call(rbind, turningPoints)

  #sharpeWeights <- do.call(rbind, sharpeWeights)
  threshWts <- tail(turningPoints, 1)

  #change this to turningPoints, sharpeWeights if you want to use golden Sharpe search
  return(list(turningPoints, threshWts))
}

draw_key_line <- function(data, params, size) {
  data$linetype[is.na(data$linetype)] <- 0

  grid::segmentsGrob(0.1, 0.5, 0.9, 0.5,
                     gp = grid::gpar(
                       col = scales::alpha(data$colour, data$alpha),
                       lwd = data$size * .pt,
                       lty = data$linetype,
                       lineend = "butt"
                     )
  )
}

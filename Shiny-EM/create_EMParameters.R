create_EMParameters <- function(data, numModes){
  
  #data <- vroom::vroom("sim-data.csv", delim = ",")
  #numModes <- 3
  
  
  # Sort the data and get initial estimate of stats
  x <- sort(data$x)
  if(numModes == 1){
    xList <- list(x)
  } else {
    xList <- split(x, cut(seq_along(x), numModes, labels = FALSE))  
  } 
  
  muList <- sapply(xList, mean)
  sdList <- sapply(xList, sd)
  piList <- sapply(xList, length) / length(x)
  
  # Create function to only consider finite values
  #   If not used the log(pi) + log(dnorm) can get extremely small
  sum.finite <- function(x) {
    sum(x[is.finite(x)])
  }
  
  Q <- 0 
  # starting value of expected value of the log likelihood
  
  mapList <- mapply(dnorm, mean = muList, sd = sdList, MoreArgs = list(x = x))
  Q[2] <- sum.finite(log(piList) + log(mapList))
  
  #zList <- Map(function( x, mu, sd, pi) sum.finite(log(pi) + log(dnorm(x, mean = mu, sd = sd))), 
  #             x, muList, sdList, piList)
  #Q[2] <- Reduce("+", zList)
  
  #Q[2] <- sum.finite(log(piList[1])+log(dnorm(x, muList[1], sdList[1]))) + sum.finite(log(piList[2])+log(dnorm(x, muList[2], sdList[2])))
  
  k <- 2
  
  # Create Dataframe with dynamic number of columns (i.e. numModes for each statistics)
  colPreffixes <- c("pi.","mu.","sd.")
  colSuffixes <- 1:numModes
  dfOut <- data.frame(matrix(ncol = 3 * numModes + 1, nrow = 0))
  dfOut <- rbind(dfOut, 
                 cbind(
                   data.frame(t(sapply(piList, c))), 
                   data.frame(t(sapply(muList, c))), 
                   data.frame(t(sapply(sdList, c))),
                   Q[2]
                 )
  )
  colnames(dfOut) <- c(paste(rep(colPreffixes, each = numModes), colSuffixes, sep = ""),"Like")
  
  #dfOut <- data.frame(pi.1 = pi1, pi.2 = pi2, mu.1 = mu1, mu.2 = mu2, sd.1 = sigma1, sd.2 = sigma2)
  if(numModes > 1){
    while (abs(Q[k]-Q[k-1])>=1e-4) {
      # E step
      #comp1 <- pi1 * dnorm(x, mu1, sigma1)
      #comp1 <- piList[1] * dnorm(x, muList[1], sdList[1])
      #comp2 <- pi2 * dnorm(x, mu2, sigma2)
      #comp2 <- piList[2] * dnorm(x, muList[2], sdList[2])
      #comp.sum <- comp1 + comp2
      
      
      compList <- piList * mapply(dnorm, mean = muList, sd = sdList, MoreArgs = list(x = x))
      compList.sum <- rowSums(compList)
      pList <- compList/compList.sum
      piList <- apply(pList, 2, sum)/length(x)
      
      #p1 <- comp1/comp.sum
      #p2 <- comp2/comp.sum
      
      # M step
      #pi1 <- sum.finite(p1) / length(x)
      #pi2 <- sum.finite(p2) / length(x)
      
      #mu1 <- sum.finite(p1 * x) / sum.finite(p1)
      #mu2 <- sum.finite(p2 * x) / sum.finite(p2)
      muList <- apply(pList * x, 2, sum.finite) / apply(pList, 2, sum.finite)
      
      #sigma1 <- sqrt(sum.finite(p1 * (x-mu1)^2) / sum.finite(p1))
      #sigma2 <- sqrt(sum.finite(p2 * (x-mu2)^2) / sum.finite(p2))
      sdList <- sqrt(
        apply(pList * (replicate(numModes, x) - t(replicate(length(x), unname(muList))))^2, 
              2, 
              sum.finite)
        / apply(pList, 2, sum.finite))
      
      dfTempOut <- cbind(
        data.frame(t(sapply(piList, c))), 
        data.frame(t(sapply(muList, c))), 
        data.frame(t(sapply(sdList, c))),
        Q[k]
      )
      colnames(dfTempOut) <- c(paste(rep(colPreffixes, each = numModes), colSuffixes, sep = ""), "Like")
      dfOut <- rbind(dfOut, dfTempOut)
      
      #p1 <- pi1 
      #p2 <- pi2
      
      k <- k + 1
      
      Q[k] <- sum(log(compList.sum))
      
    }
  }
  
  dfOut
} 

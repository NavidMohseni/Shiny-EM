create_plots <- function(data, numModes, dfEMParams, numBins, numEMStep, variables){
  
  #### Section Used for Testing ######################
  #data <- vroom::vroom("sim-data.csv", delim = ",")
  #numBins <- 30
  #numEMStep <- 1
  #
  ########dfEMParams for 1 mode
  #dfEMParams <- data.frame(pi.1 = 1,
  #                         mu.1 = 2,
  #                         sd.1 = 1.5)
  ########dfEMParams for 2 modes
  #dfEMParams <- data.frame(pi.1 = 0.5, pi.2 = 0.5,
  #                         mu.1 = 1.02, mu.2 = 4.0,
  #                         sd.1 = 0.81, sd.2 = 0.97)
  ########dfEMParams for 3 modes
  #dfEMParams <- data.frame(pi.1 = 0.33, pi.2 = 0.33, pi.3 = 0.34,
  #                         mu.1 = 0.59, mu.2 = 2.41, mu.3 = 4.53,
  #                         sd.1 = 0.65, sd.2 = 0.65, sd.3 = 0.65)
  ########################################3
  library(dplyr)
  x <- sort(data$x)
  hist(x, breaks = numBins, freq = FALSE)
  
  initialStep <- 1
  currentStep <- max(numEMStep, 1)
  lastStep <- nrow(dfEMParams)
  
  pidf <- dfEMParams %>% select(starts_with("pi"))
  mudf <- dfEMParams %>% select(starts_with("mu"))
  sddf <- dfEMParams %>% select(starts_with("sd"))
  
  
  calc_y <- function(numStep){
    
    ############USE FOR TESTING
    #numStep <- numEMStep
    ############################3  
    
    piS <- matrix(t(replicate(NROW(x), unlist(pidf[numStep,]))), ncol = numModes)
    tempNorms <- norms <- mapply(dnorm, mean = mudf[numStep,], sd = sddf[numStep,], MoreArgs = list(x = x))
    norms <- piS * tempNorms
    
    rowSums(norms)
    
    ##### Previous Code #################
    # Remove when done testing##########
    #dfEMParams$pi.1[numStep] * dnorm(x, 
    #                                 mean = dfEMParams$mu.1[numStep], 
    #                                 sd = dfEMParams$sd.1[numStep]) +
    #  dfEMParams$pi.2[numStep] * dnorm(x, 
    #                                   mean = dfEMParams$mu.2[numStep], 
    #                                   sd = dfEMParams$sd.2[numStep])
  }
  #calc_y(initialStep)
  ##############################################
  
  points(x, calc_y(currentStep), type = "l", lwd = 2, lty = 2, col = "black")
  
  if (!is.null(variables) ){
    if ("init" %in% variables){
      points(x, calc_y(initialStep), type = "l", lwd = 2, lty = 2, col = "royalblue")  
    }
    if ("final" %in% variables){
      points(x, calc_y(lastStep), type = "l", lwd = 2, lty = 1, col = "red")  
    }
    if("legend" %in% variables){
      strCurrentStep <- paste("Step: ", currentStep)
      legend("topleft", legend=c("Initial", strCurrentStep, "Final"),
             col=c("royalblue", "black","red"), lty=c(2, 2, 1), cex=0.8)
    }
  }
}

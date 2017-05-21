inferenceForecastRecursion <- function(mi0, sigma0, mi1, sigma1, p00, p11, fi, values){
  
  # Inititalize empty vectors
  forecastProbs0 <- c()
  inferenceProbs0 <- c()
  
  # Append fi as the first forecast value
  forecastProbs0 <- c(forecastProbs0, fi) 
  
  # Calculate first inference using fi an append it
  inf0 <- (dnorm(values[1], mi0, sigma0)*fi) / ((dnorm(values[1], mi0, sigma0)*fi) +
                                                  dnorm(values[1], mi1, sigma1)*(1-fi))
  inferenceProbs0 <- c(inferenceProbs0, inf0)
  
  # Loop trough all values and recursively calculate inferences and forecasts
  for (i in 2:length(values)){
    
    lastInf <- inferenceProbs0[i-1]
    
    for0 <- p00*lastInf + (1 - p11)*(1 - lastInf)
    forecastProbs0 <- c(forecastProbs0, for0)
    
    lastFor <- forecastProbs0[i]
    inf0 <- (dnorm(values[i], mi0, sigma0)*lastFor) / ((dnorm(values[i], mi0, sigma0)*lastFor) +
                                                         dnorm(values[i], mi1, sigma1)*(1-lastFor))
    inferenceProbs0 <- c(inferenceProbs0, inf0)
  }

  smoothedInfProbs0 <- c(last(inferenceProbs0)) # The last smoothed inference probabilites are the same
  
  # Smooth the inferences
  for (i in 2:length(inferenceProbs0)) {
    smooth0 <- tail(inferenceProbs0, i)[1]*
      (p00*smoothedInfProbs0[i-1]/(tail(forecastProbs0, i-1)[1]) +
         (1-p11)*(1-smoothedInfProbs0[i-1])/(1 - tail(forecastProbs0, i-1)[1]))
    smoothedInfProbs0 <- c(smoothedInfProbs0, smooth0)
  }
  
  return(list(forecasts=forecastProbs0, inferences=inferenceProbs0, smoothedInferences=rev(smoothedInfProbs0)))
}

logLikelihood <- function(mi0, sigma0, mi1, sigma1, values, forecastProbs){
  return(sum(log(dnorm(values, mi0, sigma0)*forecastProbs + dnorm(values, mi1, sigma1)*(1-forecastProbs))))
}

maximizeParameters <- function(mi0, sigma0, mi1, sigma1, p00, p11, fi, values, infProbs, forProbs, smoothedInfProbs){
  mi0_ <- sum(smoothedInfProbs*values)/sum(smoothedInfProbs)
  mi1_ <- sum((1-smoothedInfProbs)*values)/sum(1-smoothedInfProbs)
  sigma0_ <- sqrt(sum(smoothedInfProbs*((values - mi0_)**2))/sum(smoothedInfProbs))
  sigma1_ <- sqrt(sum((1-smoothedInfProbs)*((values - mi1_)**2))/sum(1-smoothedInfProbs))
  p_tilda_00 <- head(infProbs, -1)*(tail(smoothedInfProbs, -1)/tail(forProbs, -1))*p00
  p00_ <- sum(p_tilda_00)/sum(head(smoothedInfProbs,-1))
  p_tilda_11 <- head(1-infProbs, -1)*(tail(1-smoothedInfProbs, -1)/tail(1-forProbs, -1))*p11
  p11_ <- sum(p_tilda_11)/sum(head(1-smoothedInfProbs, -1))
  fi_ <- smoothedInfProbs[1]
  return(list(mi0_ = mi0_, sigma0_ = sigma0_, mi1_ = mi1_, sigma1_ = sigma1_, p00_ = p00_, p11_ = p11_, fi_ = fi_))
}

EMalgorithm <- function(mi0, sigma0, mi1, sigma1, p00, p11, fi, values, threshold = 1e-8, printSteps=FALSE){
  probs <- inferenceForecastRecursion(mi0, sigma0, mi1, sigma1, p00, p11, fi, values)
  fors0 <- probs$forecasts
  infs0 <- probs$inferences
  smoothInfs0 <- probs$smoothedInferences
  
  logLike <- logLikelihood(mi0, sigma0, mi1, sigma1, values, fors0)
  
  while(TRUE){
    
    if(printSteps) {
      print(paste("Parameters:", mi0, sigma0, mi1, sigma1, p00, p11, fi))
      print("Log likelihood:", logLike)
    }
    
    # MAXIMIZATION
    newParams <- maximizeParameters(mi0, sigma0, mi1, sigma1, p00, p11, fi, values, infs0, fors0, smoothInfs0)
    
    mi0 <- newParams$mi0_
    sigma0 <- newParams$sigma0_
    mi1 <- newParams$mi1_
    sigma1 <- newParams$sigma1_
    p00 <- newParams$p00_
    p11 <- newParams$p11_
    fi <- newParams$fi_
    
    # EXPECTATION
    probs <- inferenceForecastRecursion(mi0, sigma0, mi1, sigma1, p00, p11, fi, values)
    
    fors0 <- probs$forecasts
    infs0 <- probs$inferences
    smoothInfs0 <- probs$smoothedInferences
    
    newLogLike <- logLikelihood(mi0, sigma0, mi1, sigma1, values, fors0)
    
    if (abs(logLike - newLogLike) < threshold) {
      break
    }
    else {
      logLike <- newLogLike
    }
    
  }
  
  return(list(mi0=mi0, sigma0=sigma0, mi1=mi1, sigma1=sigma1, p00=p00, p11=p11, fi=fi))
}
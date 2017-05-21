library(ggplot2)

model <- function(mi0, sigma0, mi1, sigma1, p00, p11, fi, steps=100) {
  state <- !rbinom(1, 1, fi) # FALSE marks regime 0, TRUE marks regime 1
  
  prediction <- c()
  regimes <- c()
  
  for (i in 1:steps){
    regimes <- c(regimes, state)
    # print(paste("In state:", state))
    if (state) { # If in regime 1
      value <- rnorm(1, mean=mi1, sd=sigma1) # Generate a value
      if (rbinom(1, 1, 1-p11)){ # We generate a random bernoulli observation based on probability
        # for switching form regime 1 to 0. If it comes out 1, we switch regimes.
        state <- !state
      }
    }
    else { # If in regime 0
      value <- rnorm(1, mean=mi0, sd=sigma1)
      if (rbinom(1, 1, 1-p00)){
        state <- !state
      }
    }
    # print(paste("Generated value:", value))
    prediction <- c(prediction, value)
  }
  
  return(list(prediction=prediction, regimes=regimes))
}

# All of attributes must be of the same length
plotPredictionTrue <- function(pred, true, x, regimes){
  predictData = data.frame(
    x = x,
    predValues = pred,
    regimes = regimes,
    trueValues = true
  )
  
  ggplot(data = predictData, aes(x = x, y = predValues, colour=regimes)) +
    geom_line(aes(group=1)) +
    geom_line(data = predictData, aes(x = x, y = trueValues), colour='grey', alpha=0.8)

  # ggplot() +
  #   geom_line(data=predictData, aes(x = x, y = abs(trueValues - predValues), colour='red'))
}

# RMSE
rmse <- function(pred, true) {
  return(sqrt(sum((true-pred)**2)/length(pred)))
}
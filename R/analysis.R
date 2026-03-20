#' Overarching function (generates data, runs each condition, extracts stats)
#'
#' @param iter Number of iterations
#' @param n Sample size in baseline scenario
#' @param b0 Intercept value
#' @param b1 Main effect coefficient in the effect scenario
#' @param b_z Continuous covariate effect coefficient
#' @param b_d Dichotomous covariate effect coefficient
#' @param dep The dependent variable ("y_yes" or "y_no")
#' @return b1, p-value and confidence interval for baseline and each deviation
#'   scenario
#' @export

analysis <- function(iter, n, b0, b1, b_z, b_d, dep) {

  #create empty dataframe for the statistics
  stats <- data.frame(matrix(ncol = 5, nrow = (10*iter)))
  colnames(stats) <- c("b1","p.value","lower.ci","upper.ci", "condition")

  for (i in 1:iter) {

    #set.seed(i)

    #simulate a dataset
    df <- dgm(n, b0, b1, b_z, b_d)

    #run all scenarios on the dataset
    result <- conditions(df, dep)

    #add scenarios to the stats dataset and add iteration number
    stats[(((i-1)*10+1)):(i*10),] <- result
    stats$iteration[(((i-1)*10+1)):(i*10)] <- i

  }

  return(stats)
  }



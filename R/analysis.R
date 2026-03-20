#' Overarching function (generates data, runs each condition, extracts stats)
#'
#' @param iter Number of iterations
#' @param n Sample size in baseline scenario
#' @param b0 Intercept value
#' @param b1 Main effect coefficient in the effect scenario
#' @param b_z Continuous covariate effect coefficient
#' @param b_d Dichotomous covariate effect coefficient
#' @param it The iteration number
#' @return b1, p-value and confidence interval for baseline and each deviation
#'   scenario
#' @export

analysis <- function(iter, n, b0, b1, b_z, b_d, it) {
  #simulate a dataset
  df <- dgm(n, b0, b1, b_z, b_d)

  #run all scenarios on the dataset
  result_yes <- conditions(df, "y_yes") |>
    stats::setNames(c("b1", "p.value", "lower.ci", "upper.ci", "condition")) |>
    cbind(scenario = "effect", iteration = it)
  result_no <- conditions(df, "y_no")  |>
    stats::setNames(c("b1", "p.value", "lower.ci", "upper.ci", "condition")) |>
    cbind(scenario = "effect", iteration = it)

  results <- rbind(result_yes, result_no)

  return(results)
}

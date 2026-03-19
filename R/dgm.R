#' Data generating mechanism
#'
#' @param n Sample size in the baseline scenario
#' @param b0 Intercept value
#' @param b1 Main effect coefficient in the effect scenario
#' @param b_z Continuous covariate effect coefficient
#' @param b_d Dichotomous covariate effect coefficient
#' @return Dataframe
#' @importFrom dplyr filter mutate
#' @importFrom magrittr %>%
#' @importFrom stats rnorm rbinom runif
#' @importFrom faux rnorm_pre
#' @export
dgm <- function(n, b0, b1, b_z, b_d) {

  #biggest possible sample size
  nbig <- ceiling(n*1.15)

  #sampling from distributions
  ##simulating the independent variable x
  x <- rnorm(n = nbig, mean = 0, sd = 1)

  ##simulating the continuous covariate z
  z <- rnorm(n = nbig, mean = 0, sd = 1)

  ##simulating the dichotomous covariate d
  d <- rbinom(n = nbig, size = 1, prob = 0.5)

  ##simulating random error epsilon
  re <- rnorm(n = nbig, mean = 0, sd = 0.86)


  #formulating the regression formula
  ##simulating the dependent variable y
  y_no <-  b0 + b_z * z + b_d * d + re
  y_yes <- y_no + b1 * x

  #create data frame
  df <- data.frame(x, y_no, y_yes, z, d)

  #replacing 5% of values with potentially random outliers
  ##creating an increased random error
  re_outlier_high <- rnorm(n = nbig, mean = 3, sd = 1)
  re_outlier_low <- rnorm(n = nbig, mean = -3, sd = 1)

  ##creating new dependent variable with increased outlier probability
  y_no_outlier_high <- b0 + b_z * z + b_d * d + re_outlier_high
  y_no_outlier_low <- b0 + b_z * z + b_d * d + re_outlier_low

  y_yes_outlier_high <- y_no_outlier_high + b1 * x
  y_yes_outlier_low <-y_no_outlier_low + b1 * x

  #replace 5% (list) of all y values (x) with a value from the new dependent variable (values)
  repla <- sample(1:nbig, 0.05 * nbig)

  df <- df %>% mutate(y_no =
                        replace(
                          x      = y_no,
                          list   = repla,
                          values = ifelse(
                            runif(length(repla)) < 0.5,
                            sample(y_no_outlier_high, 0.05 * nbig),
                            sample(y_no_outlier_low, 0.05 * nbig)
                          )
                        ))


  df <- df %>% mutate(y_yes =
                        replace(
                          x      = y_yes,
                          list   = sample(1:nbig, 0.05 *
                                            nbig),
                          values = ifelse(
                            runif(length(repla)) < 0.5,
                            sample(y_yes_outlier_high, 0.05 * nbig),
                            sample(y_yes_outlier_low, 0.05 * nbig)
                          )
                        ))

  #Alternative y variable correlated .6 with y
  df$y_yes_alt <- rnorm_pre(df$y_yes,
                            mu = 0,
                            sd = 1,
                            r = 0.6)
  df$y_no_alt <-  rnorm_pre(df$y_no,
                            mu = 0,
                            sd = 1,
                            r = 0.6)

  #request the data set as output of the function
  return(df)
}

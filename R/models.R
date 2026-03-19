#' Adds a continuous or dichotomous covariate to the linear regression, or uses
#' alternative outcome.
#'
#' @param df The dataframe you wish to analyze
#' @param dep The dependent variable
#' @return b1, p-value and confidence interval for the model deviations
#' @importFrom dplyr filter slice
#' @importFrom magrittr %>%
#' @importFrom stats lm
#' @export

models <- function(df, dep) {

  #create dataframe to save statistics
  models.stat <- data.frame(matrix(ncol = 4, nrow = 3)) #4 because four values get saved in the extract function, 3 columns for methods
  colnames(models.stat) <- c("b1","p.value","lower.ci","upper.ci")

  #we take the first 200 participants, then
  #remove outliers >3sd
  ssbase <- (nrow(df)/1.15)
  df.models <- df %>% slice(1:ssbase)

  df.models <- df.models %>% filter(
    .data[[dep]] >= mean(.data[[dep]]) - 3*sd(.data[[dep]]),
    .data[[dep]] <= mean(.data[[dep]]) + 3*sd(.data[[dep]])
  )

  #save dependent variable as y
  y <- df.models[[dep]]

  #then we run the different possible models
  ##continuous covariate
  reg <- lm(y ~ x + z, data = df.models)
  models.stat[1,] <- extr(model = reg)
  models.stat$condition[1] <- "Continuous covariate"

  ##dichotomous covariate
  reg <- lm(y ~ x + d, data = df.models)
  models.stat[2,] <- extr(model = reg)
  models.stat$condition[2] <- "Dichotomous covariate"

  ##alternative outcome
  ya <- df.models[[paste0(dep, "_alt")]]
  reg <- lm(ya ~ x, data = df.models)
  models.stat[3,] <- extr(model = reg)
  models.stat$condition[3] <- "Alternative outcome"


  #return the statistics as output
  return(models.stat)
}

#' Removes outliers based on z-score > 2 and Cook's distance > 1
#'
#' @param df The dataframe you wish to analyze
#' @param dep The dependent variable
#' @return b1, p-value and confidence interval for the outlier deviations
#' @importFrom dplyr filter slice between
#' @importFrom magrittr %>%
#' @importFrom stats lm cooks.distance
#' @export

outlier <- function(df, dep){

  #create dataframe to save statistics
  outlier.stat <- data.frame(matrix(ncol = 4, nrow = 2)) #4 because four values get saved in the extract function, 2 columns for methods
  colnames(outlier.stat) <- c("b1","p.value","lower.ci","upper.ci")

  #for the outlier scenario we only take the first 200 participants
  ssbase <- (nrow(df)/1.15)
  df.outlier <- df %>% slice(1:ssbase)

  ##perform baseline regression in order to identify outliers with Cook's
  y <- df.outlier[[dep]]
  reg <- lm(y~x, data = df.outlier)

  #Strict Z-scores (2 sd)
  ##exclude outliers based on z-scores
  df.outlier$zscore <- scale(df.outlier[[dep]])
  df.ex.z <- df.outlier %>% filter(between(zscore, -2, 2))

  ##perform regression for the z-score condtion
  y <- df.ex.z[[dep]]
  reg.ex.z <- lm(y~x, data = df.ex.z)
  outlier.stat[1,] <- extr(model = reg.ex.z)
  outlier.stat$condition[1] <- "Strict z-score"


  #Cook's
  ##exclude outliers based on Cook's distance
  df.outlier$cook <- cooks.distance(reg)
  df.ex.cook <- df.outlier %>% filter(between(cook, 0, 1))

  ##perform regression for the Cook's condition
  y <- df.ex.cook[[dep]]
  reg.ex.cook <- lm(y~x, data = df.ex.cook)
  outlier.stat[2,] <- extr(model = reg.ex.cook)
  outlier.stat$condition[2] <- "Cook's distance"

  #return the statistics as output
  return(outlier.stat)
}

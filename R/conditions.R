#' Combines the different deviation conditions and baseline condition
#'
#' @param df The dataframe you wish to analyze
#' @param dep The dependent variable
#' @return b1, p-value and confidence interval for all scenarios
#' @export

conditions <- function(df, dep){
  base.stat <- baseline(df, dep)
  size.stat <- samplesize(df, dep)
  outlier.stat <- outlier(df, dep)
  model.stat <- models(df, dep)

  frames <- list(base.stat, size.stat, outlier.stat, model.stat)
  do.call(rbind, frames)
}

#' Baseline Scenario
#'
#' @param df The dataframe you wish to analyze
#' @param dep The dependent variable
#' @return b1, p-value and confidence interval for baseline scenario
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom stats lm
#' @export
baseline <- function(df, dep){

  #for the baseline scenario we only take the first 200 participants, then
  #remove outliers >3sd
  ssbase <- (nrow(df)/1.15)
  df.baseline <- df %>% slice(1:ssbase)

  df.baseline <- df.baseline %>% filter(
    .data[[dep]] >= mean(.data[[dep]]) - 3*sd(.data[[dep]]),
    .data[[dep]] <= mean(.data[[dep]]) + 3*sd(.data[[dep]])
  )

  #save dependent variable as y
  y <- df.baseline[[dep]]

  #perform a regression analysis and extract the statistics
  reg <- lm(y ~ x, data = df.baseline)
  base.stat <- extr(model = reg)
  base.stat$condition <- c("Baseline")

  return(base.stat)
}

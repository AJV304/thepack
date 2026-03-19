#' Changes your sample size with 2.5%/15% increase and decrease.
#'
#' @param df The dataframe you wish to analyze
#' @param dep The dependent variable
#' @return b1, p-value and confidence interval for sample size deviations
#' @importFrom dplyr filter slice
#' @importFrom magrittr %>%
#' @importFrom stats lm
#' @export
samplesize <- function(df, dep){

  #specify the alternative sample sizes to test
  size <- c(0.85, 0.975, 1.025, 1.15) #2.5% & 15% increase and decrease
  ss <- length(size)
  base <- (nrow(df)/1.15)

  #create empty data frame to save the output in
  size.stat <- data.frame(matrix(ncol = 4, nrow = ss)) #4 because four values get saved in the extract function
  colnames(size.stat) <- c("b1","p.value","lower.ci","upper.ci")

  #create forloop which tests for each different sample size
  for (i in 1:ss) {
    #take the specified size from the full data set
    df.samplesize1 <- df %>% slice(1:(base*(size[i])))
    #filter the outliers from the data
    df.samplesize <- df.samplesize1 %>% filter(
      .data[[dep]] >= mean(.data[[dep]]) - 3*sd(.data[[dep]]),
      .data[[dep]] <= mean(.data[[dep]]) + 3*sd(.data[[dep]])
    )

    #use the specified depedent variable as the dependent variable
    y <- df.samplesize[[dep]]

    #perform a regression analysis and extract the statistics
    reg <- lm(y ~ x, data = df.samplesize)
    size.stat[i,] <- extr(model = reg)
    size.stat$condition[i] <- paste0("Sample size (", nrow(df.samplesize1), ")")

  }

  #return the statistics as output
  return(size.stat)
}


#' Choice analysis, chooses a significant p-value from deviations if baseline is insignificant
#'
#' @param scenarios The dataframe you wish to analyze, typically a dataframe from the analysis function
#' @return b1, p-value and confidence interval for chosen scenario per iteration. As well as how many deviations were significant.
#' @importFrom dplyr filter slice_sample
#' @importFrom magrittr %>%
#' @export

choice <- function(scenarios){
  #save the number of iterations as x
  x <- length(unique(scenarios[,6]))

  #create empty data frame with 1 row per iteration
  final <- data.frame(matrix(ncol = sum(ncol(scenarios), 1), nrow = x))
  colnames(final) <- c(colnames(scenarios), "n.sig")

  #for each iteration follow the rank order to check which result to select
  for (i in 1:x){
    #select the current iteration
    it <- scenarios %>% filter(iteration == i)

    #check if the baseline scenario is significant
    if (it[it$condition == "Baseline", "p.value"] < 0.05){
      #if it is, then select the baseline scenario
      final[i,] <- it[it$condition == "Baseline",]
      final[i, "n.sig"] <- "NA"
    } else {
      #checks if any of the deviations have a p-value under 0.05
      if (any(it[,"p.value"] < 0.05)){
        #chooses one of the significant deviations at random
        sig <- it %>% filter(p.value < 0.05)
        final[i,] <- sig %>% slice_sample(n = 1)
        #saves the number of significant deviations
        final[i, "n.sig"] <- nrow(sig)
        } else {
          #if nothing is significant, then the baseline is reported and 0
          #significant deviations
          final[i,] <- it[it$condition == "Baseline",]
          final[i,"n.sig"] <- 0
          }
    }
  }

  return(final)
  }





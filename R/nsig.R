#' Significance counter
#'
#' @param df Input a dataframe which follows the output structure of the analysis function
#' @return Dataframe with the number of significant p-values per condition
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @export
nsig <- function(df){

  #output dataframe, one row per condition, and one column for significance
  #count, one column for condition name to be filled in later
  out <- as.data.frame(matrix(nrow = 10, ncol = 0))

  #for each condition, select the number of significant p-values
  conditions <- df[1:10, "condition"]

  for (i in 1:10){
    #filter only the results of 1 specific condition
    condition <- df %>% filter(condition == conditions[i])

    #count the number of significant p-values
    sig <- condition %>% filter(p.value < 0.5)
    out[i, "n.sig"] <- nrow(sig)

    #save the number of significant values as a percentage of that condition
    perc <- (nrow(sig)/nrow(condition))*100
    perc <- round(perc, digits = 1)
    out[i, "n.sig.perc"] <- paste0(perc, "%")

    #add what condition we're testing
    out[i, "conditions"] <- df[i, "condition"]
  }


#return the out df as output
  return(out)
}

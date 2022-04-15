#' @title getDataset
#' @description Selects the best model from a list of MplusObjects based on a
#'     specified method. Current selection methods are "BIC" (select model with 
#'     the lowest BIC) and "BIC_LRT" (select model with the lowest BIC that also 
#'     has a significant LRT p-value).
#' @param model An MplusObject containing save data
#' @param df A data frame to combine with Mplus classes
#' @param idvar A character vector containing the ID variable in the data frame
#' @return An MplusObject
#' @export
getDataset <- function(model, df, idvar) {
  
  # C <- model[["results"]][["savedata"]][["C"]]
  # ID <- as.factor(model[["rdata"]][[idvar]])
  SAVEDATA <- model[["results"]][["savedata"]] %>% rename(idvar = PIN)
    
  
  # class_vars <- c()
  # for (i in 1:max(C)) {
  #   n_cls <- model[["results"]][["class_counts"]][["mostLikely"]][["count"]][[i]]
  #   class_vars <- c(class_vars, paste0(i, ' (N=', n_cls, ')'))
  # }
  
  # C <- as.factor(C)
  # levels(C) <- class_vars
  
  # class_res <- data.frame(idvar, SAVEDATA)
  # colnames(class_res) <- c(idvar, 'Class')
  
  df_final <- merge(df, class_res, by.y = idvar)
  df_final$C <- as.factor(df_final$C)
  
  return(df_final)
  
}
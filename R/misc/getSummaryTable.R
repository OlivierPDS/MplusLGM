#' @title getSummaryTable
#' @param df A data frame containing all descriptive variables and the grouping variable
#' @param list_vars A vector containing the variables to summarize
#' @param var_labels A vector of labels for all in list_var
#' @param group The variable name for grouping
#' @param group.test Boolean, specify whether vtable will run ANOVA/ChiSq
#' @return A data frame
#' @export
#' @import vtable
#' @import tidyverse
getSummaryTable <- function(df, list_vars, var_labels = NA, group = NA, group.test = FALSE) {
  
  tab <- sumtable(
    data = df,
    vars = list_vars,
    labels = var_labels,
    summ = c('notNA(x)','mean(x)','sd(x)'),
    summ.names = c('N','MP','SD'),
    group = group,
    group.test = group.test,
    out = "return",
    digits = 3
  )
  
  
  # If not by group
  if (is.na(group)) {
    
    tab$Overall <- ifelse(
      tab$SD != '',
      tab$Overall <- paste0('M = ', tab$MP, ' ± ', tab$SD),
      ifelse(
        tab$MP != '',
        tab$Overall <- paste0('N = ', tab$N, ' (', tab$MP, ')'),
        tab$MP <- ''
      )
    )
    
    return(subset(tab, select = c('Variable', 'Overall')))
    
  } else {
    new_list <- list(Variable = c(tab[2:nrow(tab), 1]))
    
    for (c in 2:length(tab)) { # skip if the name of grouping variable
      
      if (tab[1 , c] != '') { # if the name of level of group variable
        
        group_res <- c()
        for (r in 2:nrow(tab)) { # iterate through rows which correspond to N
          
          # get values from mean/percent and SD
          n_tmp <- tab[r , c]
          mp_tmp <- tab[r , c+1]
          sd_tmp <- tab[r , c+2]
          
          if (mp_tmp == '' && sd_tmp == '') {
            group_res <- c(group_res, '')
          } 
          
          else if (sd_tmp == '') {
            group_res <- c(group_res, paste0('N = ', n_tmp, ' (', mp_tmp, ')'))
          }
          
          else {
            group_res <- c(group_res,  paste0('M = ', mp_tmp, ' ± ', sd_tmp))
          }
          
        }
        
        new_list[[tab[1 , c]]] <- unlist(group_res)
        
      }
      
      else if (colnames(tab)[[c]] == 'Test') {
        
        new_list[['Test']] <- tab[2:nrow(tab), c]
        
      }
      
    }
    
    return(data.frame(new_list))
    
  }
}

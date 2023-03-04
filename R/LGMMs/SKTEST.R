SKTEST <- function(mpobj, usevar) {
  
  sktest_mpobj <- mpobj %>% 
    update(DATA = ~ "LISTWISE = ON;", 
           OUTPUT = ~ . + "TECH13;") %>% 
    mplusModeler(
      object = .,
      dataout = glue(getwd(), '/{usevar}/Results/FINALm_SKTEST.dat'),
      modelout = glue(getwd(), '/{usevar}/Results/FINALm_SKTEST.inp'),
      hashfilename = FALSE,
      run = 1,
      check = TRUE,
      varwarnings = TRUE,
      writeData = "always"
    )
  
  output <- sktest_mpobj[["results"]][["output"]]
  
  tech13 <- output %>% 
    str_which("TECHNICAL 13 OUTPUT")
  
  skew <- output[tech13 + c(7:10)] %>%  
    str_extract_all("\\d+.\\d+") %>%
    as.data.frame() %>% 
    setNames(c("Sample Value", "Mean", "Standard Deviation", "P-Value"))
  
  kurtosis <- output[tech13 + c(14:17)] %>%  
    str_extract_all("\\d+.\\d+") %>%
    as.data.frame() %>% 
    setNames(c("Sample Value", "Mean", "Standard Deviation", "P-Value"))
    
  sktest <- bind_rows(skew, kurtosis)
  
  rownames(sktest) <- c("skew", "kurtosis")
  
  return(sktest)

  }
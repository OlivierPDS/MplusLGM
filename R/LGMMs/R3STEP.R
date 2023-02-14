R3STEP <- function(df,
                       idvar,
                       usevar,
                       cov,
                       starts = 0,
                       output = c("SAMPSTAT", "STANDARDIZED", "CINTERVAL"),
                       model) {
  
  
  # To do -------------------------------------------------------------------
  ## select cov-specific starts values
  ## add arguments for plots  
  ## silence warning message from MplusAutomation::parseMplus
  
  # Test arguments ----------------------------------------------------------
  # df <-  SAPS_df
  # idvar  <-  'pin'
  # usevar <-  SAPS
  # cov <-  c(SD_num, SD_cat)
  # starts <-  0
  # output <-  c("SAMPSTAT", "STANDARDIZED", "CINTERVAL")
  # model  <-  FINAL_model
  
  # Extract model parameters and data ---------------------------------------
  logits_df <- model[["results"]][["class_counts"]][["logitProbs.mostLikely"]] #logits
  k <- model[["results"]][["summaries"]][["NLatentClasses"]] #classes
  lgmm <- model[["TITLE"]] #title
  
  savedata <-
    purrr::map(cov,
               \(z) merge(
                 x = model[["results"]][["savedata"]], 
                 y = select(df, c(idvar, z)),
                 by.x = stringr::str_to_upper(idvar),
                 by.y = idvar,
                 all = TRUE
               )) %>%
    purrr::map(rename, N = C) #data
  
  ## Create each arguments for mplusObject() -------------------------------
  ## TITLE = 
  title <-  cov %>% 
    purrr::map(~ glue::glue("{lgmm} - R3STEP_{.x}")) %>%
    purrr::map(~ MplusAutomation::parseMplus(.x, add = TRUE))
  
  ## VARIABLE = 
  usevars <- purrr::map(cov, \(x) glue("USEVAR = {x} N"))
  
  nominal <- "NOMINAL = N"
  
  classes <- glue::glue("CLASSES = c({k})")
  
  variable <- purrr::map(usevars, \(x) c(x, nominal, classes)) %>%
    purrr::map(~ MplusAutomation::parseMplus(.x, add = TRUE))
    # purrr::map(~ strwrap(.x, width = 90, exdent = 5)) %>%
    # purrr::map(~ paste(.x, collapse = "\n")) %>%
    # purrr::map(~ gsub(";", ";\n",.x))
  
  ## ANALYSIS =
  type <- "TYPE = MIXTURE"
  algorithm <- "ALGORITHM = INTEGRATION"
  integration <- "INTEGRATION = MONTECARLO"
  start_val <-  glue::glue("STARTS = {starts}")
  processors <- glue::glue('PROCESSORS = {detectCores()}')
  analysis <- MplusAutomation::parseMplus(c(type, algorithm, integration, start_val, processors), add = TRUE)
  
  ## MODEL = 
  class_spec <- purrr::map(1:k, ~ glue::glue('%C#{.x}%')) #class sections
  
  logits <-  purrr::map(1:k, \(x) purrr::map2(x, 1:k, \(x, y) logits_df[x, y])) %>%
    unlist() %>%
    purrr::map2(rep(1:k, k), ., \(x, y) glue::glue("[N#{x}@{y}]")) %>%
    split(rep(1:k, each = k)) %>%
    purrr::map(\(x) utils::head(x, -1))  #logits specification
  
  reg <- purrr::map(cov, \(x) glue::glue("C ON {x}")) #regressions specification

  ### Model specification
  model1 <- list('%OVERALL%', reg, cov) %>%
    purrr::reduce(~ purrr::map2(.x, .y, ~ c(.x, .y)))
  
  model2 <- list(class_spec, logits) %>%
    purrr::reduce(~ purrr::map2(.x, .y, ~ c(.x, .y)))
  
  model3 <- map(model1, \(x) append(x, model2)) %>% 
    purrr::map(~ MplusAutomation::parseMplus(unlist(.x), add = TRUE)) %>%
    purrr::map(~ gsub("%;", "%",.x)) #remove semicolon after model sections
  
  # OUTPUT =
  outputs <- MplusAutomation::parseMplus(output, add = TRUE)
  
  # Create Mplus object -----------------------------------------------------
  mpobj <- purrr::pmap(list(title, variable, model3, savedata),  \(title, variable, model, savedata)
                       MplusAutomation::mplusObject(
                         TITLE = title,
                         VARIABLE = variable,
                         MODEL = model,
                         ANALYSIS = analysis,
                         OUTPUT = outputs,
                         autov = FALSE,
                         usevariables = colnames(savedata),
                         rdata = savedata)
  )
  
  # Create directory for Mplus data, inputs and outputs ---------------------
  path <- glue::glue(getwd(), substitute(usevar), 'Results', 'R3STEP', .sep = "/")
  if (!dir.exists(path)) {dir.create(path, recursive = TRUE)}
  
  # Run Mplus models --------------------------------------------------------
  model_lst <- purrr::map2(mpobj, cov, \(x, y)
                           MplusAutomation::mplusModeler(
                             object = x,
                             dataout = glue::glue("{path}/{y}_R3STEP.dat"),
                             modelout = glue::glue("{path}/{y}_R3STEP.inp"),
                             hashfilename = FALSE,
                             run = 1,
                             check = FALSE,
                             writeData = "always",
                             quiet = TRUE
                           )
  )
  
  names(model_lst) <- cov
  
  return(model_lst)
}

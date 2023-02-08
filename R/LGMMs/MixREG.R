MixREG <- function(df,
                       idvar,
                       usevar,
                       cov,
                       starts = 0,
                       output = c("SAMPSTAT", "STANDARDIZED", "CINTERVAL"),
                       model) {
  
  
  # To do -------------------------------------------------------------------
  ## select cov-specific starts values
  ## add arguments for plots  
  ## silence warning message from parseMplus
  
  # Test arguments ----------------------------------------------------------
  # df <-  SAPS_df
  # idvar  <-  'pin'
  # usevar <-  SAPS
  # cov <-  list(SOFAS, SANS)
  # starts  <-  0
  # output  <-  c("SAMPSTAT", "CINTERVAL")
  # model  <-  FINAL_model
  
  # Re-format called arguments ----------------------------------------------
  cov_vec <- substitute(cov) %>%
    as.character() %>%
    tail(-1)
  
  cov <- cov %>% 
    modify_if(\(cov) length(usevar) < length(cov), 
              \(cov) subset(cov, str_detect(cov, paste(parse_number(usevar), collapse = "|"))), 
              .else = \(cov) identity(cov))
  
  # usevar_lst <- cov %>% 
  #   map_if(\(x) length(usevar) > length(x), 
  #          \(x) subset(usevar, str_detect(usevar, paste(parse_number(x), collapse = "|"))), 
  #          .else = \(x) identity(usevar))
  
  usevar_col <- usevar %>% paste(collapse = " ")
  
  # Extract model parameters and data ---------------------------------------
  logits_df <- model[["results"]][["class_counts"]][["logitProbs.mostLikely"]] #logits
  k <- model[["results"]][["summaries"]][["NLatentClasses"]] #classes
  lgmm <- model[["TITLE"]] #title
  
  savedata <-
    map(cov,
        \(z) merge(
          x = select(model[["results"]][["savedata"]], -usevar), #error when usevar %in% cov
          y = select(df, c(idvar, z)),
          by.x = str_to_upper(idvar),
          by.y = idvar,
          all = TRUE
        )) %>%
    map(rename, N = C) #data
  
  ## Create each arguments for mplusObject() -------------------------------
  ## TITLE = 
  title <-  cov_vec %>% 
    map(~ glue("{lgmm} - MixREG_{.x}")) %>%
    map(~ parseMplus(.x, add = TRUE))
  
  ## VARIABLE = 
  usevars <- cov %>% 
    map(\(x) paste(x, collapse = " ")) %>%
    map(\(x) paste("USEVAR =", x, "N", "I", "S", collapse = " "))
  
  nominal <- "NOMINAL = N"
  
  classes <- glue("CLASSES = c({k})")
  
  variable <- map(usevars, \(x) c(x, nominal, classes)) %>%
    map(~ parseMplus(.x, add = TRUE))
    # map(~ strwrap(.x, width = 90, exdent = 5)) %>%
    # map(~ paste(.x, collapse = "\n")) %>%
    # map(~ gsub(";", ";\n",.x))
  
  ## ANALYSIS =
  type <- "TYPE = MIXTURE"
  start_val <-  glue("STARTS = {starts}")
  processors <- glue('PROCESSORS = {detectCores()}')
  analysis <- parseMplus(c(type, start_val, processors), add = TRUE)
  
  ## MODEL = 
  class_spec <- map_chr(1:k, ~ glue('%C#{.x}%')) #class sections
  
  logits <-  map(1:k, \(x) map2(x, 1:k, \(x, y) logits_df[x, y])) %>%
    unlist() %>%
    map2_chr(rep(1:k, k), ., \(x, y) glue("[N#{x}@{y}]")) %>%
    split(rep(1:k, each = k)) %>%
    map(\(x) head(x, -1))  #logits specification
  
  reg_i <- map(cov, \(x) glue("I ON {paste(x, collapse = ' ')}")) #regressions specification
  reg_s <- map(cov, \(x) glue("S ON {paste(x, collapse = ' ')}")) #regressions specification
  
  variance <- map(cov, \(x) paste(x, collapse = " ")) #covariates variance specification
  
  ### Model specification
  model1 <- list(reg_i, reg_s, variance) %>%
    reduce(~ map2(.x, .y, ~ c(.x, .y)))
  
  model2 <- model1 %>%
    map(~ c('%OVERALL%', .x))
  
  model3 <- list(class_spec, logits) %>%
    reduce(~ map2(.x, .y, ~ c(.x, .y)))
  
  model4 <- cross2(model3, model1) %>% 
    split(., rep(1:(length(.)/k), each = k))
  
  model5 <- list(model2, model4) %>%
    reduce(~ map2(.x, .y, ~ c(.x, .y))) %>%
    map(~ parseMplus(unlist(.x), add = TRUE)) %>%
    map(~ gsub("%;", "%",.x)) #remove semicolon after model sections
  
  # OUTPUT =
  outputs <- parseMplus(output, add = TRUE)
  
  # Create Mplus object -----------------------------------------------------
  mpobj <- pmap(list(title, variable, model5, savedata),  \(title, variable, model, savedata)
                mplusObject(
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
  path <- glue(getwd(), substitute(usevar), 'Results', 'MixREG', .sep = "/")
  if (!dir.exists(path)) {dir.create(path, recursive = TRUE)}
  
  # Run Mplus models --------------------------------------------------------
  model_lst <- map2(mpobj, cov_vec, \(x, y)
                    mplusModeler(
                      object = x,
                      dataout = glue("{path}/{y}_MixREG.dat"),
                      modelout = glue("{path}/{y}_MixREG.inp"),
                      hashfilename = FALSE,
                      run = 1,
                      check = FALSE,
                      writeData = "always",
                      quiet = TRUE
                    )
  )
  
  names(model_lst) <- cov_vec
  
  return(model_lst)
}

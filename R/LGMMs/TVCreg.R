TVCreg <- function(df,
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
  # cov <-  list(SANS, CDS, PSR)
  # starts  <-  0
  # output  <-  c("SAMPSTAT", "CINTERVAL")
  # model  <-  FINAL_model
  
# Re-format called arguments ----------------------------------------------
  cov_vec <- substitute(cov) %>%
    as.character() %>%
    utils::tail(-1)

  cov <- cov %>% 
    purrr::modify_if(\(cov) length(usevar) < length(cov), 
              \(cov) subset(cov, stringr::str_detect(cov, paste(parse_number(usevar), collapse = "|"))), 
              .else = \(cov) identity(cov))
  
  cov_dummy <- cov %>%  
    map_depth(1, #at
    \(x) purrr::map_if(x,#x
      \(x) is.factor(df[[x]]), #p
      \(x) map_chr(1:(nlevels(df[[x]]) - 1), #f, y
                   \(y) paste0(x, y)) %>% #f
        paste(collapse = " ")))
    
  usevar_lst <- cov %>% 
    purrr::map_if(\(x) length(usevar) > length(x), 
           \(x) subset(usevar, stringr::str_detect(usevar, paste(parse_number(x), collapse = "|"))), 
           .else = \(x) identity(usevar))
  
  usevar_col <- usevar_lst %>% purrr::map(~ paste(.x, collapse = " "))
  
# Extract model parameters and data ---------------------------------------
  logits_df <- model[["results"]][["class_counts"]][["logitProbs.mostLikely"]] #logits
  k <- model[["results"]][["summaries"]][["NLatentClasses"]] #classes
  lgmm <- model[["TITLE"]] #title
  
  savedata <-
    purrr::map(cov,
        \(z) merge(
          x = model[["results"]][["savedata"]], 
          y = dplyr::select(df, all_of(c(idvar, z)), -any_of(usevar)),
          by.x = stringr::str_to_upper(idvar),
          by.y = idvar,
          all = TRUE
        )) %>%
    purrr::map(rename, N = C) #data
  
## Create each arguments for mplusObject() -------------------------------
## TITLE = 
  title <-  cov_vec %>% 
    purrr::map(~ glue::glue("{lgmm} - TVCreg_{.x}")) %>%
    purrr::map(~ MplusAutomation::parseMplus(.x, add = TRUE))

## VARIABLE = 
  usevars <- cov_dummy %>% 
    purrr::map(\(x) paste(x, collapse = " ")) %>%
    purrr::map2(usevar_col, \(x, y) paste("USEVAR =", "N", y, x, collapse = " "))
    
  nominal <- "NOMINAL = N"
  
  classes <- glue::glue("CLASSES = c({k})")
  
  variable <- purrr::map(usevars, \(x) c(x, nominal, classes)) %>%
    purrr::map(~ MplusAutomation::parseMplus(.x, add = TRUE)) %>%
    purrr::map(~ strwrap(.x, width = 90, exdent = 5)) %>%
    purrr::map(~ paste(.x, collapse = "\n")) %>%
    purrr::map(~ gsub(";", ";\n",.x))

## DEFINE =
  define <- cov %>% 
    purrr::map_depth(1, #at
              \(x) purrr::map_if(x, #x
                                 \(x) is.factor(df[[x]]), #p
                                 \(x) purrr::map_chr(1:(nlevels(df[[x]]) - 1), #f, y
                                                     \(y) glue::glue("{x}{y} = {x} == {y}")),
                                 .else = ~ NULL )) %>% 
    purrr::map_depth(1,
              \(x) purrr::map_if(x,  
                                 \(x) !is.null(x), 
                                 \(x) MplusAutomation::parseMplus(x, add = TRUE))) %>% 
    purrr::map_if(\(x) !all(purrr::map_lgl(x, \(y) is.null(y))), 
                  \(x) paste(x, collapse = " "), 
                  .else = ~ NULL) %>% 
    purrr::map_if(\(x) !is.null(x),
                  \(x) gsub(";", ";\n", x))

## ANALYSIS =
  type <- "TYPE = MIXTURE"
  start_val <-  glue::glue("STARTS = {starts}")
  processors <- glue::glue('PROCESSORS = {detectCores()}')
  analysis <- MplusAutomation::parseMplus(c(type, start_val, processors), add = TRUE)
  
## MODEL = 
  class_spec <- purrr::map_chr(1:k, ~ glue::glue('%C#{.x}%')) #class sections

  logits <-  purrr::map(1:k, \(x) purrr::map2(x, 1:k, \(x, y) logits_df[x, y])) %>%
    unlist() %>%
    purrr::map2_chr(rep(1:k, k), ., \(x, y) glue::glue("[N#{x}@{y}]")) %>%
    split(rep(1:k, each = k)) %>%
    purrr::map(\(x) utils::head(x, -1))  #logits specification

  reg <- purrr::map2(usevar_lst, cov_dummy, \(x, y) 
              purrr::map2(x, y, \(x, y) glue::glue("{x} ON {y}"))) #regressions specification

  variance <- purrr::map(cov_dummy, \(x) paste(x, collapse = " ")) #covariates variance specification

### Model specification
  model1 <- list(reg, variance) %>%
    purrr::reduce(~ purrr::map2(.x, .y, ~ c(.x, .y)))

  model2 <- model1 %>%
    purrr::map(~ c('%OVERALL%', .x))

  model3 <- list(class_spec, logits) %>%
    purrr::reduce(~ purrr::map2(.x, .y, ~ c(.x, .y)))

  model4 <- purrr::cross2(model3, model1) %>% 
    split(., rep(1:(length(.)/k), each = k))

  model5 <- list(model2, model4) %>%
    purrr::reduce(~ purrr::map2(.x, .y, ~ c(.x, .y))) %>%
    purrr::map(~ MplusAutomation::parseMplus(unlist(.x), add = TRUE)) %>%
    purrr::map(~ gsub("%;", "%",.x)) #remove semicolon after model sections
  
# OUTPUT =
  outputs <- MplusAutomation::parseMplus(output, add = TRUE)
  
# Create Mplus object -----------------------------------------------------
  mpobj <- purrr::pmap(list(title, variable, define, model5, savedata),  \(title, variable, define, model, savedata)
                MplusAutomation::mplusObject(
                  TITLE = title,
                  VARIABLE = variable,
                  DEFINE = define,
                  MODEL = model,
                  ANALYSIS = analysis,
                  OUTPUT = outputs,
                  autov = FALSE,
                  usevariables = colnames(savedata),
                  rdata = savedata)
                )

# Create directory for Mplus data, inputs and outputs ---------------------
  path <- glue::glue(getwd(), substitute(usevar), 'Results', 'TVCreg', .sep = "/")
  if (!dir.exists(path)) {dir.create(path, recursive = TRUE)}

# Run Mplus models --------------------------------------------------------
  model_lst <- purrr::map2(mpobj, cov_vec, \(x, y)
    MplusAutomation::mplusModeler(
      object = x,
      dataout = glue::glue("{path}/{y}_TVCreg.dat"),
      modelout = glue::glue("{path}/{y}_TVCreg.inp"),
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

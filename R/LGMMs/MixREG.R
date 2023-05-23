MixREG <- function(df,
                    idvar,
                    usevar,
                    cov,
                    test = NULL,
                    startval = NULL,
                    gf = "S",
                    gfw = c("iw", "sw"),
                    output = c("SAMPSTAT", "STANDARDIZED", "CINTERVAL"),
                    model) {
  
  # To do -------------------------------------------------------------------
  ## add arguments for plots
  ## silence warning message from MplusAutomation::parseMplus
  
  # Test arguments ----------------------------------------------------------
  # df <-  SANS_df
  # idvar  <-  'pin'
  # usevar <-  SANS
  # cov <-  list(SAPS, SOFAS)
  # startval <- list(SANS = 500, YMRS = 1000)
  # output  <-  c("SAMPSTAT", "CINTERVAL")
  # model  <-  FINAL_model
  # test <- "S1 = S3"
  # gf <-  "S"
  # gfw <-  c("iw", "sw")
  
  
  # Re-format called arguments ----------------------------------------------
  cov_vec <- substitute(cov) %>%
    as.character() %>%
    tail(-1)
  
  # cov_dummy <- cov %>%  
  #   map_depth(1, #at
  #             \(x) purrr::map_if(x,#x
  #                                \(x) is.factor(df[[x]]), #p
  #                                \(x) purrr::map_chr(1:(nlevels(df[[x]]) - 1), #f, y
  #                                                    \(y) paste0(x, y)) %>% #f
  #                                  paste(collapse = " ")))
  
  # Extract model parameters and data ---------------------------------------
  logits_df <- model[["results"]][["class_counts"]][["logitProbs.mostLikely"]] #logits
  k <- model[["results"]][["summaries"]][["NLatentClasses"]] #classes
  lgmm <- model[["TITLE"]] #title
  gf <- as.list(gf) #growth factors
  gfw <- as.list(gfw) #wihtin-class growth factors
  
  savedata <-
    purrr::map(cov,
               \(z) merge(
                 x = model[["results"]][["savedata"]], #error when usevar %in% cov
                 y = select(df, all_of(c(idvar, z)), -any_of(usevar)),
                 by.x = stringr::str_to_upper(idvar),
                 by.y = idvar,
                 all = TRUE
               )) %>%
    purrr::map(rename, N = C) #data
  
  # Create each arguments for mplusObject() -------------------------------
  ## TITLE = 
  title <-  cov_vec %>% 
    purrr::map(~ glue::glue("{lgmm} - MixREG_{.x}")) %>%
    purrr::map(~ MplusAutomation::parseMplus(.x, add = TRUE))
  
  ## VARIABLE = 
  usevars <- cov %>% 
    purrr::map(\(x) paste(x, collapse = " ")) %>%
    purrr::map(\(x) paste("USEVAR =", "N", paste(gf, collapse = " "), x, collapse = " "))
  
  nominal <- "NOMINAL = N"
  
  classes <- glue::glue("CLASSES = c({k})")
  
  variable <- purrr::map(usevars, \(x) c(x, nominal, classes)) %>%
    purrr::map(~ MplusAutomation::parseMplus(.x, add = TRUE))

  
  ## ANALYSIS =
  type <- "TYPE = MIXTURE"
  
  starts <- purrr::map(seq_along(cov_vec), \(x) glue::glue("STARTS = 0"))
  names(starts) <- cov_vec
  
  if(!is.null(startval)) {
    SV <- purrr::map(startval, \(x) glue::glue("STARTS = {x}"))
    starts[names(SV)] <- SV
  }
  
  processors <- glue::glue('PROCESSORS = {detectCores()}')
  analysis <- purrr::map(starts, \(x) MplusAutomation::parseMplus(c(type, x, processors), add = TRUE))
  
  ## DEFINE =
  # define <- cov %>% 
  #   purrr::map_depth(1, #at
  #                    \(x) purrr::map_if(x, #x
  #                                       \(x) is.factor(df[[x]]), #p
  #                                       \(x) purrr::map_chr(1:(nlevels(df[[x]]) - 1), #f, y
  #                                                           \(y) glue::glue("{x}{y} = {x} == {y}")),
  #                                       .else = ~ NULL )) %>% 
  #   purrr::map_depth(1,
  #                    \(x) purrr::map_if(x,  
  #                                       \(x) !is.null(x), 
  #                                       \(x) MplusAutomation::parseMplus(x, add = TRUE))) %>% 
  #   purrr::map_if(\(x) !all(purrr::map_lgl(x, \(y) is.null(y))), 
  #                 \(x) paste(x, collapse = " "), 
  #                 .else = ~ NULL) %>% 
  #   purrr::map_if(\(x) !is.null(x),
  #                 \(x) gsub(";", ";\n", x))
  
  ## MODEL = 
  ### %WITHIN% class
  c <- glue::glue('%C#{1:k}%') #class sections
  
  logits <-  purrr::map(1:k, \(x) purrr::map2(x, 1:k, \(x, y) logits_df[x, y])) %>%
    unlist() %>%
    purrr::map2_chr(rep(1:k, k), ., \(x, y) glue::glue("[N#{x}@{y}]")) %>%
    split(rep(1:k, each = k)) %>%
    purrr::map(\(x) head(x, -1))  #logits specification
  
  timepoints <- purrr::map(cov, ~ parse_number(.x))
  
  gcm <- purrr::map2(cov, timepoints,
                     \(cov, timepoint) purrr::map2(cov, timepoint, 
                                                   \(cov, timepoint) glue::glue("{cov}@{timepoint}"))) %>% 
    purrr::map(~ paste(.x, collapse = " ")) %>% 
    purrr::map(~ paste(paste(gfw, collapse = " "), "|", .x)) %>% 
    purrr::map(~ strwrap(.x, width = 90, exdent = 0)) %>%
    purrr::map(~ paste(.x, collapse = "\n")) %>%
    purrr::map(~ gsub(";", ";\n",.x))
  
  cov_var <- purrr::map(cov, \(x) paste(x, collapse = " ")) #covariates variance specification
  
  gfw_mean <- glue::glue('[{gfw}]')
  gfw_var <- paste(gfw, collapse = " ")
  gfw_covar <- map(seq_along(gfw), 
               \(x) paste(gfw[x], "WITH", paste(gfw[-(1:x)], collapse = " "))) %>% head(-1)
    
  reg <- paste(gf, "ON", paste(gfw, collapse = " "))

  ### Model labels
  gfw_label <- purrr::map(1:k, 
                          ~ glue::glue('{gfw_mean} ({gfw}{.x})'))
  
  start <- seq(from = 1, 
               to = length(gfw) * k,
               by = length(gfw))
  
  end <- seq(from = length(gfw),
             to = length(gfw) * k,
             by = length(gfw))
  
  label <- purrr::map2(start, end,
                       \(x, y) purrr::map(gf, \(z) glue::glue("({z}{x} - {z}{y})")))
  
  reg_label <- purrr::map(label,
                           \(x) map2(x, reg, \(x, y) c(y, x))) %>%
    purrr::map_depth(2, ~ paste(.x, collapse = " "))
  
  ### Model specification
  overall <- purrr::map(gcm, ~ c('%OVERALL%', .x)) %>% 
    purrr::map(~ c(.x, reg))
  
  class <- list(c, logits, reg_label, gfw_label) %>% 
    purrr::pmap(\(c, logits, reg_label, gfw_label) c(c, logits, reg_label, gfw_label)) %>% 
    purrr::map(\(class) c(class, gfw_var, gfw_covar)) %>% 
    {purrr::map(cov_var, \(cov_var) purrr::map(., \(class) c(class, cov_var)))}
  
  model <- list(overall, class) %>%
    purrr::reduce(~ purrr::map2(.x, .y, ~ c(.x, .y))) %>%
    purrr::map(~ flatten(.x)) %>% 
    purrr::map(~ paste0(.x, ";")) %>% 
    purrr::map(~ strwrap(.x, width = 90, exdent = 0)) %>% 
    purrr::map(~ paste(.x, collapse = "\n")) %>% 
    purrr::map(~ gsub("%;", "%",.x)) #remove semicolon after model sections
  
  ## MODEL CONSTRAINTS =
  # diff <- purrr::map2(head(1:k, -1), tail(1:k, -1), ~ glue::glue("diff{.x}{.y}"))
  # diff <- if(k == 3){purrr::append(diff, purrr::map2(head(1:k, 1), tail(1:k, 1), ~ glue::glue("diff{.x}{.y}")))}
  # 
  # m <- purrr::map2(head(1:k, -1), tail(1:k, -1), ~ glue::glue(" = {test}{.x} - {test}{.y}"))
  # m <- if(k == 3){purrr::append(m, purrr::map2(head(1:k, 1), tail(1:k, 1), ~ glue::glue(" = {test}{.x} - {test}{.y}")))}
  # 
  # new <- glue::glue("New ({paste(diff, collapse = ' ')})")
  # 
  # constraint <- c(new,
  #                 purrr::map2(diff, m, \(x, y) c(x, y))) %>%
  #   purrr::map_chr(~ paste(.x, collapse = " ")) %>%
  #   MplusAutomation::parseMplus(add = TRUE)
  
  ## MODEL TEST =
  test <- MplusAutomation::parseMplus(test, add = TRUE)
  
  ## OUTPUT =
  outputs <- MplusAutomation::parseMplus(output, add = TRUE)
  
  # Create Mplus object -----------------------------------------------------
  mpobj <- purrr::pmap(list(title, variable, model, analysis, savedata),  \(title, variable, model, analysis, savedata)
                       MplusAutomation::mplusObject(
                         TITLE = title,
                         VARIABLE = variable,
                         #DEFINE = define, 
                         MODEL = model,
                         MODELTEST = test,
                         #MODELCONSTRAINT = constraint,
                         ANALYSIS = analysis,
                         OUTPUT = outputs,
                         autov = FALSE,
                         usevariables = colnames(savedata),
                         rdata = savedata)
  )
  
  # Create directory for Mplus data, inputs and outputs ---------------------
  path <- glue::glue(getwd(), substitute(usevar), 'Results', 'MixREG', .sep = "/")
  if (!dir.exists(path)) {dir.create(path, recursive = TRUE)}
  
  # Run Mplus models --------------------------------------------------------
  model_lst <- purrr::map2(mpobj, cov_vec, \(x, y)
                           MplusAutomation::mplusModeler(
                             object = x,
                             dataout = glue::glue("{path}/{y}_MixREG.dat"),
                             modelout = glue::glue("{path}/{y}_MixREG.inp"),
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

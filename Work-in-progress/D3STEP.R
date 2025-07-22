D3STEP <- function(df,
                   idvar,
                   usevar,
                   cov,
                   startval = NULL,
                   output = c("SAMPSTAT", "STANDARDIZED", "CINTERVAL", "TECH7", "TECH12"),
                   model) {
  # To do -------------------------------------------------------------------
  ## select cov-specific starts values
  ## select ref category for factors(?)
  ## add arguments for plots
  ## silence warning message from MplusAutomation::parseMplus

  # Test arguments ----------------------------------------------------------
  # df <- SOFAS_df
  # idvar <- "pin"
  # usevar <- SOFAS
  # cov <- cov_D3STEP
  # startval <- NULL
  # output <- c("SAMPSTAT", "CINTERVAL")
  # model <- FINAL_model

  # Extract model parameters and data ---------------------------------------
  logits_df <- model[["results"]][["class_counts"]][["logitProbs.mostLikely"]] # logits
  k <- model[["results"]][["summaries"]][["NLatentClasses"]] # classes
  lgmm <- model[["TITLE"]] # title

  savedata <-
    purrr::map(
      cov,
      \(z) merge(
        x = model[["results"]][["savedata"]],
        y = dplyr::select(df, all_of(c(idvar, z)), -any_of(usevar)),
        by.x = stringr::str_to_upper(idvar),
        by.y = idvar,
        all = TRUE
      )
    ) %>%
    purrr::map(~ dplyr::rename(.x, N = "C")) # data

  # Create each arguments for mplusObject() -------------------------------
  ## TITLE =
  title <- cov %>%
    purrr::map(~ glue::glue("D3STEP_{.x} - {lgmm}")) %>%
    purrr::map(~ MplusAutomation::parseMplus(.x, add = TRUE))

  ## VARIABLE =
  usevars <- cov %>%
    purrr::map(\(x) paste(x, collapse = " ")) %>%
    purrr::map(\(x) paste("USEVAR =", x, "N", collapse = " "))

  nominal <- "NOMINAL = N"

  categorical <- purrr::map_if(cov,
    ~ is.factor(df[[.x]]),
    ~ glue::glue("CATEGORICAL = {.x}"),
    .else = ~NULL
  )

  classes <- glue::glue("CLASSES = c({k})")

  variable <- purrr::map2(usevars, categorical, \(x, y) c(x, nominal, y, classes)) %>%
    purrr::map(~ MplusAutomation::parseMplus(.x, add = TRUE))
  # purrr::map(~ strwrap(.x, width = 90, exdent = 5)) %>%
  # purrr::map(~ paste(.x, collapse = "\n")) %>%
  # purrr::map(~ gsub(";", ";\n",.x))

  ## ANALYSIS =
  type <- "TYPE = MIXTURE"
  algorithm <- purrr::map_if(cov,
    ~ is.factor(df[[.x]]),
    ~ glue::glue("ALGORITHM = INTEGRATION"),
    .else = ~NULL
  )

  starts <- purrr::map(seq_along(cov), \(x) glue::glue("STARTS = 0"))
  names(starts) <- cov

  if (!is.null(startval)) {
    SV <- purrr::map(startval, \(x) glue::glue("STARTS = {x} {x / 4}"))
    starts[names(SV)] <- SV
  }

  processors <- glue::glue("PROCESSORS = {detectCores()}")
  analysis <- purrr::map2(algorithm, starts, \(x, y) c(type, x, y, processors)) %>%
    purrr::map(~ MplusAutomation::parseMplus(.x, add = TRUE))

  ## MODEL =
  class_spec <- purrr::map_chr(1:k, ~ glue::glue("%C#{.x}%")) # class sections

  logits <- purrr::map(1:k, \(x) purrr::map2(x, 1:k, \(x, y) logits_df[x, y])) %>%
    unlist() %>%
    purrr::map2_chr(rep(1:k, k), ., \(x, y) glue::glue("[N#{x}@{y}]")) %>%
    split(rep(1:k, each = k)) %>%
    purrr::map(\(x) utils::head(x, -1)) # logits specification

  reg <- purrr::map(
    1:k,
    \(x) purrr::map_if(cov,
      \(y) is.factor(df[[y]]),
      \(y) glue::glue("[{y}$1](M{x})"),
      .else = \(y) glue::glue("[{y}](M{x}); {y}")
    )
  ) %>%
    purrr::transpose()

  ### Model specification
  model1 <- list(class_spec, logits) %>%
    purrr::reduce(~ purrr::map2(.x, .y, ~ c(.x, .y)))

  model2 <- purrr::map(reg, \(x) purrr::map2(x, model1, \(x, y) c(y, x)))

  model3 <- list("%OVERALL%", model2) %>%
    purrr::reduce(~ purrr::map2(.x, .y, ~ c(.x, .y))) %>%
    purrr::map(~ MplusAutomation::parseMplus(unlist(.x), add = TRUE)) %>%
    purrr::map(~ gsub("%;", "%", .x)) # remove semicolon after model sections


  ## MODEL CONSTRAINTS =
  diff <- expand.grid(seq(k), seq(k)) %>%
    filter(Var1 != Var2) %>%
    arrange(Var1, Var2) %>% 
    mutate(newdiff = paste0("diff", Var1, Var2))

  form <- glue::glue("{diff$newdiff} = M{diff$Var1} - M{diff$Var2}")
  
  new <- glue::glue("New ({paste(diff$newdiff, collapse = ' ')})")

  constraint <- c(new, form) %>%
    purrr::map_chr(~ paste(.x, collapse = " ")) %>%
    MplusAutomation::parseMplus(add = TRUE)

  # probs <- glue::glue("prob{seq(k)}")
  # form1 <- glue::glue("{probs} =  1 / (1 + exp(M{seq(k)}))") %>% as.list
  #
  # odds <- glue::glue("odd{seq(k)}")
  # form2 <- glue::glue("{odds} =  prob{seq(k)} / (1 - prob{seq(k)})") %>% as.list
  #
  # ors <- expand.grid(seq(k), seq(k)) %>%
  #   filter(Var1 < Var2) %>%
  #   mutate(ors = paste0('or', Var1, Var2))
  # form3 <- glue::glue("{ors$ors} =  odd{ors$Var1} / odd{ors$Var2}") %>% as.list
  #
  # new <-  glue::glue("New ({paste(c(diff$newdiff, probs, odds, ors$ors), collapse = ' ')})")
  #
  # constraint <- c(new, form0, form1, form2, form3) %>%
  #    purrr::map_chr(~ paste(.x, collapse = " ")) %>%
  #    MplusAutomation::parseMplus(add = TRUE)
  #    # purrr::map(~ strwrap(.x, width = 90, exdent = 5)) %>%
  #    # purrr::map(~ paste(.x, collapse = "\n")) %>%
  #    # purrr::map_chr(~ gsub(";", ";\n",.x))

  ## MODEL TEST =
  test <- purrr::map_chr(head(1:k, -1), ~ glue::glue("M{.x} = M{.x+1}")) %>%
    MplusAutomation::parseMplus(add = TRUE)

  ## OUTPUT =
  outputs <- MplusAutomation::parseMplus(output, add = TRUE)

  # Create Mplus object -----------------------------------------------------
  mpobj <- purrr::pmap(list(title, variable, model3, analysis, savedata), \(title, variable, model, analysis, savedata)
  MplusAutomation::mplusObject(
    TITLE = title,
    VARIABLE = variable,
    MODEL = model,
    MODELTEST = test,
    MODELCONSTRAINT = constraint,
    ANALYSIS = analysis,
    OUTPUT = outputs,
    autov = FALSE,
    usevariables = colnames(savedata),
    rdata = savedata
  ))

  # Create directory for Mplus data, inputs and outputs ---------------------
  path <- glue::glue(getwd(), substitute(usevar), "Results", "D3STEP", .sep = "/")
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  # Run Mplus models --------------------------------------------------------
  model_lst <- purrr::map2(mpobj, cov, \(x, y)
  MplusAutomation::mplusModeler(
    object = x,
    dataout = glue::glue("{path}/{y}_D3STEP.dat"),
    modelout = glue::glue("{path}/{y}_D3STEP.inp"),
    hashfilename = FALSE,
    run = 1,
    check = FALSE,
    writeData = "always",
    quiet = TRUE
  ))

  names(model_lst) <- cov

  return(model_lst)
}

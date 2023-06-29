MixREG <- function(df,
                   idvar,
                   usevar,
                   cov,
                   startval = NULL,
                   gf = "S",
                   gfw = c("iw", "sw"),
                   test = NULL,
                   output = c("SAMPSTAT", "STANDARDIZED", "CINTERVAL"),
                   model) {
  # To do -------------------------------------------------------------------
  ## add Wald test for I on gfw
  ## add arguments for plots
  ## silence warning message from MplusAutomation::parseMplus

  # Test arguments ----------------------------------------------------------
  # df <-  SAPS_df
  # idvar  <-  'pin'
  # usevar <-  SAPS
  # cov <-  list(SAPS, SOFAS, NSR)
  # startval <- list(SAPS = 500, SOFAS = 1000)
  # gf <-  "S"
  # gfw <-  c("iw", "sw")
  # test <- "S_sw"
  # output  <-  c("SAMPSTAT", "CINTERVAL")
  # model  <-  FINAL_model

  stopifnot(gf %in% c("I", "S", "Q", "CUB"))
  stopifnot(gfw %in% c("iw", "sw", "qw", "cubw"))

  # Re-format called arguments ----------------------------------------------
  cov_vec <- substitute(cov) %>%
    as.character() %>%
    tail(-1)

  # Extract model parameters and data ---------------------------------------
  logits_df <- pluck(model, "results", "class_counts", "logitProbs.mostLikely") # logits
  k <- pluck(model, "results", "summaries", "NLatentClasses") # classes
  lgmm <- model[["TITLE"]] # title
  gf <- as.list(gf) # growth factors
  gfw <- as.list(gfw) # wihtin-class growth factors

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
  title <- cov_vec %>%
    purrr::map(~ glue::glue("{lgmm} - MixREG_{.x}")) %>%
    purrr::map(~ MplusAutomation::parseMplus(.x, add = TRUE))

  ## VARIABLE =
  usevars <- cov %>%
    purrr::map(\(x) paste(x, collapse = " ")) %>%
    purrr::map(\(x) paste("USEVAR =", "N", paste(gf, collapse = " "), x, collapse = " "))

  nominal <- "NOMINAL = N"

  categorical <- purrr::map_if(cov,
    ~ is.factor(df[[.x[[1]]]]),
    ~ glue::glue("CATEGORICAL = {paste(.x, collapse = ' ')}"),
    .else = ~NULL
  )

  classes <- glue::glue("CLASSES = c({k})")

  variable <- purrr::map2(usevars, categorical, \(x, y) c(x, y, nominal, classes)) %>%
    purrr::map(~ MplusAutomation::parseMplus(.x, add = TRUE))

  # variable <- purrr::map(usevars, \(x) c(x, nominal, classes)) %>%
  #   purrr::map(~ MplusAutomation::parseMplus(.x, add = TRUE))

  ## ANALYSIS =
  type <- "TYPE = MIXTURE"

  # algorithm <- purrr::map_if(cov,
  #                            ~ is.factor(df[[.x[[1]][[1]]]]),
  #                            ~ glue::glue("ALGORITHM = INTEGRATION"),
  #                            .else = ~ NULL)

  algorithm <- purrr::map_if(cov,
    ~ is.factor(df[[.x[[1]]]]),
    ~ glue::glue("ALGORITHM = INTEGRATION"),
    .else = ~NULL
  )

  link <- purrr::map_if(cov,
    ~ is.factor(df[[.x[[1]]]]),
    ~ glue::glue("LINK = LOGIT"),
    .else = ~NULL
  )

  starts <- purrr::map(seq_along(cov_vec), \(x) glue::glue("STARTS = 0"))
  names(starts) <- cov_vec

  if (!is.null(startval)) {
    SV <- purrr::map(startval, \(x) glue::glue("STARTS = {x}"))
    starts[names(SV)] <- SV
  }

  processors <- glue::glue("PROCESSORS = {detectCores()}")
  analysis <- list(algorithm, link, starts) %>%
    purrr::pmap(\(x, y, z) c(type, x, y, z, processors)) %>%
    purrr::map(~ MplusAutomation::parseMplus(.x, add = TRUE))
  # analysis <- purrr::map(starts, \(x) MplusAutomation::parseMplus(c(type, x, processors), add = TRUE))


  ## MODEL =
  ### %WITHIN% class
  c <- glue::glue("%C#{1:k}%") # class sections

  logits <- purrr::map(1:k, \(x) purrr::map2(x, 1:k, \(x, y) logits_df[x, y])) %>%
    unlist() %>%
    purrr::map2_chr(rep(1:k, k), ., \(x, y) glue::glue("[N#{x}@{y}]")) %>%
    split(rep(1:k, each = k)) %>%
    purrr::map(\(x) head(x, -1)) # logits specification

  timepoints <- purrr::map(cov, ~ parse_number(.x))

  gcm <- purrr::map2(
    cov, timepoints,
    \(cov, timepoint) purrr::map2(
      cov, timepoint,
      \(cov, timepoint) glue::glue("{cov}@{timepoint}")
    )
  ) %>%
    purrr::map(~ paste(.x, collapse = " ")) %>%
    purrr::map(~ paste(paste(gfw, collapse = " "), "|", .x)) %>%
    purrr::map(~ strwrap(.x, width = 90, exdent = 0)) %>%
    purrr::map(~ paste(.x, collapse = "\n")) %>%
    purrr::map(~ gsub(";", ";\n", .x))

  cov_var <- purrr::map_if(cov,
    ~ !is.factor(df[[.x[[1]]]]),
    ~ paste(.x, collapse = " "),
    .else = ~NULL
  )
  # cov_var <- purrr::map(cov, \(x) paste(x, collapse = " ")) # covariates variance specification (to remove if using CATEGORICAL option)

  gfw_mean <- glue::glue("[{gfw}]")
  gfw_var <- paste(gfw, collapse = " ")
  gfw_covar <- map(
    seq_along(gfw),
    \(x) paste(gfw[x], "WITH", paste(gfw[-(1:x)], collapse = " "))
  ) %>% head(-1)

  reg <- paste(gf, "ON", paste(gfw, collapse = " "))

  ### Model labels
  gfw_label <- purrr::map(
    1:k,
    ~ glue::glue("{gfw_mean} ({gfw}{.x})")
  )

  start <- seq(
    from = 1,
    to = length(gfw) * k,
    by = length(gfw)
  )

  end <- seq(
    from = length(gfw),
    to = length(gfw) * k,
    by = length(gfw)
  )

  label <- purrr::map2(
    start, end,
    \(x, y) purrr::map(gf, \(z) glue::glue("({z}{x} - {z}{y})"))
  )

  reg_label <- purrr::map(
    label,
    \(x) map2(x, reg, \(x, y) c(y, x))
  ) %>%
    purrr::map_depth(2, ~ paste(.x, collapse = " "))

  ### Model specification
  overall <- purrr::map(gcm, ~ c("%OVERALL%", .x)) %>%
    purrr::map(~ c(.x, reg))

  class <- list(c, logits, reg_label, gfw_label) %>%
    purrr::pmap(\(c, logits, reg_label, gfw_label) c(c, logits, reg_label, gfw_label)) %>%
    purrr::map(\(class) c(class, gfw_var, gfw_covar)) %>%
    {
      purrr::map(cov_var, \(cov_var) purrr::map(., \(class) c(class, cov_var)))
    }

  model <- list(overall, class) %>%
    purrr::reduce(~ purrr::map2(.x, .y, ~ c(.x, .y))) %>%
    purrr::map(~ flatten(.x)) %>%
    purrr::map(~ paste0(.x, ";")) %>%
    purrr::map(~ strwrap(.x, width = 90, exdent = 0)) %>%
    purrr::map(~ paste(.x, collapse = "\n")) %>%
    purrr::map(~ gsub("%;", "%", .x)) # remove semicolon after model sections

  ## MODEL CONSTRAINTS =
  if (test %in% gfw) {
    diff <- as.list(gfw) %>%
      map(~ mutate(expand.grid(seq(k), seq(k)), newdiff = glue("{.x}_{Var1}{Var2}"))) %>%
      map(~ filter(.x, Var1 < Var2))

    form0 <- map2(diff, gfw, \(x, y) glue::glue("{x$newdiff} = {y}{x$Var1} - {y}{x$Var2}"))

    new <- bind_rows(diff) %>%
      pull(newdiff) %>%
      {
        glue::glue("New ({paste(., collapse = ' ')})")
      }

    constraint <- c(new, form0) %>%
      purrr::map_chr(~ paste(.x, collapse = " ")) %>%
      MplusAutomation::parseMplus(add = TRUE)
  } else {
    constraint <- NULL
  }

  ## MODEL TEST =
  if (test %in% c("S_iw", "S_sw", "S_qw", "S_cubw")) {
    t <- switch(test,
      "S_iw" = 1,
      "S_sw" = 2,
      "S_qw" = 3,
      "S_cubw" = 4
    )

    mtest <- purrr::map_chr(
      head(
        seq(
          from = t,
          to = k * length(gfw),
          by = length(gfw),
        ),
        -1
      ), ~ glue::glue("S{.x} = S{.x+length(gfw)}")
    )
  } else if (test %in% gfw) {
    mtest <- purrr::map_chr(head(1:k, -1), ~ glue::glue("{test}{.x} = {test}{.x+1}"))
  }

  mtest <- MplusAutomation::parseMplus(mtest, add = TRUE)

  ## OUTPUT =
  outputs <- MplusAutomation::parseMplus(output, add = TRUE)

  # Create Mplus object -----------------------------------------------------
  mpobj <- purrr::pmap(list(title, variable, model, analysis, savedata), \(title, variable, model, analysis, savedata)
  MplusAutomation::mplusObject(
    TITLE = title,
    VARIABLE = variable,
    # DEFINE = define,
    MODEL = model,
    MODELTEST = mtest,
    MODELCONSTRAINT = constraint,
    ANALYSIS = analysis,
    OUTPUT = outputs,
    autov = FALSE,
    usevariables = colnames(savedata),
    rdata = savedata
  ))

  # Create directory for Mplus data, inputs and outputs ---------------------
  path <- glue::glue(getwd(), substitute(usevar), "Results", "MixREG", .sep = "/")
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

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
    quiet = FALSE
  ))

  names(model_lst) <- cov_vec

  return(model_lst)
}

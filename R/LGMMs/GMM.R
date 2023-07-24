GMM <- function(df,
                idvar,
                usevar,
                k,
                startval = 500,
                overall_polynomial,
                random_effect,
                output = c("TECH7", "TECH11", "SAMPSTAT", "STANDARDIZED", "CINTERVAL"),
                working_dir = getwd()) {
  # Test Arguments ----------------------------------------------------------
  # df <- SOFAS_df
  # idvar <- "pin"
  # usevar <- SOFAS
  # k <- 2
  # startval <- 500
  # random_effect <- "CI"
  # overall_polynomial <- 3
  # output <-  c("TECH7", "TECH11", "SAMPSTAT", "STANDARDIZED", "CINTERVAL")
  # working_dir <-  getwd()

  outcome <- stringr::str_remove_all(usevar[1], "[^[:alpha:]]")

  # Input validation
  stopifnot(
    is.data.frame(df),
    is.vector(usevar)
  )

  gf_var <- switch(overall_polynomial,
    "1" = c("i s@0", "i s"),
    "2" = c("i s@0 q@0", "i s q@0", "i s q"),
    "3" = c("i s@0 q@0 cub@0", "i s q@0 cub@0", "i s q cub@0", "i s q cub")
  )

  # Create each arguments for mplusObject() -------------------------------
  ## TITLE = --------------------------------------------------------------
  title <- purrr::map(
    gf_var,
    \(gf_var) purrr::map(
      c("FIX", "C", "T", "CT"),
      \(resvar) paste("GMM", random_effect, stringr::str_remove_all(gf_var, "\\s"), resvar, sep = "_")
    )
  ) %>%
    # map_depth(2, ~ MplusAutomation::parseMplus(.x, add = TRUE)) %>%
    purrr::flatten()

  ## VARIABLE = -----------------------------------------------------------
  usevars <- paste("USEVAR =", paste(usevar, collapse = " "))
  idvars <- paste("IDVAR =", idvar)
  classes <- glue::glue("CLASSES = c({k})")

  variable <- c(usevars, idvars, classes) %>%
    MplusAutomation::parseMplus(add = TRUE)

  ## ANALYSIS =
  type <- "TYPE = MIXTURE"
  starts <- paste("STARTS =", startval, startval / 4)
  k1starts <- paste("K-1STARTS =", startval / 2, startval / 8)
  processors <- paste("PROCESSORS =", parallel::detectCores())

  analysis <- c(type, starts, k1starts, processors) %>%
    MplusAutomation::parseMplus(add = TRUE)

  ## MODEL = --------------------------------------------------------------
  ### Growth model
  gf <- switch(overall_polynomial,
    "1" = c("i", "s"),
    "2" = c("i", "s", "q"),
    "3" = c("i", "s", "q", "cub")
  )

  timepoints <- readr::parse_number(usevar)

  gm <- purrr::map2_chr(usevar, timepoints, \(usevar, timepoints) glue::glue("{usevar}@{timepoints}")) %>%
    paste(., collapse = " ") %>%
    paste0(paste(gf, collapse = " "), " | ", .) %>%
    strwrap(width = 90, exdent = 0) %>%
    paste(collapse = "\n")

  ### Variance specification
  gf_covar <- purrr::map(seq_along(gf), \(x) paste(gf[x], "WITH", paste(gf[-(1:x)], collapse = " "))) %>%
    head(-1)

  resvarFIX <- glue::glue("{paste(usevar, collapse = ' ')} (1)") %>%
    rep(k) %>%
    as.list()

  resvarC <- map(seq(k), \(k) glue::glue("{paste(usevar, collapse = ' ')} ({k})"))

  resvarT <- purrr::imap_chr(usevar, \(x, idx) glue::glue("{x} ({idx})")) %>%
    rep(k) %>%
    split(rep(1:k, each = length(usevar)))

  resvarCT <- rep(usevar, k) %>%
    purrr::imap(\(x, idx) glue::glue("{x} ({idx})")) %>%
    split(rep(1:k, each = length(usevar)))

  resid_var <- list(resvarFIX, resvarC, resvarT, resvarCT)

  ### Model specification
  overall <- purrr::map(
    gf_var,
    \(gf_var) c("%OVERALL%", gm, gf_var, gf_covar, paste(usevar, collapse = " "))
  )

  c <- glue::glue("%C#{1:k}%") # class sections

  random_effect <- match.arg(random_effect, c("CI", "CV"))

  if (random_effect == "CI") {
    class <- purrr::map(
      resid_var,
      \(resid_var) purrr::map2(
        c, resid_var,
        \(c, resid_var) c(c, resid_var)
      )
    )

    model <- purrr::map(
      overall,
      \(overall) purrr::map(class, \(class) c(overall, class))
    )
  } else if (random_effect == "CV") {
    class <- purrr::map(
      gf_var,
      \(gf_var) purrr::map(
        resid_var,
        \(resid_var) purrr::map2(
          c, resid_var,
          \(c, resid_var) c(c, gf_var, gf_covar, resid_var)
        )
      )
    )

    model <- purrr::map2(
      overall, class,
      \(overall, class) purrr::map(class, \(class) c(overall, class))
    )
  }

  model <- model %>%
    purrr::map_depth(2, ~ paste0(unlist(.x), ";")) %>%
    purrr::map_depth(2, ~ gsub("%;", "%", .x)) %>%
    flatten()

  ## OUTPUT = ----------------------------------------------------------------
  output <- MplusAutomation::parseMplus(output, add = TRUE)

  ## SAVEDATA = --------------------------------------------------------------
  file <- purrr::map(title, \(title) glue::glue("FILE = {title}_CP.dat"))
  save <- "SAVE = CPROBABILITIES"

  savedata <- purrr::map(file, \(file) c(file, save)) %>%
    purrr::map(~ MplusAutomation::parseMplus(.x, add = TRUE))

  # Create Mplus object -----------------------------------------------------
  mpobj <- list(title, model, savedata) %>%
    purrr::pmap(\(title, model, savedata)
    MplusAutomation::mplusObject(
      TITLE = title,
      VARIABLE = variable,
      MODEL = model,
      ANALYSIS = analysis,
      OUTPUT = output,
      SAVEDATA = savedata,
      autov = FALSE,
      usevariables = names(select(df, tidyselect::all_of(c(idvar, usevar)))),
      rdata = df
    ))

  # Create directory for results if does not already exist
  model_dir <- purrr::map_chr(
    gf_var,
    \(gf_var) glue::glue("{working_dir}", "{outcome}", "Results", "GMM", "{random_effect}", "{str_remove_all(gf_var, '[[:blank:]]')}", .sep = "/")
  ) %>%
    purrr::walk(~ ifelse(!dir.exists(.x), dir.create(.x, recursive = TRUE), .x)) %>%
    purrr::map(~ rep(.x, each = 4)) %>%
    purrr::flatten()

  # Run the model
  model_lst <- list(mpobj, model_dir, seq_along(mpobj)) %>%
    purrr::pmap(\(mpobj, model_dir, name)
    MplusAutomation::mplusModeler(
      object = mpobj,
      dataout = glue::glue("{model_dir}/GMM{name}.dat"),
      modelout = glue::glue("{model_dir}/GMM{name}.inp"),
      hashfilename = FALSE,
      run = 1,
      check = FALSE,
      writeData = "always",
      quiet = FALSE
    ))

  names(model_lst) <- purrr::map(title, ~ stringr::str_to_upper(.x))

  return(model_lst)
}

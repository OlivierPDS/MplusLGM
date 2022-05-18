fitGMMv <- function(df,
                    usevar,
                    list_mpobj,
                    overall_polynomial,
                    working_dir = getwd()) {
  
   
  # Input validation
  stopifnot(overall_polynomial <= 3)
  
  ### Create Mplus Object
  mpobj_2 <- list()
  mpobj_1 <- list()
  
  if (overall_polynomial == 1) {
    growth_factors <- c('i s@0', 'i s')
  } else if (overall_polynomial == 2) {
    growth_factors <- c('i s-q@0', 'i s q@0', 'i s q')
  } else if (overall_polynomial == 3) {
    growth_factors <-
      c('i s-cub@0', 'i s q-cub@0', 'i s q cub@0', 'i s q cub')
  } else {
    stop('Error: Does not currently support model with polynomial order higher than 3 (cubic models)')
  }
  
  for (rv in 1:4) {
    for (gf in growth_factors) {
      mpobj_2[[gf]] <- update(
        list_mpobj[[rv]][[gf]],
        TITLE = as.formula(glue("~ 'GMM{rv}v_{gf};'")),
        SAVEDATA = as.formula(
          glue("~ '
        FILE = GMM{rv}v_{gf}_res.dat;
        SAVE = CPROBABILITIES;'")
        ),
        autov = FALSE,
        rdata = df
      )
      mpobj_2[[gf]][["MODEL"]] <- str_replace_all(list_mpobj[[rv]][[gf]][["MODEL"]], "\\[i s q[:space:]?[:alpha:]*\\]", gf)
    }
    mpobj_1[[rv]] <- mpobj_2
    
    # Create directory for results if does not already exist
    model_dir <- glue::glue('{working_dir}', '{usevar}', 'Results', 'GMMv', 'GMM{rv}', .sep = "/")
    if (!dir.exists(model_dir)) {
      dir.create(model_dir, recursive = TRUE)
    }
  }
  
  ### Create, run, and read Mplus models
  GMMv_models <- list()
  GMMv <- list()
  
  for (rv in 1:4) {
    for (gf in growth_factors) {
      
      GMMv[[gf]] <- mplusModeler(
        object = mpobj_1[[rv]][[gf]],
        dataout = glue(getwd(), '/{usevar}/Results/GMMv/GMM{rv}/GMM{rv}v_{gf}.dat'),
        modelout = glue(getwd(), '/{usevar}/Results/GMMv/GMM{rv}/GMM{rv}v_{gf}.inp'),
        hashfilename = FALSE,
        run = 1,
        check = TRUE,
        varwarnings = TRUE,
        writeData = "always"
      )
    }
    GMMv_models[[rv]] <- GMMv
  }
  return(GMMv_models)
}
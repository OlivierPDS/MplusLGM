
R3STEP_models <- R3STEP(
  df = SAPS_df,
  idvar = 'pin',
  usevar = 'SAPS',
  cov = c(SD_num, SD_cat),
  model = FINAL_model, 
  method = 'manual'
)

list_mpobj <- R3STEP_models

alt_param <- readModels(target=str_c(getwd(), '/SAPS/Results/R3STEP/R3STEPm_gender.out')) %>% paramExtract("regression")

output <- R3STEPm_models[["gender"]][["results"]][["output"]]



MixREG_models <- MixREG(
  df = SAPS_df,
  idvar = 'pin',
  usevar = 'SAPS',
  cov = c('SANS_0', 'gender'),
  model = FINAL_model
)

list_mpobj <- MixREG_models

MixREG_fit <- MixREGfit(MixREG_models, c('SANS_0', "gender"))

D3STEPm_models <- D3STEPm(
  df = SAPS_df,
  idvar = 'pin',
  usevar = 'SAPS',
  cov = c('SAPS_24', "work"),
  model = FINAL_model
)

list_mpobj <- D3STEPm_models

D3STEPm_fit <- D3STEPfit(D3STEPm_models, c('SAPS_24', "work"))

df <- SOFAS_df
usevar <- 'SOFAS'
idvar <- 'pin'
timepoints <- c(0, 12, 24)
p <- 2

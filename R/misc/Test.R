
MixREG_models <- MixREG(
  df = SANS_df,
  idvar = 'pin',
  usevar = 'SANS',
  cov = c('SAPS_0', 'SUD'),
  model = FINAL_model
)

MixREG_fit <- MixREGfit(MixREG_models, c('SANS_24', "PSR_24"))


D3STEPm_models <- D3STEPm(
  df = SANS_df,
  idvar = 'pin',
  usevar = 'SANS',
  cov = c('SOFAS_24', 'SAPS_24', "PSR_24", "work"),
  model = FINAL_model
)



D3STEPm_fit <- D3STEPfit(D3STEPm_models, c('SAPS_24', "work"))


i <- 'SAPS_24'
i <- 'work'
cov <- c('SAPS_24', "work")
list_mpobj <- D3STEPm_models

df <- SANS_df
usevar <- 'SANS'
idvar <- 'pin'
timepoints <- c(0, 12, 24)
p <- 3

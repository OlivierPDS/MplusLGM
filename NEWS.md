# MplusLGM (development version)
- bugfix: `getPoly`- edit p-value threshold for statistical significance
- bugfix: `getFit`- remove sorting on NLatentclass because of GCM
- bugfix: `runLGM` - fixed tryCatch to return NULL instead of LL_m2 when no error

# MplusLGM 1.0.0
- data: `symptoms` is a simulated dataset of longitudinal assessment of symptom severity
- function: `LGMobject` to create Mplus model objects for latent growth modelling.
- function: `runLGM` to run latent growth models and replicate the best loglikelihood value.
- function: `getSpaghetti` to plot individual trajectories of outcome.
- function: `fitGCM` to fit growth curve models
- function: `fitGBTM` to fit group based trajectory models for class enumeration.
- function: `fitLCGA` to fit Latent class growth analysis models to refine covariance structure.
- function: `getFit` to get fit indices from latent growth models.
- function: `getBest` to select best-fitting model from a list of latent growth models.
- function: `getPoly` to refine the polynomial order of latent growth models.

* Initial CRAN submission.

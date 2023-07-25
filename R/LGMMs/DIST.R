DIST <- function(mpobj, usevar, dist = c("TDIST", "SKEWNORMAL", "SKEWT")) {
  

  dist <- match.arg(dist)
  
  mpobj <- mpobj %>%
    update(ANALYSIS = as.formula(glue("~ . + 'DISTRIBUTION = {dist};'"))
           ) %>%
    mplusModeler(
      object = .,
      dataout = glue(getwd(), "/{usevar}/Results/FINAL/DIST_{dist}.dat"),
      modelout = glue(getwd(), "/{usevar}/Results/FINAL/DIST_{dist}.inp"),
      hashfilename = FALSE,
      run = 1,
      check = TRUE,
      varwarnings = TRUE,
      writeData = "always"
    )

  return(mpobj)
}

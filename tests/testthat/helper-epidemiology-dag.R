# A realistic epidemiology DAG, the effect of smoking on lung cancer, drawn
# by the defaults tests and by the ggarrow resection tests, whose fixture of
# routed paths under an explicit cap is pinned on it.
epidemiology_dag <- function() {
  dagify(
    cancer ~ smoking + tar + age + ses + genetics + diet + occupation,
    tar ~ smoking,
    smoking ~ age + ses,
    diet ~ ses,
    occupation ~ ses,
    exposure = "smoking",
    outcome = "cancer",
    labels = c(
      age = "Age",
      ses = "Socioeconomic status",
      smoking = "Smoking",
      diet = "Diet",
      genetics = "Genetic risk",
      tar = "Tar in the lungs",
      occupation = "Occupation",
      cancer = "Lung cancer"
    )
  )
}

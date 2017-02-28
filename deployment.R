## update and redeploy shiny app
library(rsconnect)
library(shinyUtils) #devtools::install_github(repo = "Chr96er/shinyUtils")


#'@export
deployShinyApp <-
  function(applicationDirectory,
           appTitle,
           dev = T,
           buildLevel = c("build", "minor", "major", "release")) {
  incVersion(directory = applicationDirectory,
               level = buildLevel,
               build = T)
    setDeploymentTime(applicationDirectory)
    setAccountInfo(name = accountName,
                   token = accountToken,
                   secret = accountSecrect)
    deployApp(
      appDir = applicationDirectory,
      account = accountName,
      appName = appTitle
    )
  }
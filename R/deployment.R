## update and redeploy shiny app
library(rsconnect)

#'@export
deployShinyApp <-
  function(applicationDirectory,
           appTitle,
           dev = T,
           buildLevel = c("build", "minor", "major", "release"),
           accountName, accountToken, accountSecret) {
  incVersion(directory = applicationDirectory,
               level = buildLevel,
               build = T)
    setDeploymentTime(applicationDirectory)
    rsconnect::setAccountInfo(name = accountName,
                   token = accountToken,
                   secret = accountSecrect)
    rsconnect::deployApp(
      appDir = applicationDirectory,
      account = accountName,
      appName = appTitle
    )
  }
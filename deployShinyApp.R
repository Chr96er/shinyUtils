## update and redeploy shiny app
library(rsconnect)

deployShinyApp <-
  function(applicationDirectory,
           appTitle,
           dev = T,
           buildLevel = c("build", "feature", "stable", "release"),
           resetSubLevel = T,
           build = T,
           version = F,
           accountName, accountToken, accountSecret) {
  incVersion(directory = applicationDirectory,
               level = buildLevel,
               resetSubLevel,
               build,
               version)
    setDeploymentTime(applicationDirectory)
    rsconnect::setAccountInfo(name = accountName,
                   token = accountToken,
                   secret = accountSecret)
    rsconnect::deployApp(
      appDir = applicationDirectory,
      account = accountName,
      appName = appTitle
    )
  }
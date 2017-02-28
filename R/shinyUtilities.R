library(shiny)

manual <- function(text) {
  h4(text, style = "font-style: italic;
     font-weight: 20; line-height: 1;
     color: #888888;")
}

htmlStyle <- function() {
  tags$head(tags$style("html * {font-family: palanquin;}"))
}

getVersion <- function(){
  strsplit(strsplit(gsub(".VERSION", replacement = "", dir()[grep(dir(), pattern = "VERSION")]), "-")[[1]][1],"\\.")[[1]]
}

getDeploymentDate <- function(){
  deploymentDatetime <- strsplit(strsplit(gsub(".VERSION", replacement = "", dir()[grep(dir(), pattern = "VERSION")]), "-")[[1]][2],"#")[[1]]
  paste0(strftime(strptime(deploymentDatetime[1], format = "%Y.%m.%d.%H.%M.%S"),format = "%Y.%m.%d %H:%M:%S")," ", gsub("\\.", "/", deploymentDatetime[2]))
}
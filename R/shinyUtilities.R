#'@export
manual <- function(text) {
  shiny::h4(text, style = "font-style: italic;
     font-weight: 20; line-height: 1;
     color: #888888;")
}

#'@export
htmlStyle <- function() {
  shiny::tags$head(shiny::tags$style("html * {font-family: palanquin;}"))
}

#'@export
getVersion <- function(){
  strsplit(strsplit(gsub(".VERSION", replacement = "", dir()[grep(dir(), pattern = "VERSION")]), "-")[[1]][1],"\\.")[[1]]
}

#'@export
getDeploymentDate <- function(){
  deploymentDatetime <- strsplit(strsplit(gsub(".VERSION", replacement = "", dir()[grep(dir(), pattern = "VERSION")]), "-")[[1]][2],"#")[[1]]
  paste0(strftime(strptime(deploymentDatetime[1], format = "%Y.%m.%d.%H.%M.%S"),format = "%Y.%m.%d %H:%M:%S")," ", gsub("\\.", "/", deploymentDatetime[2]))
}
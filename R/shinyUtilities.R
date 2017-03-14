#'@import shiny
library(shiny)

#'@export
manual <- function(text) {
  h4(text, style = "font-style: italic;
     font-weight: 20; line-height: 1;
     color: #888888;")
}

#'@export
htmlStyle <- function() {
  tags$head(tags$style("html * {font-family: palanquin;}"))
}

#'@export
styledDiv <- function(text, style) {
  switch(style,
         italic = tags$div(HTML(paste(
           tags$span(style = "font-style: italic", text), sep = ""
         ))),
         bold = tags$div(HTML(paste(
           tags$span(style = "font-weight: bold", text), sep = ""
         ))))
}

#'@export
getVersion <- function() {
  strsplit(strsplit(gsub(".VERSION", replacement = "", dir()[grep(dir(), pattern = "VERSION")]), "-")[[1]][1], "\\.")[[1]]
}

#'@export
getDeploymentDate <- function() {
  deploymentDatetime <-
    strsplit(strsplit(gsub(".VERSION", replacement = "", dir()[grep(dir(), pattern = "VERSION")]), "-")[[1]][2], "#")[[1]]
  paste0(
    strftime(
      strptime(deploymentDatetime[1], format = "%Y.%m.%d.%H.%M.%S"),
      format = "%Y.%m.%d %H:%M:%S"
    ),
    " ",
    gsub("\\.", "/", deploymentDatetime[2])
  )
}

#'@title renderVersion
#'@name renderVersion
#'@param url Url to source code repository
#'@export
renderVersion <- function(url) {
  version <- getVersion()
  deploymentDate <- getDeploymentDate()
  
  list(
    tags$p(paste0("Source code available under ", url),
           align = "right"),
    tags$p(
      "Version ",
      paste(version[1:3], collapse = "."),
      "; build ",
      version[4],
      " (last deployment: ",
      deploymentDate,
      ")",
      align = "right"
    )
  )
}
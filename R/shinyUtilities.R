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

#'@title insertTab
#'@name insertTab
#'@param id Unique html id of element to be inserted.
#'@param title Displayed tab title.
#'@param renderExpression An expression to be displayed in a div on the newly created tab.
#'@param placeholder Selector of element where tab is to be inserted to.
#'@export
insertTab <-
  function(id,
           title,
           renderExpression,
           placeholder = "#tabsetPlaceholder") {
    if (!dir.exists("www/images/")) {
      dir.create("www/images/", recursive = T)
    }
    if (!file.exists("www/images/closebutton.jpg")) {
      download.file(
        "https://raw.githubusercontent.com/Chr96er/shinyUtils/master/inst/img/closebutton.png",
        "www/images/closebutton.jpg"
      )
    }
    insertUI(
      selector = placeholder,
      where = "beforeEnd",
      ui = tags$li(id = paste0(id, "li"), HTML(
        paste0(
          "<a href='",
          paste0("#", id),
          "' data-toggle = 'tab', data-value = '",
          id,
          "'>",
          title,
          "   ",
          tags$img(
            src = "images/closebutton.jpg",
            id = paste0(id, "closeButton"),
            class = "closebutton",
            width = "20px",
            height = "20px",
            name = paste0(id, "closeButton"),
            onmouseover = paste0(
              id,
              "closeButton.",
              "width='22';",
              id,
              "closeButton.",
              "height='22';"
            ),
            onmouseout = paste0(
              id,
              "closeButton.",
              "width='20';",
              id,
              "closeButton.",
              "height='20';"
            )
          ),
          "</a>"
        )
      ))
    )
    insertUI(
      selector = ".tab-content",
      where = "beforeEnd",
      ui = tabPanel(
        title = id,
        value = id,
        id = id,
        tags$div(id = paste(id, "div"),
                 renderExpression)
      )
    )
  }

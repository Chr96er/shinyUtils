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
          insertIcon("closebutton.png", id, class = "closebutton"),
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


#'@title insertIcon
#'@name insertIcon
#'@param src Image source.
#'@param id Unique html id.
#'@param width Regular width.
#'@param height Regular height.
#'@param width.mouseHover Width when hovering over image.
#'@param height.mouseHover Height when hovering over image.
#'@param class Class of the newly created image.
#'@export
insertIcon <-
  function(src,
           id,
           width = "20",
           height = "20",
           width.mouseHover = "22",
           height.mouseHover = "22",
           class = NULL,
           name = NULL) {
    if (!dir.exists("www/images/")) {
      dir.create("www/images/", recursive = T)
    }
    target <- paste0("www/images/", src)
    if (!file.exists(target)) {
      download.file(
        paste0("https://raw.githubusercontent.com/Chr96er/shinyUtils/master/inst/img/", src),
        target
      )
    }
    if(is.null(name)){
      name <- tools::file_path_sans_ext(src) 
    }
    tags$img(
      src = paste0("images/", src),
      class = class,
      id = paste0(id, name),
      width = width,
      height = height,
      name = paste0(id, name),
      onmouseover = paste0(
        id,
        paste0(name, "."),
        "width=&quot;",
        width.mouseHover ,
        "&quot;;",
        id,
        paste0(name, "."),
        "height=&quot;",
        height.mouseHover,
        "&quot;;"
      ),
      onmouseout = paste0(
        id,
        paste0(name, "."),
        "width=&quot;",
        width,
        "&quot;;",
        id,
        paste0(name, "."),
        "height=&quot;",
        height,
        "&quot;;"
      )
    )
  }
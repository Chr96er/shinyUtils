library(shiny)
library(shinyjs)
server <- function(input, output, session) {
  insertedTabs <- c()
  observeEvent(input$addTab, {
    if (is.null(input$addTab)) {
      return(NULL)
    }
    id <- paste0("tabset", input$tabTitle)
    if (id %in% insertedTabs) {
      return(NULL)
    }
    insertedTabs <<- c(id, insertedTabs)
    insertTab(id, title = input$tabTitle, renderPlot(plot(runif(1:100))))
    reset("tabTitle")
  })
  
  observeEvent(input$closeTab, {
    if (is.null(input$closeTab)) {
      return(NULL)
    }
    tabToClose <- gsub("closeButton", "", input$closeTab)
    hide(paste0(tabToClose, "li"), anim = T, animType = "fade")
    hide(tabToClose, anim = T, animType = "fade")
    insertedTabs <<- insertedTabs[-(insertedTabs == tabToClose)]
  })
  
}

ui <- fluidPage(
  titlePanel("Add tab dynamically"),
  useShinyjs(),
  tags$script(
    #Get currently clicked tab through jquery and send result to server
    HTML(
      '
      $(function(){
      $("body").on("click", "img", function(){
      let closeTab = $(this)[0].id;
      Shiny.onInputChange("closeTab",closeTab);
      });
      })'
    )
  ),
  sidebarLayout(
    sidebarPanel(
      textInput("tabTitle", "Tab title"),
      actionButton("addTab", "Add tab")
    ),
    mainPanel(tabsetPanel(id = "tabsetPlaceholder"))
  )
)

shinyApp(ui = ui, server = server)
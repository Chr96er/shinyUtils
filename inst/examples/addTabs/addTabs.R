library(shiny)
library(shinyjs)
library(shinyUtils)
server <- function(input, output, session) {
  insertedTabs <- c()
  newTab <- eventReactive(input$addTab, {
    if (is.null(input$addTab)) {
      return("noID")
    }
    id <- paste0("tabset", input$tabTitle)
    if (id %in% insertedTabs) {
      return("duplicate")
    }
    insertedTabs <<- c(id, insertedTabs)
    insertTab(id, title = input$tabTitle, renderPlot(plot(runif(1:100))))
    reset("tabTitle")
    return("success")
  })
  
  observeEvent(input$closeTab, {
    if (is.null(input$closeTab)) {
      return(NULL)
    }
    tabToClose <- gsub("closebutton", "", input$closeTab)
    hide(paste0(tabToClose, "li"),
         anim = T,
         animType = "fade")
    hide(tabToClose, anim = T, animType = "fade")
  })
  
  output$error <- renderText({
    switch(
      newTab(),
      "success" = "Successfully added tab.",
      "noID" = "Please insert a title.",
      "duplicate" = "A tab with the same title has already been added. Please choose a different name."
    )
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
      actionButton("addTab", "Add tab"),
      textOutput("error")
    ),
    mainPanel(tabsetPanel(id = "tabsetPlaceholder"))
  )
  )

shinyApp(ui = ui, server = server)
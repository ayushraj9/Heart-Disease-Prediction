library(shiny)
library(shinydashboard)
d<-getwd()
setwd(d)
source("Svm.R")
options(warn = -1)

shinyServer<-function(input, output){
  Data <- data.frame()
  Results <- reactive(data.frame(input$age, input$gender, input$cp, input$restingbp, input$cholestrol,
                                 input$fastingbp, input$restcg, input$maxheartrate, input$exang,
                                 input$oldpeak, input$slope,input$ca, input$thal))

  observeEvent(input$Action,{
    Data <<- rbind(Data,Results())
    output$table <- renderTable(Data)
  })

  output$Data <- downloadHandler(

    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(Data, file)
    })

  observeEvent(input$Pred,{output$Prediction <- renderText(svm1(input$region))})

  observeEvent(input$Recommend,{output$Result <- renderPrint(recommend(input$asthama, input$diabetes, input$thyroid))})
}

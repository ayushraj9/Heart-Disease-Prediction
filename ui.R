library(shiny)
library(shinydashboard)
d<-getwd()
setwd(d)
source("Svm.R")
options(warn = -1)

shinyUI(dashboardPage(

  dashboardHeader(title = "Predictions"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard",
               tabName = "dashboard",
               icon = icon("dashboard")))),

  dashboardBody(
    tabItems(

      tabItem( tabName = "dashboard",
               h1("Enter The Details for prediction "),
               fluidRow(
                 column(width = 6,
                        box( title = h4("Enter Personal Details"),
                             width = 12 ,solidHeader = T,
                             status = "primary",

                             numericInput("age","Enter Your Age",35),

                             selectInput("gender", label="Enter Your Gender", selectize = TRUE, choices = c("0", "1"), selected = "1"),helpText("1 = male, 0 = female"),

                             selectInput("region", label="Enter Your Region Code", selectize = TRUE, choices = c("101", "102","103"), selected = "101"),helpText("101 = Cleveland, 102 = Hungarian, 103 = Switzerland"),

                             selectInput("cp",label = "Chest Pain Type:",selectize = TRUE, choices = c("1","2","3","4"), selected = "2"), helpText("1 = Typical Angina, 2 = Atypical Angina, 3 = Non Anginal, 4 = Asymptotic"),

                             textInput("restingbp", label =("Resting Blood Pressure"), value = "111"),helpText("Range=100-189 in mm/Hg"),

                             textInput("cholestrol", label =("Cholestrol"), value = "222"), helpText("Range=130-410 mg/dL"),

                             textInput("fastingbp", label =("Fasting Blood Sugar"), value = "1"), helpText(" (120>)-1 = true, (120<)-0 = false"),

                             textInput("restcg", label =("Resting ECG result"), value = "1"), helpText("0 = normal, 1 = having ST-T wave abnormality, 2 = showing ventricular hypertrophy "),

                             textInput("maxheartrate", label =("Max Heart Rate Achieved"),  value = "170"),

                             textInput("exang", label =("Exercise induced angina"),  value = "1"), helpText("0 = no, 1 = yes"),

                             textInput("oldpeak", label =("ST depression induced due to exercise"),  value = "1"),

                             textInput("slope", label =("Slope of peak ST segment"),  value = "1"), helpText("1 = upsloping, 2 = flat, 3 = downsloping"),

                             textInput("ca", label =("Number of major vessels coloured by fluroscopy"), value = "1"), helpText("0 - 3"),

                             textInput("thal", label =("Thal value"),value = "3"), helpText("3 = normal, 6 = fixed defect, 7 = reversible defect"),

                             selectInput("asthama",label = "Asthama:",selectize = TRUE, choices = c("0","1"), selected = "0"), helpText("0 = Not present, 1 = Present"),

                             selectInput("diabetes",label = "Diabetes:",selectize = TRUE, choices = c("0","1"), selected = "0"), helpText("0 = Not present, 1 = Present"),

                             selectInput("thyroid",label = "Thyroid:",selectize = TRUE, choices = c("0","1"), selected = "0"), helpText("0 = Not present, 1 = Present")
                        )
                 ),

                 #tableOutput("table"),
                 actionButton("Action", "Submit!"),
                 #Button to save the file
                 downloadButton('Data', 'Download!'),
                 #Button to predict
                 actionButton("Pred","Predict!"),
                 textOutput("Prediction"),
                 #Button to recommend lifestyle
                 actionButton("Recommend","Recommendations!"),
                 textOutput("Result")
                 

               ))
    ))
))

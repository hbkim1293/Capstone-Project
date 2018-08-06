library(shiny)
library(DT)

shinyUI(fluidPage(
  titlePanel("Predict next word!"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
        h3("Prepare"),
        uiOutput("tab1"),
        uiOutput("tab2"),
        textInput("text","Your sentences in progress : ", value = "Hi, This is my new predict")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       dataTableOutput("Prediction")
    )
  )
))

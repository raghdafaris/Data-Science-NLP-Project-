library(shiny)

ui <- fluidPage(
  titlePanel("Next Word Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("sentence", "Enter a sentence:", value = "The quick brown"),
      actionButton("predict", "Predict Next Word")
    ),
    
    mainPanel(
      h3("Predicted Next Words:"),
      tableOutput("predictions"),
      plotOutput("prediction_plot")
    )
  )
)
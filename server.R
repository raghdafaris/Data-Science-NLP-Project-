# Define server logic
server <- function(input, output) {
  # Load RDS files when the server starts
  bigrams_list <- readRDS("bigrams_list.rds")
  trigrams_list <- readRDS("trigrams_list.rds")
  quadgrams_list <- readRDS("quadgrams_list.rds")
  fivegrams_list <- readRDS("fivegrams_list.rds")
  sixgrams_list <- readRDS("sixgrams_list.rds")
  
  # Reactive expression to predict the next word(s)
  predicted_words <- reactive({
    req(input$sentence)  # Ensure the input is available
    
    # Call the prediction function
    predict_next_word(
      input_text = input$sentence,
      sixgrams_list = sixgrams_list, 
      fivegrams_list = fivegrams_list, 
      quadgrams_list = quadgrams_list, 
      trigrams_list = trigrams_list, 
      bigrams_list = bigrams_list
    )
  })
  
  # Render the predictions table
  output$predictions <- renderTable({
    predictions <- predicted_words()
    
    # If no predictions are available, show a message
    if (nrow(predictions) == 0) {
      data.frame(word = "No prediction available.", frequency = 0)
    } else {
      predictions
    }
  })
  
  # Render the prediction plot
  output$prediction_plot <- renderPlot({
    predictions <- predicted_words()
    
    # Only plot if there are predictions
    if (nrow(predictions) > 0) {
      ggplot(predictions, aes(x = reorder(word, frequency), y = frequency)) +
        geom_bar(stat = "identity", fill = "blue") +
        coord_flip() +
        labs(title = "Next Word Predictions", x = "Word", y = "Frequency")
    }
  })
}
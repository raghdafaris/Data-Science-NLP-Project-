# functions.R

library(shiny)
library(dplyr)
library(ggplot2)
library(tm)
library(stringr)
library(tidytext)
library(tidyverse)
library(tidyr)


# Function to predict the next word using n-gram lists
predict_next_word <- function(input_text, sixgrams_list, fivegrams_list, quadgrams_list, trigrams_list, bigrams_list) {
  # Clean and preprocess input text
  input_text <- tolower(input_text)
  input_text <- removePunctuation(input_text)
  input_text <- removeNumbers(input_text)
  input_text <- stripWhitespace(input_text)
  
  input_tokens <- unlist(strsplit(input_text, "\\s+"))
  
  predictions <- data.frame(word = character(), frequency = numeric(), stringsAsFactors = FALSE)
  
  # Try to predict using sixgrams
  if (length(input_tokens) >= 5) {
    last_five_words <- tail(input_tokens, 5)
    
    for (sixgram_counts in sixgrams_list) {
      next_words <- sixgram_counts %>%
        filter(word1 == last_five_words[1], word2 == last_five_words[2],
               word3 == last_five_words[3], word4 == last_five_words[4],
               word5 == last_five_words[5]) %>%
        group_by(word6) %>%
        summarise(total_n = sum(n), .groups = 'drop') %>%
        arrange(desc(total_n)) %>%
        slice(1:10)
      
      if (nrow(next_words) != 0) {
        predictions <- rbind(predictions, data.frame(word = next_words$word6, frequency = next_words$total_n, stringsAsFactors = FALSE))
      }
    }
  }
  
  # Try to predict using fivegrams
  if (nrow(predictions) == 0 && length(input_tokens) >= 4) {
    last_four_words <- tail(input_tokens, 4)
    
    for (fivegram_counts in fivegrams_list) {
      next_words <- fivegram_counts %>%
        filter(word1 == last_four_words[1], word2 == last_four_words[2],
               word3 == last_four_words[3], word4 == last_four_words[4]) %>%
        group_by(word5) %>%
        summarise(total_n = sum(n), .groups = 'drop') %>%
        arrange(desc(total_n)) %>%
        slice(1:10)
      
      if (nrow(next_words) != 0) {
        predictions <- rbind(predictions, data.frame(word = next_words$word5, frequency = next_words$total_n, stringsAsFactors = FALSE))
      }
    }
  }
  
  # Try to predict using quadgrams
  if (nrow(predictions) == 0 && length(input_tokens) >= 3) {
    last_three_words <- tail(input_tokens, 3)
    
    for (quadgram_counts in quadgrams_list) {
      next_words <- quadgram_counts %>%
        filter(word1 == last_three_words[1], word2 == last_three_words[2],
               word3 == last_three_words[3]) %>%
        group_by(word4) %>%
        summarise(total_n = sum(n), .groups = 'drop') %>%
        arrange(desc(total_n)) %>%
        slice(1:10)
      
      if (nrow(next_words) != 0) {
        predictions <- rbind(predictions, data.frame(word = next_words$word4, frequency = next_words$total_n, stringsAsFactors = FALSE))
      }
    }
  }
  
  # Try to predict using trigrams
  if (nrow(predictions) == 0 && length(input_tokens) >= 2) {
    last_two_words <- tail(input_tokens, 2)
    
    for (trigram_counts in trigrams_list) {
      next_words <- trigram_counts %>%
        filter(word1 == last_two_words[1], word2 == last_two_words[2]) %>%
        group_by(word3) %>%
        summarise(total_n = sum(n), .groups = 'drop') %>%
        arrange(desc(total_n)) %>%
        slice(1:10)
      
      if (nrow(next_words) != 0) {
        predictions <- rbind(predictions, data.frame(word = next_words$word3, frequency = next_words$total_n, stringsAsFactors = FALSE))
      }
    }
  }
  
  # Try to predict using bigrams
  if (nrow(predictions) == 0 && length(input_tokens) >= 1) {
    last_word <- tail(input_tokens, 1)
    
    for (bigram_counts in bigrams_list) {
      next_words <- bigram_counts %>%
        filter(word1 == last_word) %>%
        group_by(word2) %>%
        summarise(total_n = sum(n), .groups = 'drop') %>%
        arrange(desc(total_n)) %>%
        slice(1:10)
      
      if (nrow(next_words) != 0) {
        predictions <- rbind(predictions, data.frame(word = next_words$word2, frequency = next_words$total_n, stringsAsFactors = FALSE))
      }
    }
  }
  
  # Process predictions if available
  if (nrow(predictions) > 0) {
    predictions <- predictions %>%
      group_by(word) %>%
      summarise(frequency = sum(frequency), .groups = 'drop') %>%
      arrange(desc(frequency)) %>%
      slice(1:3)
  } else {
    predictions <- data.frame(word = "No prediction available.", frequency = 0)
  }
  
  return(predictions)
}
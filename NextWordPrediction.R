# Load necessary libraries
library(ggplot2)
library(dplyr)
library(knitr)
library(stringr)
library(tm)
library(RWeka)
library(tidytext)
library(kableExtra)
library(tidyverse)
library(rsample)

### Load the raw data
# Create datasets folder (if it does not already exist)
datasets.folder <- "data"
if (!file.exists(datasets.folder)){
  dir.create(datasets.folder)
}

# Download Swiftkey dataset (if it has not been downloaded yet)
url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
zipfile <- "Coursera-SwiftKey.zip"
zipfile.path <- file.path(datasets.folder, zipfile)
if (!file.exists(zipfile.path)){
  download.file(url, destfile=zipfile.path, method="curl")
  unzip(zipfile.path, exdir=datasets.folder, overwrite=TRUE)
}

# List files in the extracted directory
data_dir <- file.path(datasets.folder, "final", "en_US")
files <- list.files(data_dir, full.names=TRUE)

# Function to read a text file and return its contents
read_text_file <- function(file_path) {
  con <- file(file_path, "r")
  text_data <- readLines(con, encoding="UTF-8", skipNul=TRUE)
  close(con)
  return(text_data)
}

# Read each file
blogs_file <- file.path(data_dir, "en_US.blogs.txt")
news_file <- file.path(data_dir, "en_US.news.txt")
twitter_file <- file.path(data_dir, "en_US.twitter.txt")

# Load and clean a subset of the data
twitter_data <- read_text_file(twitter_file)  
news_data <- read_text_file(news_file)       
blogs_data <- read_text_file(blogs_file)    

# Combine the data into one single text
combined_data <- paste(twitter_data, news_data, blogs_data, sep = " ")

# Display a portion of the combined data
substr(combined_data,1, 20)  # Display the first 20 characters

# Save to a file
writeLines(combined_data, "combined_data.txt")

## Sampling the data to 20% test, 80% train
alldata <- data.frame(text = combined_data, stringsAsFactors = FALSE)

# Save the data frame as a CSV file
write.csv(alldata, "data.csv", row.names = FALSE)

# Set seed for reproducibility
set.seed(123)

# Set data 
data <- alldata

# Split: 80% for training and 20% for testing
initial_split <- initial_split(data, prop = 0.80)
train <- training(initial_split)
test <- testing(initial_split)

# Print the dimensions of each set
cat("Training data dimensions: ", dim(train), "\n")
cat("Testing data dimensions: ", dim(test), "\n")

#_______________________SPLIT MORE (ENSMBLED)_________________________________
# To save space and time, I split the train data into 8 sets before cleaning it.  
# Split text for cleaning and training
n <- nrow(train)

# Calculate the split indices
indices <- sample(1:n, n)

# Calculate the split points for 8 parts
split_points <- floor(seq(0, n, length.out = 9))

# Split the dataset into 8 parts
train1 <- train[indices[1:split_points[2]], ]
train2 <- train[indices[(split_points[2] + 1):split_points[3]], ]
train3 <- train[indices[(split_points[3] + 1):split_points[4]], ]
train4 <- train[indices[(split_points[4] + 1):split_points[5]], ]
train5 <- train[indices[(split_points[5] + 1):split_points[6]], ]
train6 <- train[indices[(split_points[6] + 1):split_points[7]], ]
train7 <- train[indices[(split_points[7] + 1):split_points[8]], ]
train8 <- train[indices[(split_points[8] + 1):n], ]

# Convert each subset into a data frame
train1 <- data.frame(text = train1$text)
train2 <- data.frame(text = train2$text)
train3 <- data.frame(text = train3$text)
train4 <- data.frame(text = train4$text)
train5 <- data.frame(text = train5$text)
train6 <- data.frame(text = train6$text)
train7 <- data.frame(text = train7$text)
train8 <- data.frame(text = train8$text)

#______________________________Clean the data___________________________________
# From now we will use LISTS 

# The datasets are named train1, train2, ..., train8 and stored in a list
datasets <- list(train1, train2, train3, train4, train5, train6, train7, train8)

# Initialize a list to store cleaned text data frames
cleaned_train <- list()  # Correct initialization

# Loop over each dataset
for (i in 1:length(datasets)) {
  print(i)
  train <- datasets[[i]]
  
  # Convert to corpus
  corpus <- Corpus(VectorSource(train$text))
  
  # Cleaning steps
  corpus <- tm_map(corpus, content_transformer(tolower)) # LOWERCASE
  corpus <- tm_map(corpus, removePunctuation)            # remove Punctuation
  corpus <- tm_map(corpus, removeNumbers)                # remove Numbers
  corpus <- tm_map(corpus, stripWhitespace)              # strip Whitespace
  corpus <- tm_map(corpus, content_transformer(function(x) str_replace_all(x, "http\\S+|www\\S+|\\S+@\\S+", "")))  # Remove URLs, emails
  corpus <- tm_map(corpus, content_transformer(function(x) str_replace_all(x, "#\\S+|@\\S+", "")))  # Remove hashtags and mentions
  corpus <- tm_map(corpus, content_transformer(function(x) str_replace_all(x, "[^a-z\\s]", "")))    # Remove non-alphabetic
  
  # Extract cleaned text from corpus
  cleaned_text <- sapply(corpus, as.character)
  
  # Create a data frame from the cleaned text
  cleantext <- data.frame(text = cleaned_text, stringsAsFactors = FALSE)
  
  # Store the cleaned data frame in the list
  cleaned_train[[i]] <- cleantext
}
# Save the list to an RDS file
saveRDS(cleaned_train, file = "cleaned_train.rds")  

# START FINDING THE GRAMS FROM 1 to 6 

# _____________________________UNIGRAMS_________________________________________

# Initialize a list to store the unigrams for each dataset
unigrams_list <- list()

# Loop over the first 8 datasets and extract unigrams
for (i in 1:8) {
  print(i)
  dataset <- cleaned_train[[i]]  # Use cleaned_train
  
  # Unnest tokens to get unigrams and count their occurrences
  unigrams <- dataset %>%
    unnest_tokens(word, text) %>%  # Tokenize text into words
    count(word, sort = TRUE)       # Count occurrences of each word
  
  # Store the unigrams with probabilities in the list
  unigrams_list[[i]] <- unigrams
}

# Save the list to an RDS file
saveRDS(unigrams_list, file = "unigrams_list.rds")  

#__________________________BiGRAMS_________________________________________
# Initialize a list to store the bigrams for each dataset
bigrams_list <- list()

# Loop over the first 8 datasets and extract bigrams
for (i in 1:8) {
  dataset <- cleaned_train[[i]]  # Use cleaned_train
  
  # Unnest tokens to get bigrams and count their occurrences
  bigrams <- dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    count(bigram, sort = TRUE) %>%
    filter(!is.na(bigram)) %>%
    separate(bigram, into = c("word1", "word2"), sep = " ")
  
  # Store the bigrams with probabilities in the list
  bigrams_list[[i]] <- bigrams
}

# Save the list to an RDS file
saveRDS(bigrams_list, file = "bigrams_list.rds")

#__________________________TRIGRAMS____________________________________________

# Initialize a list to store the trigrams for each dataset
trigrams_list <- list()

# Loop over the first 8 datasets and extract trigrams
for (i in 1:8) {
  dataset <- cleaned_train[[i]]  # Use cleaned_train
  
  # Extract trigrams
  trigrams <- dataset %>%
    unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
    count(trigram, sort = TRUE) %>%
    filter(!is.na(trigram)) %>%
    separate(trigram, into = c("word1", "word2", "word3"), sep = " ")
  
  # Store the trigrams with probabilities in the list
  trigrams_list[[i]] <- trigrams
}

# Save the list to an RDS file
saveRDS(trigrams_list, file = "trigrams_list.rds")

# _____________________________FOURGRAMS_________________________________________

# Initialize a list to store the fourgrams for each dataset
fourgrams_list <- list()

# Loop over the first 8 datasets and extract fourgrams
for (i in 1:8) {
  dataset <- cleaned_train[[i]]  # Use cleaned_train
  
  # Extract fourgrams
  fourgrams <- dataset %>%
    unnest_tokens(fourgram, text, token = "ngrams", n = 4) %>%
    count(fourgram, sort = TRUE) %>%
    filter(!is.na(fourgram)) %>%
    separate(fourgram, into = c("word1", "word2", "word3", "word4"), sep = " ")
  
  # Store the fourgrams with probabilities in the list
  fourgrams_list[[i]] <- fourgrams
}

# Save the list to an RDS file
saveRDS(fourgrams_list, file = "fourgrams_list.rds")

# _____________________________FIVEGRAMS_________________________________________

# Initialize a list to store the fivegrams for each dataset
fivegrams_list <- list()

# Loop over the first 8 datasets and extract fivegrams
for (i in 1:8) {
  dataset <- cleaned_train[[i]]  # Use cleaned_train
  
  # Extract fivegrams
  fivegrams <- dataset %>%
    unnest_tokens(fivegram, text, token = "ngrams", n = 5) %>%
    count(fivegram, sort = TRUE) %>%
    filter(!is.na(fivegram)) %>%
    separate(fivegram, into = c("word1", "word2", "word3", "word4", "word5"), sep = " ")
  
  # Store the fivegrams with probabilities in the list
  fivegrams_list[[i]] <- fivegrams
}

# Save the list to an RDS file
saveRDS(fivegrams_list, file = "fivegrams_list.rds")

# _____________________________SIXGRAMS_________________________________________

# Initialize a list to store the sixgrams for each dataset
sixgrams_list <- list()

# Loop over the first 8 datasets and extract sixgrams
for (i in 1:8) {
  dataset <- cleaned_train[[i]]  # Use cleaned_train
  
  # Extract sixgrams
  sixgrams <- dataset %>%
    unnest_tokens(sixgram, text, token = "ngrams", n = 6) %>%
    count(sixgram, sort = TRUE) %>%
    filter(!is.na(sixgram)) %>%
    separate(sixgram, into = c("word1", "word2", "word3", "word4", "word5", "word6"), sep = " ")
  
  # Store the sixgrams with probabilities in the list
  sixgrams_list[[i]] <- sixgrams
}

# Save the list to an RDS file
saveRDS(sixgrams_list, file = "sixgrams_list.rds")

# ________________________FUNCTION TO PREDICT NEXT WORD____________________

predict_next_word <- function(input_text, sixgrams_list, fivegrams_list, quadgrams_list, trigrams_list, bigrams_list) {
  # Preprocess input text
  input_text <- tolower(input_text)
  input_words <- unlist(strsplit(input_text, " "))  # Split input into words
  
  # Initialize an empty list to store predictions
  predictions <- character(0)
  
  # Use six-grams
  if (length(input_words) >= 5) {
    last_words <- tail(input_words, 5)
    sixgram_candidate <- paste(last_words, collapse = " ")
    for (i in seq_along(sixgrams_list)) {
      sixgram_counts <- sixgrams_list[[i]]
      match_row <- sixgram_counts[grepl(sixgram_candidate, sixgram_counts$sixgram), ]
      if (nrow(match_row) > 0) {
        predictions <- c(predictions, as.character(match_row$word6[1]))
      }
    }
  }
  
  # Use five-grams
  if (length(input_words) >= 4 && length(predictions) == 0) {
    last_words <- tail(input_words, 4)
    fivegram_candidate <- paste(last_words, collapse = " ")
    for (i in seq_along(fivegrams_list)) {
      fivegram_counts <- fivegrams_list[[i]]
      match_row <- fivegram_counts[grepl(fivegram_candidate, fivegram_counts$fivegram), ]
      if (nrow(match_row) > 0) {
        predictions <- c(predictions, as.character(match_row$word5[1]))
      }
    }
  }
  
  # Use quadgrams
  if (length(input_words) >= 3 && length(predictions) == 0) {
    last_words <- tail(input_words, 3)
    quadgram_candidate <- paste(last_words, collapse = " ")
    for (i in seq_along(quadgrams_list)) {
      quadgram_counts <- quadgrams_list[[i]]
      match_row <- quadgram_counts[grepl(quadgram_candidate, quadgram_counts$quadgram), ]
      if (nrow(match_row) > 0) {
        predictions <- c(predictions, as.character(match_row$word4[1]))
      }
    }
  }
  
  # Use trigrams
  if (length(input_words) >= 2 && length(predictions) == 0) {
    last_words <- tail(input_words, 2)
    trigram_candidate <- paste(last_words, collapse = " ")
    for (i in seq_along(trigrams_list)) {
      trigram_counts <- trigrams_list[[i]]
      match_row <- trigram_counts[grepl(trigram_candidate, trigram_counts$trigram), ]
      if (nrow(match_row) > 0) {
        predictions <- c(predictions, as.character(match_row$word3[1]))
      }
    }
  }
  
  # Use bigrams
  if (length(input_words) >= 1 && length(predictions) == 0) {
    last_word <- tail(input_words, 1)
    bigram_candidate <- last_word
    for (i in seq_along(bigrams_list)) {
      bigram_counts <- bigrams_list[[i]]
      match_row <- bigram_counts[grepl(bigram_candidate, bigram_counts$word1), ]
      if (nrow(match_row) > 0) {
        predictions <- c(predictions, as.character(match_row$word2[1]))
      }
    }
  }
  
  # Return predictions (only the first one if there are multiple)
  return(predictions)
}

#______________________________TESTING THE PREDICTION FUNCTION__________________

# Example usage
input_text <- "the quick brown fox"
predicted_words <- predict_next_word(input_text, sixgrams_list, bigrams_list, quadgrams_list, trigrams_list, bigrams_list)

# Print predicted words
cat("Predicted words:", predicted_words, "\n")

#______________________________VISUALIZATION OF N-GRAMS_________________________

# Function to visualize n-grams
visualize_ngrams <- function(ngram_list, n) {
  for (i in seq_along(ngram_list)) {
    ngram_data <- ngram_list[[i]]
    top_ngram <- head(ngram_data, 10)
    
    ggplot(top_ngram, aes_string(x = "reorder(word1, n)", y = "n")) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = paste(n, "-gram Frequency in Dataset", i),
           x = paste(n, "Gram"),
           y = "Frequency") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
}

# Visualize unigrams
visualize_ngrams(unigrams_list, 1)

# Visualize bigrams
visualize_ngrams(bigrams_list, 2)

# Visualize trigrams
visualize_ngrams(trigrams_list, 3)

# Visualize quadgrams
visualize_ngrams(quadgrams_list, 4)

# Visualize sixgrams
visualize_ngrams(sixgrams_list, 6)

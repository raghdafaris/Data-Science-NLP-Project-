# Next Word Prediction System

This project focuses on predicting the next word in a given text input using n-grams extracted from the SwiftKey dataset. The approach leverages unigrams, bigrams, trigrams, and higher-order n-grams (up to sixgrams) to predict the next word based on the input sequence.

## Introduction

The project processes and analyzes text data from the SwiftKey dataset, focusing on:
- Loading and cleaning the data.
- Extracting n-grams (unigrams to sixgrams).
- Building a prediction function to suggest the next word based on the input.

An NLP app has been built to demonstrate this functionality.

## Key Steps

### 1. Loading the Data
The first step involves loading text data from three sources: blogs, news, and Twitter. The data is split into training and testing sets with an 80-20 split.

### 2. Data Cleaning
Text data is cleaned by:
- Converting text to lowercase.
- Removing punctuation, numbers, and extra whitespace.
- Retaining only alphabetic characters.
- Stop words are **not** removed, as this reduced prediction accuracy.

Misspelled or non-frequent words (e.g., "looooooooove") are eliminated to save storage space.

### 3. Extracting N-grams
The cleaned training data is used to extract n-grams (from unigrams to sixgrams). These are stored in lists for further processing.

### 4. N-gram Prediction Function
The function predicts the next word based on the input sequence by checking:
- **Sixgrams** (if 5 words are available).
- **Fivegrams** (if 4 words are available).
- **Quadgrams** (if 3 words are available).
- **Trigrams** (if 2 words are available).
- **Bigrams** (if only 1 word is available).

It returns the top 10 predictions ranked by frequency, or a message if no prediction is available.

## Project Workflow

The overall process follows this workflow:

1. Load the dataset.
2. Clean the data.
3. Split the data into training and testing sets.
4. Split the training data into smaller subsets to manage memory.
5. Extract n-grams (1 to 6).
6. Build the n-gram prediction function.
7. Test the results.

## Try the App

You can access the NLP app at the following link:

[NLP App](https://datasciencebyraghda.shinyapps.io/myapp/)

## Presentation

A detailed presentation of this project is available here:

[Presentation](https://rpubs.com/Raghda_Altaei/1212259)


#### Load packages
library(dplyr)
library(RWeka)
library(ggplot2)

#### Load data
load("./preprocessed_RData/apple.RData")
apple_df
load("./preprocessed_RData/microsoft.RData")
microsoft_df
load("./preprocessed_RData/dell.RData")
dell_df


#### Making N-grams function

ngram_analysis <- function(data, text_column, min_n = 2, max_n = 4, top_n = 10) {
  # Define the tokenizer
  Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = min_n, max = max_n))
  
  # Extract and clean the text column
  text_data <- data[[text_column]]
  
  # Generate n-grams
  ngrams <- Tokenizer(text_data)
  
  # Convert to a table for analysis
  ngram_table <- table(ngrams)
  
  # Sort by frequency
  sorted_ngrams <- sort(ngram_table, decreasing = TRUE)
  
  # Convert to a dataframe
  ngram_df <- as.data.frame(sorted_ngrams, stringsAsFactors = FALSE)
  colnames(ngram_df) <- c("ngram", "frequency")
  
  # Plot the top n n-grams
  plot <- ngram_df %>%
    top_n(top_n, frequency) %>%
    ggplot(aes(x = reorder(ngram, frequency), y = frequency)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(x = "N-grams", y = "Frequency", title = paste("Top", top_n, "N-grams")) +
    theme_minimal()
  
  # Return the results as a list
  return(list(
    ngram_df = ngram_df%>% head(top_n),
    plot = plot
  ))
}

apple_ngrams <- ngram_analysis(
  data = apple_df,
  text_column = "text",
  min_n = 2,
  max_n = 4,
  top_n = 20
)

dell_ngrams <- ngram_analysis(
  data = dell_df,
  text_column = "text",
  min_n = 2,
  max_n = 4,
  top_n = 20
)

microsoft_ngrams <- ngram_analysis(
  data = microsoft_df,
  text_column = "text",
  min_n = 2,
  max_n = 4,
  top_n = 20
)

microsoft_ngrams$ngram_df
load("ESGterm.Rdata")
esg_terms_df


microsoft_ngrams_df <- microsoft_ngrams$ngram_df

# Now perform the left join
joined_df <- microsoft_ngrams_df %>%
  left_join(esg_terms_df, by = c("ngram" = "word"))

# View the result
print(joined_df)





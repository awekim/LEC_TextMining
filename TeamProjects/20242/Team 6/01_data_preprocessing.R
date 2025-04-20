#install.packages("pdftools")
library(pdftools)
library(dplyr)
library(stringr)
library(tm)
library(stopwords)
library(textstem)

##########################################
### PDF pre-processing
##########################################

### 1.
### Apple 2022 sustainability report

apple_path <- "./sustainability_report/apple-2022.pdf"
apple <- pdf_text(apple_path)

# remove unnecessary pages
apple <- apple[3:81]

# set the custom stopwords appear in pdf texts.
# such as contents, or repetitive words
apple_stopwords <- c(
  stopwords::stopwords("en", source = "nltk"),          # NLTK stop words
  c("introduction", "climate", "change", "resources", 
    "smarter", "chemistry", "engagement", "appendix",
    "environmental", "progress", "report", "apple", "iphone")   # Custom stop words
)

stopword_pattern <- paste0("\\b(", paste(apple_stopwords, collapse = "|"), ")\\b")
# Preprocessing function with extended stop words
clean_text_vector <- function(text_vector) {
  text_vector %>%
    # Convert to lowercase
    str_to_lower() %>%
    # Remove combined stop words
    str_remove_all(stopword_pattern) %>%
    # Remove punctuation
    str_replace_all("[[:punct:]]", "") %>%
    # Remove non-printable characters
    str_replace_all("[^\\x20-\\x7E]", " ") %>%
    # Remove numbers
    str_remove_all("[0-9]") %>%
    # Replace multiple spaces with a single space
    str_squish() %>%
    # Lemmatize strings
    lemmatize_strings()
}

# Apply preprocessing to the apple variable
cleaned_apple <- clean_text_vector(apple)

# View the first few cleaned entries
head(cleaned_apple)

apple_final <- paste(cleaned_apple, collapse = " ")
apple_df <- data.frame(company = "apple", text = apple_final, stringsAsFactors = FALSE)
save(apple_df, file = "./preprocessed_RData/apple.RData")



### 2.
### Microsoft 2022 sustainability report

microsoft_path <- "./sustainability_report/microsoft-2022.pdf"
ms <- pdf_text(microsoft_path)
ms <- ms[-c(1:3, 78:81)]

pattern_ms <- "overview sustainability customer sustainability global sustainability carbon water waste ecosystem "

ms_stopwords <- c(
  stopwords::stopwords("en", source = "nltk"),  
  c("microsoft", "appendix", "fy")
)

stopword_pattern_ms <- paste0("\\b(", paste(ms_stopwords, collapse = "|"), ")\\b")

clean_text_vector_ms <- function(text_vector) {
  text_vector %>%
    # Convert to lowercase
    str_to_lower() %>%
    # Remove punctuation
    str_replace_all("[[:punct:]]", "") %>%
    str_remove_all("[\\+\\$\\|]+") %>%
    # Remove non-printable characters
    str_replace_all("[^\\x20-\\x7E]", " ") %>%
    # Remove numbers
    str_remove_all("[0-9]") %>%
    # Replace multiple spaces with a single space
    str_squish() %>%
    # Lemmatize strings
    # Remove combined stop words
    str_remove_all(stopword_pattern_ms) %>%
    lemmatize_strings()
}


cleaned_ms <- clean_text_vector_ms(ms)
cleaned_ms <- cleaned_ms %>% str_remove_all(pattern_ms)
head(cleaned_ms)


ms_final <- paste(cleaned_ms, collapse = " ")
microsoft_df <- data.frame(company = "microsoft", text = ms_final, stringsAsFactors = FALSE)
save(microsoft_df, file = "./preprocessed_RData/microsoft.RData")



### 3.
### Dell 2022 sustainability report

dell_path <- "./sustainability_report/dell-2022.pdf"
dell <- pdf_text(dell_path)
dell <- dell[-c(1:3,132:138,148:159)]


dell_stopwords <- c(
  stopwords::stopwords("en", source = "nltk"),  
  c("fy","dell","appendix","cy","will")
)
pattern_dell <- "technology intro plan goal dashboard advance sustainability cultivate inclusion transform life ethic privacy supply chain number esg report"

stopword_pattern_dell <- paste0("\\b(", paste(dell_stopwords, collapse = "|"), ")\\b")



clean_text_vector_dell <- function(text_vector) {
  text_vector %>%
    # Remove URLs
    str_replace_all("(https?://|www\\.|[a-zA-Z0-9-]+\\.[a-zA-Z]{2,})(/[a-zA-Z0-9-_/]*)?", "") %>%
    # Convert to lowercase
    str_to_lower() %>%
    # Remove combined stop words
    # Remove punctuation
    str_replace_all("[[:punct:]]", "") %>%
    str_remove_all("[\\+\\$\\|]+") %>%
    # Remove non-printable characters
    str_replace_all("[^\\x20-\\x7E]", " ") %>%
    # Remove numbers
    str_remove_all("[0-9]") %>%
    str_remove_all(stopword_pattern_dell) %>%
    # Replace multiple spaces with a single space
    str_squish() %>%
    # Lemmatize strings
    lemmatize_strings()
}


cleaned_dell <- clean_text_vector_dell(dell)
cleaned_dell <- cleaned_dell %>% str_remove_all(pattern_dell)
head(cleaned_dell)
  
dell_final <- paste(cleaned_dell, collapse = " ")
dell_df <- data.frame(company = "dell", text = dell_final, stringsAsFactors = FALSE)
save(dell_df, file = "./preprocessed_RData/dell.RData")








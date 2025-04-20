library(dplyr)
library(tidyr)
library(tidytext)
library(wordcloud)


### Dell

## load data
load("preprocessed_RData/dell.RData")
dell_df


word_frequencies_dell <- dell_df %>% unnest_tokens(word,text) %>%
  count(word, sort = TRUE)


set.seed(1004)
word_data <- data.frame(
  word = word_frequencies_dell$word,  # 단어
  freq = word_frequencies_dell$n      # 빈도
)

wordcloud(
  words = word_data$word,  # The words from the DTM
  freq = word_data$freq,   # Their frequencies
  max.words = 100,         # 최대 단어 수
  random.order = FALSE,    # 단어 빈도 순서대로 배치
  colors = brewer.pal(8, "Dark2")  # 색상 팔레트
)


### Apple
load("preprocessed_RData/apple.RData")
apple_df

word_frequencies_ap <- apple_df %>% unnest_tokens(word,text) %>%
  count(word, sort = TRUE)


set.seed(1004)
word_data <- data.frame(
  word = word_frequencies_ap$word,  # 단어
  freq = word_frequencies_ap$n      # 빈도
)

wordcloud(
  words = word_data$word,  # The words from the DTM
  freq = word_data$freq,   # Their frequencies
  max.words = 100,         # 최대 단어 수
  random.order = FALSE,    # 단어 빈도 순서대로 배치
  colors = brewer.pal(8, "Dark2")  # 색상 팔레트
)


###Micro###
load("preprocessed_RData/microsoft.RData")
microsoft_df

word_frequencies_ms <- microsoft_df %>% unnest_tokens(word,text) %>%
  count(word, sort = TRUE)


set.seed(1004)
word_data <- data.frame(
  word = word_frequencies_ms$word,  # 단어
  freq = word_frequencies_ms$n      # 빈도
)

wordcloud(
  words = word_data$word,  # The words from the DTM
  freq = word_data$freq,   # Their frequencies
  max.words = 100,         # 최대 단어 수
  random.order = FALSE,    # 단어 빈도 순서대로 배치
  colors = brewer.pal(8, "Dark2")  # 색상 팔레트
)

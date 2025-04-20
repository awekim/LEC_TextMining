##################################################################
### Text Mining Team Project                                   ###
### Project Title: Korean Text Analysis                     ###
###                with Image Text Recognition                 ###
### Project Goal: To find psychological difference of men and  ###
###               women through the analysis of the most       ###
###               popular Webtoon by each gender               ###
### Project Members: 한찬솔 (https://github.com/ccss17)         ###
###                  박주은 (https://github.com/zuenbagi01)     ###
###                  임지훈 (https://github.com/noahlim99)      ###
##################################################################

library(magrittr)
library(dplyr)
library(tm)
library(str2str)
library(tidyr)
library(slam)
library(wordcloud2)
library(ggplot2)

get_freq_table <- function(corpus_set, table_name, lower_bound = 0) {
  corpus.token <- corpus_set %>% 
    tm_map(content_transformer(scan_tokenizer))
  corpus.all <- c()
  for (i in 1:length(corpus.token)){
    corpus.all <- c(corpus.all, corpus.token[[i]]$content)
  }
  freq_table <- table(corpus.all) %>% sort(decreasing = TRUE)
  low_outliers_idx <- which(freq_table < lower_bound)
  if (length(low_outliers_idx) == 0) {
    freq_table <- freq_table %>% data.frame
  } else {
    freq_table <- freq_table[-which(freq_table < lower_bound)] %>% data.frame
  }
  colnames(freq_table) <- c(paste(c(table_name, 'word'), collapse = '.'),  
                            paste(c(table_name, 'Freq'), collapse = '.'))
  return(freq_table)
}

weight_TfDf_Rank <- function (m, normalize = TRUE, doc_wise_weight = NULL) 
{
  isDTM <- inherits(m, "DocumentTermMatrix")
  if (isDTM) 
    m <- t(m)
  if (normalize) {
    cs <- col_sums(m)
    if (any(cs == 0)) 
      warning("empty document(s): ", paste(Docs(m)[cs == 
                                                     0], collapse = " "))
    names(cs) <- seq_len(nDocs(m))
    m$v <- m$v/cs[m$j]
  }
  rs <- row_sums(m > 0)
  if (any(rs == 0)) 
    warning("unreferenced term(s): ", paste(Terms(m)[rs == 
                                                       0], collapse = " "))
  lnrs <- exp(rs/nDocs(m))
  lnrs[!is.finite(lnrs)] <- 0
  m <- m * lnrs
  attr(m, "weighting") <- c(sprintf("%s%s", "term frequency - inverse document frequency", 
                                    if (normalize) " (normalized)" else ""), "tf-idf")
  if (!is.null(doc_wise_weight)) {
    for (doc_idx in 1:m$ncol) {
      m[,doc_idx] <- doc_wise_weight(doc_idx, m[,doc_idx])
    }
  }
  if (isDTM) 
    t(m)
  else m
}

get_TfDf_rank_score <- function(corpus, table_name, rank_step = 0.2, normalize = TRUE) {
  doc_wise_rank_weight <- function(doc_idx, x) {
      x * exp(rank_step * (1 - doc_idx))
      # x * exp(0.2 - 0.2 * 1 = 0)
      # x * exp(0.2 - 0.2 * 2 = -0.2)
      # x * exp(0.2 - 0.2 * 3 = -0.4)
      # x * exp(0.2 - 0.2 * 4 = -0.6)
  }

  corpus.TfDf <- corpus %>%
    DocumentTermMatrix(control=list(wordLengths=c(1, Inf),
                                    weighting=function(x)
                                      weight_TfDf_Rank(x, 
                                                       normalize = normalize,
                                                       doc_wise_weight = doc_wise_rank_weight))) %>%
    as.matrix() %>% colSums()
  
  TfDf.freq <- 
    data.frame(word=names(corpus.TfDf), freq=unname(corpus.TfDf)) %>% arrange(desc(freq))
  
  colnames(TfDf.freq) <- c(paste(c(table_name, 'word'), collapse = '.'),  
                           paste(c(table_name, 'score'), collapse = '.'))
  TfDf.freq
}

distinguish_window <- function(tbl1, tbl2, window_size) {
  for (i in 1:(min(nrow(tbl1), nrow(tbl2))-(window_size-1))) {
    for (j in 0:(window_size-1)) {
      for (h in 0:(window_size-1)) {
        if (!is.na(tbl1[i+j,1]) & 
            !is.na(tbl2[i+h,1]) &
            tbl1[i+j,1] == tbl2[i+h,1]) {
          tbl1[i+j,1] <- NA
          tbl2[i+h,1] <- NA
          break
        }
      }
    }
  }
  list(tbl1 %>% drop_na, tbl2 %>% drop_na)
}

embedding_gender_space <- function(gender_score_table, point_num = Inf) {
  gender_embedding <- data.frame(matrix(nrow = 0, ncol = 3))
  colnames(gender_embedding) <- c('word', 'male.score', 'female.score')
  max_idx <- min(
    length(gender_score_table.15_0.2[,1][!is.na(gender_score_table.15_0.2[,1])]),
    length(gender_score_table.15_0.2[,3][!is.na(gender_score_table.15_0.2[,3])]))
  for (i in 1:max_idx) {
    matched <- 
      gender_score_table[
        which(gender_score_table[,3] == gender_score_table[i,]$male.word),]
    if (nrow(matched) != 0) {
      gender_embedding[nrow(gender_embedding)+1,] <-
        c(gender_score_table[i,]$male.word, 
          gender_score_table[i,]$male.score, 
          matched$female.score)
    }
    matched <- 
      gender_score_table[
        which(gender_score_table[,1] == gender_score_table[i,]$female.word),]
    if (nrow(matched) != 0) {
      gender_embedding[nrow(gender_embedding)+1,] <-
        c(gender_score_table[i,]$female.word, 
          matched$male.score,
          gender_score_table[i,]$female.score)
    }
    if (nrow(gender_embedding) > point_num) 
      break
  }
  gender_embedding 
}

##############################################################################
### Analysis
##############################################################################

### HYPER PARAMETERS
window_size <- 7
rank_step <- 0.02

window_size <- 10
rank_step <- 0.1

window_size <- 15
rank_step <- 0.2

### MALE
male <- VCorpus(DirSource("output/male"), readerControl=list(language='ko'))
male_freq_table <- get_freq_table(male, 'male')
male_score_table <- get_TfDf_rank_score(male, 'male', rank_step = rank_step,
                                        normalize = TRUE)
cbind(male_freq_table, male_score_table) %>% head(20)

### FEMALE
female <- VCorpus(DirSource("output/female"), readerControl=list(language='ko'))
female_freq_table <- get_freq_table(female, 'female')
female_score_table <- get_TfDf_rank_score(female, 'female', rank_step = rank_step, normalize = TRUE)
cbind(female_freq_table, female_score_table) %>% head(20)

### combine
conv.df <- cbind_fill(male_freq_table, female_freq_table)
conv.df %>% head(20)
conv_score.df <- cbind_fill(male_score_table, female_score_table)
conv_score.df %>% head(20)

### distinguishing
distinguished_tables <- distinguish_window(male_score_table, female_score_table, 
                           window_size = window_size)
distinguished_male_female <- cbind_fill(distinguished_tables[[1]], distinguished_tables[[2]])
distinguished_male_female %>% head(30)

### export csv
dir.create(file.path('result'), recursive = TRUE, showWarnings = FALSE)
# write.csv(conv.df, "result/male_female_freq.csv", row.names=FALSE)
write.csv(conv_score.df, "result/(15,0.2)male_female_score.csv", row.names=FALSE)
write.csv(distinguished_male_female, 
          "result/(15,0.2)male_female_distinguished.csv", row.names=FALSE)

##############################################################################
### Visualization
##############################################################################

### wordcloud male and female
male_score_table %>%
  wordcloud2::wordcloud2(fontFamily = "NanumSquare")

female_score_table %>%
  wordcloud2::wordcloud2(fontFamily = "NanumSquare")

### wordcloud distinguished male and female
distinguished_tables[[1]] %>%
  wordcloud2::wordcloud2(fontFamily = "NanumSquare")

distinguished_tables[[2]] %>%
  wordcloud2::wordcloud2(fontFamily = "NanumSquare")

### embedding gender space
gender_score_table.15_0.2 <- 
  read.csv('result/(15,0.2)male_female_score.csv')
gender_score_table.15_0.2_d <- 
  read.csv('result/(15,0.2)male_female_distinguished.csv')

point_num <- 150
gender_embedding <- 
  embedding_gender_space(gender_score_table.15_0.2, point_num)
gender_embedding_d <- 
  embedding_gender_space(gender_score_table.15_0.2_d, point_num)

size <- 7
ggplot(gender_embedding, aes(male.score, female.score, label = word)) + 
  geom_text(size = size) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggplot(gender_embedding_d, aes(male.score, female.score, label = word)) + 
  geom_text(size = size) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

################################################################################ 
### TEST CODE
################################################################################ 

### distinguish window
table1 <- data.frame(word = c('a', 'b', 'c', 'd', 'e', 'f', 'g'))
table2 <- data.frame(word = c('a', '?', '?', '?', 'g', 'b', '?'))
print('tables:')
cbind_fill(table1, table2)
for (i in 1:5) {
  distinguished_tables <- distinguish_window(table1, table2, window_size = i)
  print(paste(c('window size:', i)))
  print(cbind_fill(distinguished_tables[[1]], distinguished_tables[[2]]))
}

### Compare TF-IDF vs TF-DF 
ex.df <- data.frame(doc_id=c("Doc_1",  "Doc_2"), 
                    text=c("I love you",  "I adore you you"))
ex.df.corpus <- ex.df %>% DataframeSource() %>% Corpus()
ex.df.dtm <- ex.df.corpus %>%
  DocumentTermMatrix(control=list(wordLengths=c(1, Inf)))

ex.df.TfIdf <- ex.df.corpus %>%
  DocumentTermMatrix(control=list(wordLengths=c(1, Inf),
                                  weighting=function(x)
                                    weightTfIdf(x, normalize = FALSE)))
ex.df.TfDf <- ex.df.corpus %>%
  DocumentTermMatrix(control=list(wordLengths=c(1, Inf),
                                  weighting=function(x)
                                    weight_TfDf_Rank(x, normalize = FALSE)))

doc_wise_weight_test <- function(doc_idx, x) {
  x * doc_idx * 100
}

ex.df.doc_wise_weight <- ex.df.corpus %>%
  DocumentTermMatrix(control=list(wordLengths=c(1, Inf),
                                  weighting=function(x)
                                    weight_TfDf_Rank(x, 
                                               normalize = FALSE, 
                                               doc_wise_weight = doc_wise_weight_test)))

ex.df.dtm %>% as.matrix()

# TF-IDF vs TF-DF
ex.df.TfIdf %>% as.matrix()
ex.df.TfDf %>% as.matrix()

# Compare document-wise weighting
ex.df.TfDf %>% as.matrix()
ex.df.doc_wise_weight%>% as.matrix()

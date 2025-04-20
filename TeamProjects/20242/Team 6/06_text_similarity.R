############################
##### Text Similarity  #####
############################
library(stringr)
library(proxy)
library(tm)

load("preprocessed_RData/dtm.RData")

doc.cos <-
  dtm %>%
  as.matrix %>%
  proxy::dist(method = "cosine") %>%
  as.matrix

doc.cos

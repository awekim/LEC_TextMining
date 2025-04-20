############################
###### Topic modeling ###### 
############################
library(stringr)
library(tm)
library(ldatuning)


load("preprocessed_RData/dtm.RData")

dtm %>% inspect()

# Find the best number of topics
dtm %>% 
  FindTopicsNumber(topics = seq(from=2, to=30, by=6),
                   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010","Deveaud2014"),
                   method = "Gibbs",
                   control = list(seed=1009)) %>% 
  FindTopicsNumber_plot()


dtm %>% 
  FindTopicsNumber(topics = seq(from=2, to=14, by=3),
                   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010","Deveaud2014"),
                   method = "Gibbs",
                   control = list(seed=1009)) %>% 
  FindTopicsNumber_plot()



dtm %>% 
  FindTopicsNumber(topics = seq(from=2, to=8, by=1),
                   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010","Deveaud2014"),
                   method = "Gibbs",
                   control = list(seed=1009)) %>% 
  FindTopicsNumber_plot()

## We decided that the optimal number of topics is 5~6

library(topicmodels)

dtm.lda.5 <-
  dtm %>% 
  LDA(control = list(seed = 1009), k=5)

dtm.lda.6 <-
  dtm %>% 
  LDA(control = list(seed = 1009), k=6)

dtm.lda.5 %>% terms(10)
dtm.lda.6 %>% terms(10)

###### CTM
dtm.ctm.5 <-
  dtm %>% 
  CTM(control = list(seed = 1009), k=5)

dtm.ctm.6 <-
  dtm %>% 
  CTM(control = list(seed = 1009), k=6)

dtm.ctm.5 %>% terms(10)
dtm.ctm.6 %>% terms(10)

library(stm)
library(tidyverse)
library(tidytext)
library(tm)
library(readxl)

library(future)
plan(multisession, workers = 2)

library(showtext)
font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()

df <- read_excel("data/NewsResult_19900101-20241031.xlsx")

df <- df |> select(`제목`, `일자`, `언론사`, `키워드`)
colnames(df) <- c("Title", "Date", "Press", "words")

df$Year <- year(as.Date(df$Date, format = "%Y%m%d"))
df$Keywords <- gsub(",", " ", df$words)

df <- df |>
  mutate(Press_ = case_when(`Press`=="조선일보" ~ "Josun",
                            `Press`=="중앙일보" ~ "Jungang",
                            `Press`=="동아일보" ~ "Donga",
                            `Press`=="한겨레" ~ "Hangyere",
                            `Press`=="경향신문" ~ "Kyeonghyang",
                            `Press`=="문화일보" ~ "Munhwa",
                            `Press`=="서울신문" ~ "Seoul",
                            `Press`=="국민일보" ~ "Kookmin",
                            `Press`=="세계일보"~ "Segye",
                            `Press`=="한국일보" ~ "Hankook",
                            TRUE ~ "etc")) |> 
  mutate(Polarity_ = case_when(Press_ %in% c("Josun", "Jungang", "Donga") ~ "conservative",
                               Press_ %in% c("Hangyere", "Kyeonghyang") ~ "liberal",
                               TRUE ~ "moderate")) |> 
  mutate(Era = ifelse(Year >= 2022, 2, 
                      ifelse(Year < 2014, 0, 1))) |> 
  select("Title", "Era", "Polarity_", "Keywords")

#Era 0 -> Kim
#Era 1 -> Jang
#Era 2 -> Choi

processed <- textProcessor(documents = df$Keywords, metadata = df)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

topicN <- seq(3, 10)
N <- future({
  searchK(documents = out$documents, 
          vocab = out$vocab, 
          K = topicN, 
          prevalence = ~Era + Polarity_,
          data = out$meta, 
          init.type = "Spectral",
          seed=123)
})
topicN_storage <- value(N)

plot(topicN_storage)

M <- future({
  stm(documents = out$documents, 
      vocab = out$vocab, 
      K = 6, 
      prevalence = ~Era + Polarity_,
      data = out$meta, 
      init.type = "Spectral",
      seed=123)
})

stm_model1 <- value(M)

plot(stm_model1, type = "summary")
plot(stm_model1, type = "labels")

labelTopics(stm_model1)

td_beta <- stm_model1 |> 
  tidy(matrix = 'beta') 

td_beta |>
  group_by(topic) |> 
  slice_max(beta, n = 8) |> 
  ungroup() |> 
  mutate(topic = str_c("topic ", topic)) |> 
  ggplot(aes(x = beta, 
             y = reorder_within(term, beta, topic),
             fill = topic)) +
  geom_col(show.legend = F) +
  scale_y_reordered() +
  facet_wrap(~topic, scales = "free") +
  labs(x = expression("word probability distribution: "~beta), y = NULL,
       title = "word probability distribution per topic") +
  theme(plot.title = element_text(size = 15))

top_terms <- td_beta |>  
  group_by(topic) |> 
  slice_max(beta, n = 8) |> 
  select(topic, term) |> 
  summarise(terms = str_flatten(term, collapse = ", ")) 

td_gamma <- stm_model1 |> tidy(matrix = 'gamma') 
td_gamma |> 
  mutate(max = max(gamma),
         min = min(gamma),
         median = median(gamma))

td_gamma |> 
  ggplot(aes(x = gamma, fill = as.factor(topic))) +
  geom_histogram(bins = 100, show.legend = F) +
  facet_wrap(~topic) + 
  labs(title = "document probability distribution per topic",
       y = "number of document", x = expression("document probability distribution: "~(gamma))) +
  theme(plot.title = element_text(size = 20))

gamma_terms <- td_gamma |> 
  group_by(topic) |> 
  summarise(gamma = mean(gamma)) |> 
  left_join(top_terms, by = 'topic') |> 
  mutate(topic = str_c("topic", topic),
         topic = reorder(topic, gamma))

gamma_terms |> 
  ggplot(aes(x = gamma, y = topic, fill = topic)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = round(gamma, 2)), 
            hjust = 1.4) +                
  geom_text(aes(label = terms), 
            hjust = -0.05) +              
  scale_x_continuous(expand = c(0, 0),    
                     limit = c(0, 1)) +   
  labs(x = expression("document probability distribution"~(gamma)), y = NULL,
       title = "top word of article about bipolar") +
  theme(plot.title = element_text(size = 20))

cor <- topicCorr(stm_model1)

cor$cor

plot(cor)

stm_effects <- estimateEffect(seq(1,6) ~Era+Polarity_, 
                              stmobj = stm_model1,
                              meta = out$meta,
                              uncertainty = "Global")

summary(stm_effects)

plot(stm_effects, 
     covariate = "Era", 
     method = "continuous",
     topics = c(1), 
     model = stm_model1,
     xlab = "Time",
     main = "Topic Prevalence Over Time",
)
plot(stm_effects, 
     covariate = "Era", 
     method = "continuous",
     topics = c(2), 
     model = stm_model1,
     xlab = "Time",
     main = "Topic Prevalence Over Time",
)
plot(stm_effects, 
     covariate = "Era", 
     method = "continuous",
     topics = c(3), 
     model = stm_model1,
     xlab = "Time",
     main = "Topic Prevalence Over Time",
)
plot(stm_effects, 
     covariate = "Era", 
     method = "continuous",
     topics = c(4), 
     model = stm_model1,
     xlab = "Time",
     main = "Topic Prevalence Over Time",
)
plot(stm_effects, 
     covariate = "Era", 
     method = "continuous",
     topics = c(5), 
     model = stm_model1,
     xlab = "Time",
     main = "Topic Prevalence Over Time",
)
plot(stm_effects, 
     covariate = "Era", 
     method = "continuous",
     topics = c(6), 
     model = stm_model1,
     xlab = "Time",
     main = "Topic Prevalence Over Time",
)

plot(stm_effects, 
     covariate = "Era", 
     method = "continuous",
     topics = c(1:6), 
     model = stm_model1,
     xlab = "Time",
     main = "Topic Prevalence Over Time",
)

M <- future({
  stm(documents = out$documents, 
      vocab = out$vocab, 
      K = 6, 
      prevalence = ~Era+Polarity_,
      content = ~Polarity_,
      data = out$meta, 
      init.type = "Spectral",
      seed=123)
})

stm_model2 <- value(M)

plot(stm_model2, type="perspectives", topics=1)
plot(stm_model2, type="perspectives", topics=2)
plot(stm_model2, type="perspectives", topics=3)
plot(stm_model2, type="perspectives", topics=4)
plot(stm_model2, type="perspectives", topics=5)
plot(stm_model2, type="perspectives", topics=6)

stm_effects2 <- estimateEffect(seq(1,6) ~Era+Polarity_,
                              stmobj = stm_model2,
                              meta = out$meta,
                              uncertainty = "Global")

plot(stm_effects2, 
     covariate = "Polarity_", 
     topics = seq(1,6), 
     model = stm_model2,
     method = "difference",
     cov.value1 = "liberal",
     cov.value2 = "conservative",
     xlim = c(-.3, .3),
     main = "liberal VS conservative",
     xlab = "보수적 ........ 진보적")


M <- future({
  stm(documents = out$documents, 
      vocab = out$vocab, 
      K = 6, 
      prevalence = ~Polarity_*Era,
      data = out$meta, 
      init.type = "Spectral",
      seed=123)
})

stm_model3 <- value(M)

stm_effects3 <- estimateEffect(c(6) ~ Polarity_*Era, 
                               stmobj = stm_model3,
                               meta = out$meta,
                               uncertainty = "None")
plot(stm_effects3, covariate="Era", 
     model=stm_model3, method="continuous", xlab="Year", moderator="Polarity_",
     moderator.value="liberal", linecol="blue", ylim=c(-.3,.8), printlegend=F)
plot(stm_effects3, covariate="Era", 
     model=stm_effects3, method="continuous", xlab="Year", moderator="Polarity_",
     moderator.value="conservative", linecol="red",ylim=c(-.3,.8), add=T, printlegend=F)
legend(0,.8, c("liberal", "conservative"), lwd=2, col=c("blue", "red"))


save(stm_model1, file = "result/stm_model1.RData")
save(stm_model2, file = "result/stm_model2.RData")
save(stm_model3, file = "result/stm_model3.RData")
save(out, file = "result/prepDocuments.RData")
save(topicN_storage, file = "result/topicN_storage.RData")

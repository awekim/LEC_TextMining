library(tidytext)
library(tidyverse)
library(showtext)
library(readxl)
library(gt)

font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()

df <- read_excel("data/NewsResult_19900101-20241031.xlsx")
df <- df |> select(`제목`, `일자`, `언론사`, `키워드`)
colnames(df) <- c("Title", "Date", "Press", "Keywords")

df$Year <- year(as.Date(df$Date, format = "%Y%m%d"))

tf_df <- df |> 
  unnest_tokens(words, Keywords) |> 
  count(Title, words, sort = TRUE)

tf <- tf_df |>  
  group_by(Title) |> 
  summarize(total = sum(n))

tf_df <- left_join(tf_df, tf)

tfidf_df <- tf_df |> 
  bind_tf_idf(words, Title, n)

tfidf_df |> 
  filter(nchar(words) >= 2) |> 
  group_by(words) |> 
  reframe(score = sum(tf_idf)) |>
  arrange(desc(score)) |> 
  head(20) |> 
  ggplot(aes(x = score, y = reorder(words, score), fill=words))+
  geom_col() + 
  theme(legend.position = "none") + 
  xlab("tf-idf score")+
  ylab("words") +
  ggtitle("The Frequency of words in article about handong at 1995~2024")

tfidf_df |> 
  filter(nchar(words) >= 2) |> 
  group_by(words) |> 
  reframe(score = sum(tf_idf)) |>
  arrange(desc(score)) |> 
  head(20) |> 
  gt() |> 
  tab_header(title = "The Frequency of words in article about handong university",
             subtitle = "from January 1995 to October 2024")
  

url_v <- "https://github.com/park1200656/KnuSentiLex/archive/refs/heads/master.zip"
dest_v <- "data/knusenti.zip"

download.file(url = url_v, 
              destfile = dest_v,
              mode = "wb")

unzip("data/knusenti.zip", exdir = "data")
senti_file_list <- list.files("data/KnuSentiLex-master/",
                              full.names = TRUE)

knu_dic_df <- read_tsv(senti_file_list[9], col_names = FALSE) |> 
  rename(word = X1, sScore = X2) |> 
  filter(!is.na(sScore)) 

df |> 
  unnest_tokens(word, Keywords) |> 
  inner_join(knu_dic_df) |> 
  group_by(Title) |> 
  summarise(senti_score = mean(sScore)) -> senti_score

senti_df <- left_join(df, senti_score)

senti_df |> 
  filter(Year>=1995) |> 
  group_by(Year) |> 
  reframe(sentiment_score = mean(senti_score, na.rm = T)) |> 
  ggplot(aes(x = Year, y = sentiment_score)) + 
  geom_line(col="blue") +
  geom_hline(yintercept=0, size=.5) +
  scale_x_continuous(breaks = seq(1960, 2024, 5), labels = seq(1960, 2024, 5)) +
  scale_y_continuous(limits = c(-1, 1))+
  xlab("Year")+
  ylab("Sentiment Score(+ : Positive, - : Negative)") +
  ggtitle("The Sentiment score of article about handong at 1995~2024")

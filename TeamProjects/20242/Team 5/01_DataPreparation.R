################################################################################################
### Text Mining Team Project                                                                 ###
### Project Title: Beyond the Questions : Analyzing Interview Data for Strategic Preparation ###
### Project Goal: Developing Interview Preparation Strategies through Question Analysis      ### 
###               Uncovering Hidden Topics and Evaluation Criteria.                          ###
### Project Members: YoungWoo Cho ([jayjo9](https://github.com/jayjo9))                      ###
###                  Jiho Kang ([hahahohoJIHO](https://github.com/hahahohoJIHO))             ###
###                  Yewon Kim ([daydayyewon](https://github.com/daydayyewon))               ###
################################################################################################

library(jsonlite)
library(zip)
library(dplyr)
library(magrittr)
library(stringr)
library(tidyr)
library(tm)
library(tidytext)
library(KoNLP) 
library(extrafont)
library(showtext)
library(RmecabKo)
library(reticulate)
library(ggplot2)
library(wordcloud2)
library(ggwordcloud)
library(patchwork)
library(topicmodels)
library(gridExtra)
library(LDAvis)
library(stm)
library(keras)
library(igraph)
library(NLP)
library(ggraph)
library(widyr)

#######################################################
###############     0. Data loading     ###############
#######################################################

# Step1. Set the data file direction
directory_path <- "/Users/joy/Desktop/2024-2/Text\ Mining/TeamProject/TM\ Team\ Project/data\ 3"

# Step2. Get list of all folders in the directory
subfolders <- list.dirs(path = directory_path, recursive = TRUE, full.names = TRUE)[21:48]

# Step3. Save JSON file in each subfolder
subfolder_json_data <- lapply(subfolders, function(subfolder) {
  # Get JSON file names
  json_files <- list.files(path = subfolder, pattern = "\\.json$", full.names = TRUE)
  # Read JSON file
  json_data <- lapply(json_files, function(file) {
    tryCatch({
      fromJSON(file, flatten = TRUE)
    }, error = function(e) {
      # When an error occurs, print a message and return NULL
      message(sprintf("Error occurred: %s\nSkipping the file. (%s)", e$message, file))
      NULL
    })
  })
  # Transform to list
  list(subfolder = subfolder, json_data = json_data)
})

# Step4. Create data_frame containing data we need
data_frame <- do.call(rbind, lapply(subfolder_json_data, function(folder) {
  # Extract file path
  subfolder_path <- folder$subfolder
  # Extract question text
  questions <- unlist(lapply(folder$json_data, function(json) {
    tryCatch({
      json$dataSet$question$raw$text
      # Error case
    }, error = function(e) {
      NULL
    })
  }), use.names = FALSE)
  # Combine subfolder path and question text
  if (length(questions) > 0) {
    data.frame(Subfolder = subfolder_path, Question = questions, stringsAsFactors = FALSE)
    # Return NULL if there is no question
  } else {
    NULL  
  }
}))

# Step5. Create new column containing new information using file path column
data_frame %<>%
  mutate(
    Category = str_match(Subfolder, "TL_\\d+\\.([A-Za-z]+)_([A-Za-z]+)_([A-Za-z]+)")[, 2],
    Gender = str_match(Subfolder, "TL_\\d+\\.([A-Za-z]+)_([A-Za-z]+)_([A-Za-z]+)")[, 3],
    Experience = str_match(Subfolder, "TL_\\d+\\.([A-Za-z]+)_([A-Za-z]+)_([A-Za-z]+)")[, 4]
  ) %>% select(-1) %>% distinct()

# Step6. Check the result
data_frame %>% head()

######## Font installation for Korean display ########

font_import(prompt = FALSE)
loadfonts(device = "pdf")
fonts()

font_add_google("Nanum Gothic", "nanum")
showtext_auto()
font_families()

###############   1. Data Exploration   ###############

# Check data structure
str(data_frame)

# tibble [58,787 × 7] (S3: tbl_df/tbl/data.frame)
# $ Question  : chr [1:58787] "요즘 논쟁이 많은 우버 택시 찬반에 대해서 논해 보시겠습니까" "요즘 논쟁이 많은 우버 택시 찬반에 대해서 논해 보시겠습니까" "요즘 논쟁이 많은 우버 택시 찬반에 대해서 논해 보시겠습니까" "요즘 논쟁이 많은 우버 택시 찬반에 대해서 논해 보시겠습니까" ...
# $ Category  : chr [1:58787] "Management" "Management" "Management" "Management" ...
# $ Gender    : chr [1:58787] "Female" "Female" "Female" "Female" ...
# $ Experience: chr [1:58787] "Experienced" "Experienced" "Experienced" "Experienced" ...
# $ char_count: int [1:58787] 33 33 33 33 33 89 89 89 89 89 ...
# $ word_count: int [1:58787] 9 9 9 9 9 22 22 22 22 22 ...
# $ words     : chr [1:58787] "논쟁" "우버" "택시" "찬반" ...

# Summary Statistics
summary(data_frame)

# Question           Category            Gender           Experience          char_count       word_count      words   
# Length:58787       Length:58787       Length:58787       Length:58787       Min.   :  9.00   Min.   : 2.00   Length:58787  
# Class :character   Class :character   Class :character   Class :character   1st Qu.: 61.00   1st Qu.:15.00   Class :character
# Mode  :character   Mode  :character   Mode  :character   Mode  :character   Median : 70.00   Median :18.00   Mode  :character  
# Mean   : 71.66   Mean   :17.96  
# 3rd Qu.: 82.00   3rd Qu.:20.00  
# Max.   :132.00   Max.   :39.00  
       

# Check NA
colSums(is.na(data_frame))

# [Result]
# Question   Category   Gender 
# 0          0          0 
# Experience char_count word_count 
# 0          0          0 

# Check the sample in the main text column
head(data_frame$Question, 5)
table(data_frame$Category)

# [Result]
# "지금 지원자분이 제일 가고 싶은 회사가 어딜까요"                                                                      
# "지원자님은 이전 직장을 그만둔 어떤 특별한 이유가 있습니까 그리고 현 직장도 혹시 같은 이유로 그만둘 가능성이 있을까요"
# "지원자님은 이전 직장을 그만둔 어떤 특별한 이유가 있습니까 그리고 현 직장도 혹시 같은 이유로 그만둘 가능성이 있을까요"
# "지원자님은 이전 직장을 그만둔 어떤 특별한 이유가 있습니까 그리고 현 직장도 혹시 같은 이유로 그만둘 가능성이 있을까요"
# "지원자님은 이전 직장을 그만둔 어떤 특별한 이유가 있습니까 그리고 현 직장도 혹시 같은 이유로 그만둘 가능성이 있을까요"

set.seed(123)
data_frame %>% filter(Category=="Management" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="Management" & Gender=="Male" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="Design" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="Design" & Gender=="Male" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="ICT" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="ICT" & Gender=="Male" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="ProductionManufacturing" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="ProductionManufacturing" & Gender=="Male" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="PublicService" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="PublicService" & Gender=="Male" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="SalesMarketing" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="SalesMarketing" & Gender=="Male" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="RND" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="RND" & Gender=="Male" & Experience == "New") %>% nrow()

data_frame %>% filter(Category=="Management" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="Management" & Gender=="Male" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="Design" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="Design" & Gender=="Male" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="ICT" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="ICT" & Gender=="Male" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="ProductionManufacturing" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="ProductionManufacturing" & Gender=="Male" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="PublicService" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="PublicService" & Gender=="Male" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="SalesMarketing" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="SalesMarketing" & Gender=="Male" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="RND" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="RND" & Gender=="Male" & Experience == "Experienced") %>% nrow()

#####################################
row_remove <- data_frame %>%
  filter(Category=="Management" & Gender=="Male" & Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 3900)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category=="Management" & Gender=="Female" & Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 7400)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category=="Management" & Gender=="Male" & Experience == "Experienced") %>%  # 조건 필터링
  slice_sample(n = 800)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category=="Management" & Gender=="Female" & Experience == "Experienced") %>%  # 조건 필터링
  slice_sample(n = 500)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")

row_remove <- data_frame %>%
  filter(Category=="Design" & Gender=="Female" & Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 3800)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category=="Design" & Gender=="Male" & Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 1050)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")


row_remove <- data_frame %>%
  filter(Category=="ICT" & Gender=="Male" & Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 2600)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category=="ICT" & Gender=="Female" & Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 1100)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")


row_remove <- data_frame %>%
  filter(Category == "ProductionManufacturing", Gender == "Female", Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 2800)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category == "ProductionManufacturing", Gender == "Male", Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 1800)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")


row_remove <- data_frame %>%
  filter(Category == "PublicService", Gender == "Female", Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 9050)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category == "PublicService", Gender == "Male", Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 3300)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category == "PublicService", Gender == "Female", Experience == "Experienced") %>%  # 조건 필터링
  slice_sample(n = 1100)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category == "PublicService", Gender == "Male", Experience == "Experienced") %>%  # 조건 필터링
  slice_sample(n = 400)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")


row_remove <- data_frame %>%
  filter(Category == "SalesMarketing", Gender == "Female", Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 5700)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category == "SalesMarketing", Gender == "Male", Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 2400)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category == "SalesMarketing", Gender == "Female", Experience == "Experienced") %>%  # 조건 필터링
  slice_sample(n = 300)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category == "SalesMarketing", Gender == "Male", Experience == "Experienced") %>%  # 조건 필터링
  slice_sample(n = 600)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")

row_remove <- data_frame %>%
  filter(Category == "RND", Gender == "Male", Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 1600)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category == "RND", Gender == "Female", Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 1100)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")

data_frame %>% filter(Category=="Management" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="Management" & Gender=="Male" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="Design" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="Design" & Gender=="Male" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="ICT" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="ICT" & Gender=="Male" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="ProductionManufacturing" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="ProductionManufacturing" & Gender=="Male" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="PublicService" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="PublicService" & Gender=="Male" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="SalesMarketing" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="SalesMarketing" & Gender=="Male" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="RND" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="RND" & Gender=="Male" & Experience == "New") %>% nrow()

data_frame %>% filter(Category=="Management" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="Management" & Gender=="Male" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="Design" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="Design" & Gender=="Male" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="ICT" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="ICT" & Gender=="Male" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="ProductionManufacturing" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="ProductionManufacturing" & Gender=="Male" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="PublicService" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="PublicService" & Gender=="Male" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="SalesMarketing" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="SalesMarketing" & Gender=="Male" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="RND" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="RND" & Gender=="Male" & Experience == "Experienced") %>% nrow()

# Check interview question length and distribution
data_frame$char_count <- nchar(data_frame$Question)
data_frame$word_count <- sapply(strsplit(data_frame$Question, "\\s+"), length)

summary(data_frame$char_count)
# Min. 1st Qu.  Median  Mean 3rd Qu.    Max.
# 9.00   60.00   70.00  71.25   82.00  131.00
 
summary(data_frame$word_count)
# Min. 1st Qu.  Median   Mean  3rd Qu.    Max. 
# 2.00   15.00   18.00  17.92   21.00   39.00 

hist(data_frame$char_count, main = "Character Count Distribution", xlab = "Character Count")
hist(data_frame$word_count, main = "Word Count Distribution", xlab = "Word Count")

# [Interpretation]
# * word 
# There are questions between at least 9 and 131 characters, with an average of 71 characters.
# Multiple questions are distributed in the range of 61 to 82 characters. Most questions are relatively constant in length. 
# It can be seen that the distribution of interview questions is similar to the normal distribution and is balanced.


###############   2. Data Preprocessing   ###############

# Unnecessary symbol removal
data_frame <- data_frame %>%
  mutate(Question = str_replace_all(Question, "[^가-힣\\s]", "")) %>%
  mutate(Question = str_squish(Question))

# Morphological analysis and noun extraction: using KoNLP
useNIADic()

data_frame <- data_frame %>%
  mutate(words = sapply(Question, extractNoun)) %>%
  unnest(words) # Spread the list to frame the data

# Check the frequency of words
word_frequency <- data_frame %>%
  count(words, sort = TRUE) %>%
  head(50)

print(word_frequency, n=50)

# [Result] 
# * We were able to confirm the existence of meaningless words consisting of Korean alphabet in frequently appearing words.
#    words      n
# 1  해      9714
# 2  지원자  7362
# 3  말씀    7134
# 4  님      6748
# 5  것      4701
# 6  생각    4433
# 7  한      4015
# 8  본인    3688
# 9  일      3519
# 10 무엇    3062
# 11 수      2857
# 12 설명    2716
# 13 이유    2597
# 14 때      2033
# 15 하시    1868
# 16 궁금    1830
# 17 하      1803
# 18 적      1791
# 19 회사    1783
# 20 업무    1722
# 21 경험    1716
# 22 하게    1641
# 23 그것    1530
# 24 해결    1516
# 25 할      1401
# 26 중      1246
# 27 노력    1183
# 28 상황    1149
# 29 번      1133
# 30 분      1130
# 31 공부    1091
# 32 들       996
# 33 직무     969
# 34 방법     956
# 35 지원     952
# 36 합       890
# 37 수행     825
# 38 하기     817
# 39 우리     808
# 40 분야     774
# 41 점       746
# 42 면접자   718
# 43 문제     701
# 44 가지     696
# 45 관리     696
# 46 입사     695
# 47 자신     693
# 48 지       676
# 49 시간     663
# 50 직장     645

# Remove one-character word 
data_frame <- data_frame %>%
  filter(str_length(words) > 1)

# Check the word frequency 
word_frequency <- data_frame %>%
  count(words, sort = TRUE) %>%
  head(50)
print(word_frequency, n=50)

# Load Stopword file
stopwords_ko <- readLines("stopwords-ko.txt", encoding = "UTF-8")
additional_stopwords <- c("면접자", "그것","지원자", "말씀", "하신", "있습니다", "있으세요", "생각", "본인", "이유",
                          "설명", "궁금", "하시", "하게", "그것", "하기", "구체", "주시", "가지", "해서", 
                          "면접", "부탁드리겠습니", "얘기", "평소", "주시겠습니", "좋겠습니", "주십시", "들이", "일하")

data_frame <- data_frame %>%
  filter(!words %in% c(stopwords_ko, additional_stopwords))

word_frequency <- data_frame %>%
  count(words, sort = TRUE) %>%
  head(50)
print(word_frequency, n=50)

# Check the top 20 words (based on frequency)
top_20_words <- word_frequency %>% head(20)
print(top_20_words)

#     words        n
# 1  회사      1783
# 2  업무      1722
# 3  경험      1716
# 4  해결      1516
# 5  노력      1183
# 6  상황      1149
# 7  공부      1091
# 8  직무       969
# 9  방법       956
# 10 지원       952
# 11 수행       825
# 12 분야       774
# 13 문제       701
# 14 관리       696
# 15 입사       695
# 16 직장       645
# 17 단점       643
# 18 장점       637
# 19 프로젝트   637
# 20 갈등       627

ggplot(top_20_words, aes(x = reorder(words, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Most Frequently Used Words in Job Interviews",
    x = "Words",
    y = "Frequency"
  ) +
  theme_minimal()

# [Result]
# Words such as "experience", "company", "solution", "work", "study", and "job" are ranked highly.
# Overall, interview questions tend to emphasize the <interviewee's career background, job-related competencies, and problem-solving abilities.>
# Specifically, keywords like "effort", 'method", and "application" indicate that many questions provide opportunities for candidates to explain <their own approaches> and <motivated attitudes>.
# Words like "stress" and "overcome" suggest that many questions focus on evaluating <the interviewee's coping skill and psychological stability>. 

# [Get some Insight Here]
# Interview questions appear to focus on evaluating <core competencies> related to actual job performance, such as experience, problem-solving, and stress management.
# Therefore, interviewees should prepare specific examples of their problem-solving experiences and the lessons learned from them.
# It also seems important for interviewees to effectively express their psychological resilience, including examples of how they handled <stressful situations>.
# Based on this data, it is expected to be useful for analyzing question trends by job category or generating tailored question examples.

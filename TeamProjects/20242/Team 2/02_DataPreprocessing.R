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
library(parallel)
library(stringr)
library(tm)
library(bitNLP)

dirty_word <- 
  c('×같'='좆같', '×까'='좆까', '×나'='좆나', '×되'='좆되', '×밥'='좆밥', 
    '×랄'='지랄', '×발'='씨발', '×끼'='새끼', '×됐'='좆됐', '×새끼'='개새끼', 
    '×친'='미친', '개××'='개새끼', '씨×'='씨발', '새×'='새끼', '존×'='좆나', 
    '×신'='병신', '시×'='시발', '시×'='시발', '지×'='지랄', '병×'='병신', 
    '×시'='빙시', '×바'='시바', '×소리'='개소리', '×도'= '좆도', '걸레×'='걸레년',
    '개×'='개새끼', '걸×'='걸레', '개새×'='개새끼', '×레기'= '쓰레기', 
    '×빠지게'='좆빠지게')
# '×년'='썅년'
# '나쁜×' = '나쁜놈' or '미친년'
# '미친×'='미친놈' or '미친년'
unblind_dirty_word <-function(x) str_replace_all(x, dirty_word)
remove_except_kor <- function(x) gsub('[^가-힣|ㄱ-ㅎ|ㅏ-ㅣ]{1,}', ' ', x)
pattern = "(^NNG$|^NNP$|^NNB$|^NNBC$|^NR$|^NP$|^VV$|^VA$|^MM$|^MAG$|^MAJ$|^IC$)"
morpheme_analysis <- 
  function(x) morpho_mecab(x, pattern = pattern, 
                           indiv = FALSE, encoding = 'UTF-8')
remove_consonant_vowel <- function(x) str_remove_all(x, '[ㅏ-ㅣ|ㄱ-ㅎ]{1,}')
remove_one_letter <- function(x) str_remove_all(x, '^[가-힣]{1}$')
normalize_consonant <- function(s){
  if (str_detect(s, '[ㄱ-ㅎ]{3,}'))
    s %>% substr(1, 2)
  else
    s
}

preprocessing <- function(s) {
  s %>% 
    unblind_dirty_word %>% 
    remove_except_kor %>% 
    morpheme_analysis %>% 
    lapply(normalize_consonant) %>% 
    unlist %>% 
    remove_consonant_vowel %>% 
    remove_one_letter
}

preprocessing_export_doc <- function(doc, dirpath) {
  dir.create(file.path(dirpath), recursive = TRUE, showWarnings = FALSE)
  filepath <- paste(c(dirpath, doc$meta$id), collapse = '/')
  print(paste(c(filepath)))
  if (!file.exists(filepath) || file.size(filepath) == 0) {
    cleaned <- preprocessing(doc$content)
    sink(filepath)
    cat(cleaned[cleaned != ''])
    sink()
    "Processed"
  }
  "Skipped"
}

export_preprocessing_mp <- function(processes, output_path, corpus) {
  cl <- makeCluster(processes)
  clusterEvalQ(cl, {
    library(magrittr)
    library(dplyr)
    library(parallel)
    library(stringr)
    library(bitNLP)
    library(tm)
  })
  clusterExport(cl, "remove_except_kor")
  clusterExport(cl, "dirty_word")
  clusterExport(cl, "unblind_dirty_word")
  clusterExport(cl, "pattern")
  clusterExport(cl, "morpheme_analysis")
  clusterExport(cl, "remove_consonant_vowel")
  clusterExport(cl, "remove_one_letter")
  clusterExport(cl, "normalize_consonant")
  clusterExport(cl, "preprocessing")
  clusterExport(cl, "preprocessing_export_doc")
  print(parLapply(cl = cl, X = corpus,
            function(x) {preprocessing_export_doc(x, output_path)}))
  stopCluster(cl)
}

################################################################################
### Data Preprocessing
################################################################################
dataset_male <- VCorpus(DirSource("dataset/male"), 
                        readerControl=list(language='ko'))
dataset_female <- VCorpus(DirSource("dataset/female"), 
                          readerControl=list(language='ko'))
processes <- detectCores()
export_preprocessing_mp(processes, 'output/male', dataset_male)
export_preprocessing_mp(processes, 'output/female', dataset_female)

################################################################################
### TEST CODE
################################################################################

### test custom `morpho_mecab` function
test_s <- "아버지가 방에 들어가신다. ㅋㅋ ㅇㅇㅇㅇ ㅋㅋㅋㅋㅋㅋ ㅅㅅㅅ ㅎ"
test_s2 <- "님은 갔습니다. 차마 아아, 사랑하는 나의 님은 갔습니다. ㅋㅋ ㅎㅎㅎ ㅇㅅㅇ"
test_s3 <- "나는 대학생입니다."
test_s4 <- "아버지가 방에 들어가신다. ㅋㅋ ㅇㅇㅇ ㅋㅋㅋㅋ 님은 갔습니다. 차마 아아, 사랑하는 나의 님은 갔습니다. ㅋㅋ ㅎㅎㅎ ㅇㅅㅇ 나는 대학생입니다. 사과가 있다. 차마, 그것을 볼 수 없었습니다."
test_s5 <- "사과가 있다"
pattern = "(^NNG$|^NNP$|^NNB$|^NNBC$|^NR$|^NP$|^VV$|^VA$|^MM$|^MAG$|^MAJ$|^IC$)"

morpho_mecab(test_s)
morpho_mecab(test_s, pattern = pattern)
morpho_mecab(test_s2)
morpho_mecab(test_s2, pattern = pattern)
morpho_mecab(test_s3)
morpho_mecab(test_s3, pattern = pattern)
morpho_mecab(test_s4)
morpho_mecab(test_s4, pattern = pattern)
morpho_mecab(test_s5)
morpho_mecab(test_s5, pattern = pattern)

### Test Pattern of Morphological Analysis
# (^NNG$|^NNP$|^NNB$|^NNBC$|^NR$|^NP$|^VV$|^VA$|^MM$|^MAG$|^MAJ$|^IC$)
x <- morpho_mecab(dataset_male[[1]]$content, indiv = FALSE, encoding = 'UTF-8')
x[names(x) == "IC"]

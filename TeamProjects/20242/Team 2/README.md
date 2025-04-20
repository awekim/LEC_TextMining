
# Installation

## 02_DataPreprocessing.R

- install mecab-ko(https://r2bit.com/bitNLP/articles/Install_mecab.html)

- install [`ccss17/bitNLP`](https://github.com/ccss17/bitNLP)

    ```R
    install.packages("remotes")
    remove.packages("bitNLP")
    remotes::install_github("ccss17/bitNLP")
    install_mecab_ko()
    install.packages("RcppMeCab")
    ```

- install necessary R library

    ```R
    install.packages("magrittr")
    install.packages("dplyr")
    install.packages("parallel")
    install.packages("stringr")
    install.packages("tm")
    ```

## 03_DataAnalysis.R

- install necessary R library

    ```R
    install.pakcages("magrittr")
    install.pakcages("dplyr")
    install.pakcages("tm")
    install.pakcages("str2str")
    install.pakcages("tidyr")
    install.pakcages("slam")
    install.pakcages("wordcloud2")
    ```

# word embedding: gender space

- Setting 1(window size: 7, rank weight step: 0.02)

    ![](img/(15,0.2)gender_embedding_space.png)

    after applied distinguish window:

    ![](img/(15,0.2)gender_embedding_space_d.png)

    comparison:

    ![](img/(15,0.2)gender_embedding_space_comparison.png)

# Visualization: WordCloud

## Setting 1(window size: 7, rank weight step: 0.02)

- male

    ![](img/(7,0.02)male.png)

- female

    ![](img/(7,0.02)female.png)

- male distinguished

    ![](img/(7,0.02)male_d.png)

- female distinguished

    ![](img/(7,0.02)female_d.png)

## Setting 2(window size: 10, rank weight step: 0.1)

- male

    ![](img/(10,0.1)male.png)

- female

    ![](img/(10,0.1)female.png)

- male distinguished

    ![](img/(10,0.1)male_d.png)

- female distinguished

    ![](img/(10,0.1)female_d.png)

## Setting 3(window size: 15, rank weight step: 0.2)

- male

    ![](img/(15,0.2)male.png)

- female

    ![](img/(15,0.2)female.png)

- male distinguished

    ![](img/(15,0.2)male_d.png)

- female distinguished

    ![](img/(15,0.2)female_d.png)

# ChatGPT Analysis

## Setting 1(window size: 7, rank weight step: 0.02)

- ChatGPT analysis(female wordcloud)

    ![](img/(7,0.02)female_chatgpt.png)

- ChatGPT analysis(male wordcloud)

    ![](img/(7,0.02)male_chatgpt.png)

- ChatGPT analysis

    ![](img/(7,0.02)male_female_chatgpt.png)

## Setting 3(window size: 15, rank weight step: 0.2)

- ChatGPT analysis(female wordcloud)

    ![](img/(15,0.2)female_chatgpt.png)

- ChatGPT analysis(male wordcloud)

    ![](img/(15,0.2)male_chatgpt.png)

- ChatGPT analysis

    ![](img/(15,0.2)male_female_chatgpt.png)

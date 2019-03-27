library(tidyverse)
library(tidytext)
library(data.table)
data(stop_words)

setwd("~/RCapstoneProject/")
##tbl <- list.files(path = "~/RCapstoneProject/final/en_US",pattern = "*.txt") %>% 
##    map_chr(~ read_file(.)) %>% 
##    data_frame(text = .)

read_folder <- function(infolder) {
  tibble(file = dir(infolder, full.names = TRUE)) %>%
    mutate(text = map(file, read_lines)) %>%
    transmute(id = basename(file), text) %>%
    unnest(text)
}

tbl <- read_folder("~/RCapstoneProject/final/en_US")

####
library(SnowballC)
word_count_test <- tbl %>% unnest_tokens(word, text) %>% mutate(word = wordStem(word)) %>% count(word, sort = TRUE)

word_count <- tbl %>% unnest_tokens(word, text) %>% count(word, sort = TRUE)
word_count_no_stop_words <- tbl %>% 
  unnest_tokens(word, text) %>% 
  filter(!str_detect(word, "^[0-9]*$")) %>%
  anti_join(stop_words) %>% count(word, sort = TRUE)

word_count_no_stop_words_test <- tbl %>% 
  unnest_tokens(word, text) %>% 
  filter(!str_detect(word, "^[0-9]*$")) %>% mutate(word = wordStem(word)) %>% 
  anti_join(stop_words) %>% count(word, sort = TRUE)


word_count_total <- sum(word_count$n)
word_count <- word_count %>% mutate(csum = cumsum(word_count$n)/word_count_total)

ggplot(data = word_count[1:20,], aes(x = word, y = n))+geom_bar(stat = "identity")+scale_x_discrete(limits=word_count[1:20,]$word)
ggplot(data = word_count, aes(x = seq(1,dim(word_count)[1],1), y = csum))+geom_line()+scale_x_log10()

word_count_no_stop_words_total <- sum(word_count_no_stop_words$n)
word_count_no_stop_words <- word_count_no_stop_words %>% mutate(csum = cumsum(word_count_no_stop_words$n)/word_count_no_stop_words_total)

ggplot(data = word_count_no_stop_words[1:20,], aes(x = word, y = n))+geom_bar(stat = "identity")+scale_x_discrete(limits=word_count_no_stop_words[1:20,]$word)
ggplot(data = word_count_no_stop_words, aes(x = seq(1,dim(word_count_no_stop_words)[1],1), y = csum))+geom_line()+scale_x_log10()

news_bigrams <- tbl %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

news_bigram_count <- news_bigrams %>%  count(bigram, sort = TRUE)
news_bigram_count_separated <- news_bigram_count %>% separate(bigram, c("word1", "word2"), sep = " ")

bigram_reduced <- news_bigram_count_separated[!duplicated(news_bigram_count_separated$word1),]
bigram_reduced1 <- subset(bigram_reduced, n > 5)
setDT(bigram_reduced1)
saveRDS(bigram_reduced1, file = "bigram_reduced.rds")


####
news_trigrams <- tbl %>% 
  unnest_tokens(trigram, text, token = "ngrams", n = 3)

news_trigram_count <- news_trigrams %>%  count(trigram, sort = TRUE)
news_trigram_count_separated <- news_trigram_count %>% separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% unite(word12, c("word1", "word2"), sep = " ")

trigram_reduced <- news_trigram_count_separated[!duplicated(news_trigram_count_separated$word12),]
trigram_reduced1 <- subset(trigram_reduced, n > 2)
setDT(trigram_reduced1)
saveRDS(trigram_reduced1, file = "trigram_reduced.rds")
####

news_tetragrams <- tbl %>% 
  unnest_tokens(tetragram, text, token = "ngrams", n = 4)

news_tetragram_count <- news_tetragrams %>%  count(tetragram, sort = TRUE)
news_tetragram_count_separated <- news_tetragram_count %>% separate(tetragram, c("word1", "word2", "word3", "word4"), sep = " ") %>% unite(word123, c("word1", "word2", "word3"), sep = " ")

tetragram_reduced <- news_tetragram_count_separated[!duplicated(news_tetragram_count_separated$word123),]
tetragram_reduced1 <- subset(tetragram_reduced, n > 2)
setDT(tetragram_reduced1)

saveRDS(tetragram_reduced1, file = "tetragram_reduced.rds")
####


###
subset(tetragram_reduced, word123 ==  "a case of")


object.size(news_bigrams)

counts <- numeric()
for(i in seq(1000000, dim(news_bigrams)[1], 1000000)){
  s_count <- news_bigrams[1:i,] %>%  count(bigram, sort = TRUE)
  counts <- c(counts, dim(s_count)[1])
}


######





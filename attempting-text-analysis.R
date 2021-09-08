library(tidyverse)
library(SnowballC)
library(tidytext)
library(qdap)
library(Rcpp)
data('Top200Words')

ladies <- read_rds("data/ladies.Rds")

unigram <- ladies %>% 
  mutate(text_clean = str_replace_all(text_nopunct, 
                                      "[Nn]ational\\s[Ss]ocialism[A-z]*", 
                                      "ns") %>% 
           str_replace_all("[Nn]ational\\s[Ss]ocialist[A-z]*",
                           "ns") %>% 
           str_replace_all("[Pp]regnant[A-z]*",
                           "pregnant") %>% 
           str_replace_all("[Hh]itlers*",
                           "hitler") %>%  
           str_replace_all("[A-z]*[Mm]ein(\\s)*[Kk]ampf[A-z]*",
                           "meinkampf") %>% 
           str_replace_all("[Nn]ationalis[tm]",
                           "national") %>% 
           str_replace_all("[A-z]*([Tt]hird|3rd)(\\s)*[Rr]eich[A-z]*",
                           "3rd_reich") %>% 
           str_replace_all("[Nn]azi",
                           "nazi")) %>%
  unnest_tokens(word, 
                text_clean) %>% 
  select(user, 
         time,
         date,
         word,
         id) %>%
  anti_join(stop_words)

#Add the word stemmed words. 
unigram <- unigram %>% 
  mutate(word_stem = wordStem(word),
         word_stem = ifelse(word == 'baby' | word == 'mother' | word == 'pregnant',
                            as.character(word),
                            word_stem))

#Create a word count dataframe
unigram_count <- unigram %>% 
  count(word, sort = T) %>% 
  mutate(word = reorder(word, n)) %>%  
  mutate(word_stem = wordStem(word),
         word_stem = ifelse(word == 'baby' | word == 'mother' | word == 'pregnant',
                            word,
                            word_stem))

####################
bigram <- ladies %>% 
  mutate(text_clean = str_replace_all(text_nopunct, 
                                      "[Nn]ational\\s[Ss]ocialism[A-z]*", 
                                      "ns") %>% 
           str_replace_all("[Nn]ational\\s[Ss]ocialist[A-z]*",
                           "ns") %>% 
           str_replace_all("[Pp]regnant[A-z]*",
                           "pregnant") %>% 
           str_replace_all("[Hh]itlers*",
                           "hitler") %>%  
           str_replace_all("[A-z]*[Mm]ein(\\s)*[Kk]ampf[A-z]*",
                           "meinkampf") %>% 
           str_replace_all("[Nn]ationalis[tm]",
                           "national") %>% 
           str_replace_all("[A-z]*([Tt]hird|3rd)(\\s)*[Rr]eich[A-z]*",
                           "3rd_reich") %>% 
           str_replace_all("[Nn]azi",
                           "nazi") %>%
  rm_stopwords(Top200Words, 
               separate = FALSE)) %>%
  unnest_tokens(bigram, 
                text_clean,
                token = "ngrams", 
                n = 2) %>% 
  select(user, 
         time,
         date,
         bigram,
         id)

#Use the tidytext way on the stop-word-removed data using the qdap approach. Similar Results. 
bigram_split <- bigram %>% 
  separate(bigram, c("word1", 
                     "word2"), 
           sep = " ",
           remove = F) %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!is.na(bigram)) %>% 
  count(bigram, sort = T) %>% 
  mutate(bigram = reorder(bigram, n))

#Count the frequency of bigrams and order them by frequency. 
bigram_sorted <- bigram %>% 
  count(bigram, sort = T) %>% 
  mutate(bigram = reorder(bigram, n))

ggplot(bigram_sorted, aes(x=bigram, y=n)) + geom_col() + coord_flip() +
  theme_bw()
  
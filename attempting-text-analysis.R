library(tidyverse)
library(SnowballC)
library(tidytext)
library(textdata)
library(qdap)
library(Rcpp)
data('Top200Words')

####more cleaning needed, better cleaning needed

ladies <- read_rds("data/ladies.Rds")

user_rank <- ladies %>%
  group_by(user) %>%
  summarise(n = n())

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
           str_replace_all("[Nn]ationalis[tm]",
                           "national") %>% 
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
#unigram <- unigram %>% 
#  mutate(word_stem = wordStem(word),
#         word_stem = ifelse(word == 'baby' | word == 'mother' | word == 'pregnant',
#                            as.character(word),
#                            word_stem))

#Create a word count dataframe
unigram_count <- unigram %>% 
  count(word, sort = T) %>% 
  mutate(word = reorder(word, n)) %>%  
  mutate(word_stem = wordStem(word),
         word_stem = ifelse(word == 'baby' | word == 'mother' | word == 'pregnant',
                            word,
                            word_stem))
###sort
unigram_sorted <- unigram %>% 
  count(word, sort = T) %>% 
  mutate(word = reorder(word, n))

unigram_200 <- unigram_sorted %>%
  subset(n >= 98)

ggplot(unigram_200, aes(x=n, y=word, fill=n)) + geom_col() + theme_bw() +
  scale_x_continuous(limit=c(0,4000),
                     breaks = c(0, 100, 150, 200, 250, 300, 400, 500,
                                600, 700, 800, 900, 1000,
                                1100, 1200, 1300, 1400, 1500,
                                2000, 3000, 3500),
                     expand = c(0,0)) +
  theme(axis.text = element_text(size=5)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Word Frequency on Women's Forums",
       x= "Freqeuncy",
       y="",
       fill = "Frequency (High-Low)")
ggsave("figs/unigram_ladies_200.png", height=12, width = 12)

###tone
unigram_nrc <- get_sentiments('nrc') %>% 
  filter(word != "white") %>% #white is neutral in this context
  inner_join(unigram) %>% 
  add_count(sentiment, 
            sort = T) %>% 
  inner_join(user_rank)

unigram_nrc_user<- get_sentiments('nrc') %>% 
  filter(word != "white") %>% #white is neutral in this context
  inner_join(unigram) %>% 
  group_by(user) %>% 
  add_count(sentiment, 
            sort = T) %>% 
  inner_join(user_rank)

# Binary positive or negative
unigram_bing <- inner_join(unigram, get_sentiments("bing")) %>% 
  add_count(sentiment,
            name = "n_sent") %>% 
  add_count(sentiment,
            index = id,
            name = "n_sent_id") %>% 
  add_count(id, 
            name = "num_words") %>% 
  inner_join(user_rank)

# -5 to 5 negative to positive
unigram_afinn <- inner_join(unigram, get_sentiments("afinn"))  %>% 
  group_by(id) %>% 
  separate(date,
           into = c('m', 'd', 'y'),
           sep = '-',
           remove = F) %>% 
  mutate(net_score = sum(value),
         date = as.Date(ISOdate(y, m, d))) %>% 
  select(user,
         date, 
         id,
         word,
         value,
         net_score) %>% 
  inner_join(user_rank)

###plot sentiment frequency
ggplot(unigram_nrc_user, aes(x=sentiment, fill=sentiment)) +
  geom_bar() + theme_bw() +
  scale_fill_manual(values = c("#FF0000", "#FFA500", "#89a203",
                      "#A020F0", "#FFFF00", "#000080",
                      "#ADD8E6", "#0000FF", "#228B22",
                      "#FFC0CB")) +
  labs(title = "NRC Sentiment Frequency in Women's Posts",
       x = "",
       y = "Frequency",
       fill = "NRC Sentiment")
ggsave("figs/sentiment_freq_ladies.png", width=8)


unigram_nrc_user <- unigram_nrc_user %>%
  mutate(date = as.Date(date, "%m-%d-%Y"))

ggplot(unigram_nrc_user, aes(x=date, fill=sentiment)) + 
  geom_density(alpha=0.5) + theme_bw() +
  scale_fill_manual(values = c("#FF0000", "#FFA500", "#89a203",
                               "#A020F0", "#FFFF00", "#000080",
                               "#ADD8E6", "#0000FF", "#228B22",
                               "#FFC0CB")) +
  scale_y_continuous() +
  labs(title = "NRC Sentiment Frequency in Women's Posts",
       x = "Date",
       y = "Density",
       fill = "NRC Sentiment") + facet_grid(vars(sentiment))
ggsave("figs/sentiment-over-time-women.png", width = 10, height = 10)
  
ggplot(unigram_afinn, aes(x=date, y=value)) + geom_col() +
  theme_bw() + scale_y_continuous() +
  labs(title = "Female AFINN Sentiment Analysis Over Time",
       x = "Date",
       y = "AFINN Polarity Score")
ggsave("figs/afinn-over-time-women.png")

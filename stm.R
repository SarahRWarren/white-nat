library(stm)
library(tidyverse)
library(SnowballC)
library(tidytext)
library(textdata)
library(qdap)
library(Rcpp)
library(furrr)
library(knitr)

#df <- read_rds("data/ideo_philo.Rds")
#write.csv(df, "data/ideo_philo.csv")
df <- read_rds("data/ideo_philo.Rds")

tidy_forum <- df %>%
  unnest_tokens(word, text_noquote, token = "tweets") %>%
  anti_join(get_stopwords()) %>%
  filter(!str_detect(word, "[0-9]+")) %>%
  add_count(word) %>%
  filter(n > 100) %>%
  select(-n)

forum_sparse <- tidy_forum %>%
  count(id, word) %>%
  cast_sparse(id, word, n)

plan(multiprocess)
many_models <- data_frame(K = c(20, 40, 50, 60, 70, 80, 100)) %>%
  mutate(topic_model = future_map(K, ~stm(forum_sparse, K = .,
                                          verbose = FALSE)))

heldout <- make.heldout(forum_sparse)

k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, forum_sparse),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, forum_sparse),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") + theme_bw() +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics")
ggsave("figs/diagnostics-by-topic.png")

k_result %>%
  select(K, exclusivity, semantic_coherence) %>%
  filter(K %in% c(20, 50, 60, 80, 100)) %>%
  unnest(cols = c(exclusivity, semantic_coherence)) %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2, alpha = 0.7) + theme_bw() +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")
ggsave("figs/exclusivity-vs-semantics.png")

topic_model <- k_result %>% 
  filter(K == 60) %>% 
  pull(topic_model) %>% 
  .[[1]]

topic_model
#A topic model with 60 topics, 4917 documents and a 712 word dictionary.
###tidy time
td_beta <- tidy(topic_model)
td_beta

td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(forum_sparse))

td_gamma
##plot topic relevance
library(ggthemes)

top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest(cols = c(terms))

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms %>%
  top_n(20, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3,
            family = "IBMPlexSans") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.09),
                     labels = scales::percent_format()) +
  theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  theme(plot.title = element_text(size = 11,
                                  family="IBMPlexSans-Bold")) +
  labs(x = NULL, y = expression(gamma),
       title = "Top 20 topics by prevalence in the Storm Front corpus")
ggsave("figs/top-20.png", height = 8, width = 8)

gamma_terms %>%
  top_n(60, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3,
            family = "IBMPlexSans") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.09),
                     labels = scales::percent_format()) +
  theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  theme(plot.title = element_text(size = 11,
                                  family="IBMPlexSans-Bold")) +
  labs(x = NULL, y = expression(gamma),
       title = "All topics by prevalence in the Storm Front corpus")
ggsave("figs/top-all.png", height = 8, width = 8)


gamma_terms %>%
  select(topic, gamma, terms) %>%
  kable(digits = 3, 
        col.names = c("Topic", "Expected topic proportion", "Top 7 terms"))




all <- df

user_rank <- all %>%
  group_by(user) %>%
  summarise(n = n())

unigram <- all %>% 
  mutate(text_clean = str_replace_all(text_nopunct, 
                                      "[Nn]ational\\s[Ss]ocialism[A-z]*", 
                                      "ns") %>% 
           str_replace_all("[Nn]ational\\s[Ss]ocialist[A-z]*",
                           "ns") %>% 
           str_replace_all("[Jj]ew[A-z]*",
                           "jew") %>% 
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
         forum,
         id) %>%
  anti_join(stop_words)

#Add the word stemmed words. 
#unigram <- unigram %>% 
#  mutate(word_stem = wordStem(word),
#         word_stem = ifelse(word == 'baby' | word == 'mother' | word == 'pregnant',
#                            as.character(word),
#                            word_stem))

#Create a word count dataframe
unigram_count_forum <- unigram %>% 
  group_by(forum) %>%
  count(word, sort = T) %>% 
  mutate(word = reorder(word, n)) %>%  
  mutate(word_stem = wordStem(word),
         word_stem = ifelse(word == 'Hitler' | word == 'Nazi' | word == 'white',
                            word,
                            word_stem)) %>%
  subset(n > 500)

ggplot(unigram_count_forum, aes(x=n, y=word)) + 
  geom_col() + theme_bw() +
 scale_x_continuous(limit=c(0,5100),
                     breaks = c(0, 500,
                                1000,
                               1500,
                              2000, 2500, 3000, 
                              3500, 4000, 45,000, 5000),
                  expand = c(0,0)) +
  #theme(axis.text = element_text(size=5)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Unigrams Used More Than 500 Times",
       x= "Freqeuncy",
       y="",
       fill = "Frequency (High-Low)")
ggsave("figs/unigram_id.png", height=12, width = 12)



##sentiment analysis
library(qdap)
library(Rcpp)
data('Top200Words')

unigram_nrc_user<- get_sentiments('nrc') %>% 
  filter(word != "white") %>% #white is neutral in this context
  inner_join(unigram) %>% 
  group_by(user) %>% 
  add_count(sentiment, 
            sort = T) %>% 
  inner_join(user_rank)

unigram_nrc_user<- get_sentiments('nrc') %>% 
  filter(word != "white") %>% #white is neutral in this context
  inner_join(unigram) %>% 
  group_by(user) %>% 
  add_count(sentiment, 
            sort = T)

##trying
terms <- read.table(text=top_terms$terms,col.names=c('Term1', "Term2",
                                               'Term3', 'Term4', 'Term5',
                                               'Term6', 'Term7'))
terms$topic <- 1:nrow(terms)

t1 <- terms %>%
  select(topic, Term1)
t1<- rename(t1, word = Term1)
t2 <- terms %>%
  select(topic, Term2)
t2<- rename(t2, word = Term2)
t3 <- terms %>%
  select(topic, Term3)
t3<- rename(t3, word = Term3)
t4 <- terms %>%
  select(topic, Term4)
t4<- rename(t4, word = Term4)
t5 <- terms %>%
  select(topic, Term5)
t5<- rename(t5, word = Term5)
t6 <- terms %>%
  select(topic, Term6)
t6<- rename(t6, word = Term6)
t7 <- terms %>%
  select(topic, Term7)
t7<- rename(t7, word = Term7)

words_topics <- rbind(t1, t2, t3, t4, t5, t6, t7)
words_topics$topic <- as.character(words_topics$topic)
class(words_topics$topic)

unigram_nrc_topic<- get_sentiments('nrc') %>% 
  inner_join(words_topics) %>% 
  group_by(topic) %>% 
  add_count(sentiment, 
            sort = T)

ggplot(unigram_nrc_topic, aes(x=sentiment, fill=topic)) +
  geom_bar() + theme_bw() +
  labs(title = "NRC Sentiment Across Topics",
       x = "",
       y = "Frequency",
       fill = "NRC Sentiment")
ggsave("figs/sentiment_freq_topic.png", width=10, height=10)

unigram_nrc <- get_sentiments('nrc') %>% 
  inner_join(unigram) %>% 
  add_count(sentiment, 
            sort = T)

ggplot(unigram_nrc, aes(x=sentiment, fill=forum)) +
  geom_bar() + theme_bw() +
  labs(title = "NRC Sentiment Across Posts",
       x = "",
       y = "Frequency",
       fill = "Forum")
ggsave("figs/sentiment_freq_all_words.png", width=10)

unigram_time<- get_sentiments('nrc') %>% 
  inner_join(unigram) %>% 
  group_by(date) %>% 
  add_count(sentiment, 
            sort = T)

unigram_time <- unigram_time %>%
  select(date, sentiment, forum, n)

ggplot(unigram_time, aes(x=date, y=n, fill = sentiment)) +
  geom_col() + theme_minimal() +
  facet_wrap(vars(sentiment), ncol = 5) +
  theme(axis.text.x = element_text(angle=90, vjust = 1)) +
  labs(title = "NRC Sentiment Across Posts",
       x = "",
       y = "Frequency",
       fill = "Sentiment")
ggsave("figs/sentiment_freq_time", width = 10, height = 10)
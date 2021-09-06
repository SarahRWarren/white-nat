library(janitor)
library(textreadr)
#install.packages('xml2')
#install.packages('rvest')
library(readxl)
library(tidyverse)

##tidying

##NATSOC
natsoc <- read_excel("data/natsoc_readable.xlsx") %>%
  mutate(forum = "National Socialism")

brainwash <- read_excel("data/brainwash_readable.xlsx") %>%
  mutate(forum = "How We Get Brainwashed")

conservative <- read_excel("data/conservative_readable.xlsx") %>%
  mutate(forum = "Conservatives")

wn <- read_excel("data/wn_readable.xlsx") %>%
  mutate(forum = "Positive White Nationalism")

all <- rbind(natsoc, brainwash, conservative, wn)

all <- all %>%
  mutate(id = seq_along(user))


user_rank <- all %>% 
  group_by(user, forum) %>%
  summarize(posts_forum = n()) %>%
  group_by(user) %>%
  summarize(posts = n())

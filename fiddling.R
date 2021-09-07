library(tidyverse)

wn <- read_rds("data/clean_positive_white_nationalism.Rds") %>%
  mutate(forum = "Positive White Nationalism")
conservative <- read_rds("data/clean_conservatives.Rds") %>%
  mutate(forum = "Conservatives")
ns <- read_rds("data/clean_national_socialism.Rds") %>%
  mutate(forum = "National Socialism")

ideo_philo <- rbind(wn, conservative, ns)
write_rds(ideo_philo, "data/ideo_philo.Rds")

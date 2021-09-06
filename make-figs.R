library(tidyverse)
library(viridis)

intro <- read_rds("data/clean_ladies_introduction.Rds") %>%
  mutate(forum = "Introduce Yourselves Here")
preg <- read_rds("data/clean_ladies_preg.Rds") %>%
  mutate(forum = "The Pregnant/TTC Thread")
white <- read_rds("data/clean_ladies_white.Rds") %>%
  mutate(forum = "Contribution to the White Race")

ladies <- rbind(intro, preg, white)

user_rank <- ladies %>%
  group_by(user) %>%
  summarise(n = n(), forum=forum) %>%
  subset(n > 2)


##make figs
ggplot(user_rank, aes(x=n, y=user, fill=forum)) + 
  geom_col() + theme_bw() +
  scale_fill_viridis() +
  scale_fill_viridis(discrete = TRUE) +
  theme(axis.text = element_text(size=2.5)) +
  theme(legend.position = c(0.75, 0.87)) +
  labs(title = "Posts by User",
       x= "Number of Posts",
       y="Username",
       fill = "Forum")
  
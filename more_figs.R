library(tidyverse)
library(viridis)

all <- read_rds("data/all_scraped.Rds")
 freq <- all %>%
   group_by(user) %>%
   summarise(n = n(), forum = forum) %>%
   subset(n > 100)
 
 ggplot(freq, aes(x=user, y=n, fill=forum)) + geom_col() + coord_flip() +
   theme_bw() + scale_fill_viridis(discrete = TRUE) +
   scale_y_continuous(labels = scales::comma) +
   theme(axis.text.x = element_text(angle=90, vjust = 1)) +
   theme(legend.position = c(0.6, 0.7)) +
   labs(title = "Users Who Posted 100+ Times",
        x ="",
        y="",
        fill ="Forum")
 ggsave("figs/users-posted-100-times.png")
 
 
 freq_forum <- all %>%
   group_by(forum) %>%
   summarise(n = n())
 
 ggplot(freq_forum, aes(x=n, y=forum, fill = forum)) + 
   geom_col(show.legend = FALSE) +
   theme_bw() + scale_fill_viridis(discrete = TRUE) +
   theme(axis.text.x = element_text(angle=90, vjust = 1)) +
   labs(title = "Posts Per Forum",
        x ="",
        y="",
        fill ="")
 ggsave("figs/posts-per-forum.png")

  
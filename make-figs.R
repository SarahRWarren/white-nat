library(tidyverse)
library(viridis)

#####LADIES
intro <- read_rds("data/clean_ladies_introduction.Rds") %>%
  mutate(forum = "Introduce Yourselves Here")
preg <- read_rds("data/clean_ladies_preg.Rds") %>%
  mutate(forum = "The Pregnant/TTC Thread")
white <- read_rds("data/clean_ladies_white.Rds") %>%
  mutate(forum = "Contribution to the White Race")

ladies <- rbind(intro, preg, white)
write_rds(ladies, "data/ladies.Rds")

############################
user_rank <- ladies %>%
  group_by(user) %>%
  summarise(n = n()) %>%
  subset(n > 1)

##make figs
ggplot(user_rank, aes(x=n, y=user)) + 
  geom_col() + theme_bw() +
  theme(axis.text = element_text(size=3)) +
  scale_x_continuous(limit=c(0,600),
                     breaks = c(0, 10, 20, 30, 40, 50, 100,
                                200, 300, 400, 500, 600),
                     expand = c(0,0)) +
  labs(title = "Posts by User",
       caption = "Posts by Username of Women Who Posted More than Once",
       x= "Number of Posts",
       y="Username")
ggsave("figs/user_rank_ladies.png", height=20)


####################################
user_time <- ladies %>% 
  separate(date,
           into = c('m', 'd', 'y'),
           sep = '-',
           remove = F) %>% 
  mutate(date = as.Date(ISOdate(y, m, d)),
         ym = paste0(y, "-", m)) %>% 
  group_by(user, ym) %>% 
  add_count(user, 
            name = "n_posts") %>% 
  summarise(mean_length = mean(length),
            n_posts = mean(n_posts)) %>% 
  arrange(ym) %>% 
  ungroup()

ggplot(user_time, aes(x=ym, y=mean_length, fill=n_posts)) + 
  geom_col() + theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_vline(xintercept = "2009-01", linetype="dashed", 
             color = "gray", size=1.5) +
  geom_vline(xintercept = "2013-01", linetype="dashed", 
             color = "gray", size=1.5) +
  geom_vline(xintercept = "2017-01", linetype="dotted", 
             color = "red", size=1.5) +
  geom_vline(xintercept = "2017-08", linetype="dotted", 
             color = "red", size=1.5) +
  geom_vline(xintercept = "2021-01", linetype="dashed", 
             color = "gray", size=1.5) +
  labs(title = "Posts Per Month-Year",
       x= "Year-Month",
       y="Average Length",
       fill = "Number of Posts")

ggsave("figs/ladies_length_frequency.png", width=20)



ggplot(user_time, aes(x=ym, y=n_posts, fill=mean_length)) + 
  geom_col() + theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_vline(xintercept = "2009-01", linetype="dashed", 
             color = "gray", size=1.5) +
  geom_vline(xintercept = "2013-01", linetype="dashed", 
             color = "gray", size=1.5) +
  geom_vline(xintercept = "2017-01", linetype="dotted", 
             color = "red", size=1.5) +
  geom_vline(xintercept = "2017-08", linetype="dotted", 
             color = "red", size=1.5) +
  geom_vline(xintercept = "2021-01", linetype="dashed", 
             color = "gray", size=1.5) +
  labs(title = "Posts Per Month-Year",
       x= "Year-Month",
       y="Number of Posts",
       fill = "Average Length")

ggsave("figs/ladies_frequency_length.png", width=20)



########ideology
ip <- read_rds("data/ideo_philo.Rds")

user_rank <- ip %>%
  group_by(user) %>%
  summarise(n = n()) %>%
  subset(n > 1)

##make figs
ggplot(user_rank, aes(x=n, y=user)) + 
  geom_col() + theme_bw() +
  theme(axis.text = element_text(size=3)) +
  scale_x_continuous(limit=c(0,450),
                     breaks = c(0, 10, 20, 30, 40, 50, 100, 150, 200,
                                250, 300, 350, 400, 450),
                     expand = c(0,0)) +
  labs(title = "Posts by User",
       caption = "Posts by Username of Those Who Posted More than Once",
       x= "Number of Posts",
       y="Username")
ggsave("figs/user_rank_ip.png", height=20)


####################################
user_time <- ip %>% 
  separate(date,
           into = c('m', 'd', 'y'),
           sep = '-',
           remove = F) %>% 
  mutate(date = as.Date(ISOdate(y, m, d)),
         ym = paste0(y, "-", m)) %>% 
  group_by(user, ym) %>% 
  add_count(user, 
            name = "n_posts") %>% 
  summarise(mean_length = mean(length),
            n_posts = mean(n_posts)) %>% 
  arrange(ym) %>% 
  ungroup()

ggplot(user_time, aes(x=ym, y=mean_length, fill=n_posts)) + 
  geom_col() + theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_vline(xintercept = "2009-01", linetype="dashed", 
             color = "gray", size=1.5) +
  geom_vline(xintercept = "2013-01", linetype="dashed", 
             color = "gray", size=1.5) +
  geom_vline(xintercept = "2016-12", linetype="dotted", 
             color = "red", size=1.5) +
  geom_vline(xintercept = "2017-08", linetype="dotted", 
             color = "red", size=1.5) +
  geom_vline(xintercept = "2021-01", linetype="dashed", 
             color = "gray", size=1.5) +
  labs(title = "Posts Per Month-Year",
       x= "Year-Month",
       y="Average Length",
       fill = "Number of Posts")

ggsave("figs/ip_length_frequency.png", width=20)

ggplot(user_time, aes(x=ym, y=n_posts, fill=mean_length)) + 
  geom_col() + theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_vline(xintercept = "2009-01", linetype="dashed", 
             color = "gray", size=1.5) +
  geom_vline(xintercept = "2013-01", linetype="dashed", 
             color = "gray", size=1.5) +
  geom_vline(xintercept = "2016-12", linetype="dotted", 
             color = "red", size=1.5) +
  geom_vline(xintercept = "2017-08", linetype="dotted", 
             color = "red", size=1.5) +
  geom_vline(xintercept = "2021-01", linetype="dashed", 
             color = "gray", size=1.5) +
  labs(title = "Posts Per Month-Year",
       x= "Year-Month",
       y="Number of Posts",
       fill = "Average Length")

ggsave("figs/ip_frequency_length.png", width=20)


########## all together now
all <- rbind(ladies, ip)
write_rds(all, "data/all_scraped.Rds")

user_rank <- all %>%
  group_by(user) %>%
  summarise(n = n()) %>%
  subset(n > 1)

##make figs
ggplot(user_rank, aes(x=n, y=user)) + 
  geom_col() + theme_bw() +
  theme(axis.text = element_text(size=2.5)) +
  scale_x_continuous(limit=c(0,600),
                     breaks = c(0, 10, 20, 30, 40, 50, 100, 150, 200,
                                250, 300, 350, 400, 450, 500, 550, 600),
                     expand = c(0,0)) +
  labs(title = "Posts by User",
       caption = "In All Scraped Forums",
       x= "Number of Posts",
       y="Username")
ggsave("figs/user_rank_all.png", height=20)


####################################
user_time <- all %>% 
  separate(date,
           into = c('m', 'd', 'y'),
           sep = '-',
           remove = F) %>% 
  mutate(date = as.Date(ISOdate(y, m, d)),
         ym = paste0(y, "-", m)) %>% 
  group_by(user, ym) %>% 
  add_count(user, 
            name = "n_posts") %>% 
  summarise(mean_length = mean(length),
            n_posts = mean(n_posts)) %>% 
  arrange(ym) %>% 
  ungroup()

ggplot(user_time, aes(x=ym, y=mean_length, fill=n_posts)) + 
  geom_col() + theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_vline(xintercept = "2009-01", linetype="dashed", 
             color = "gray", size=1.5) +
  geom_vline(xintercept = "2013-01", linetype="dashed", 
             color = "gray", size=1.5) +
  geom_vline(xintercept = "2017-01", linetype="dotted", 
             color = "red", size=1.5) +
  geom_vline(xintercept = "2017-08", linetype="dotted", 
             color = "red", size=1.5) +
  geom_vline(xintercept = "2021-01", linetype="dashed", 
             color = "gray", size=1.5) +
  labs(title = "Posts Per Month-Year",
       x= "Year-Month",
       y="Average Length",
       fill = "Number of Posts")
ggsave("figs/all_length_frequency.png", width=20, height = 15)

ggplot(user_time, aes(x=ym, y=n_posts, fill=mean_length)) + 
  geom_col() + theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_vline(xintercept = "2009-01", linetype="dashed", 
             color = "gray", size=1.5) +
  geom_vline(xintercept = "2013-01", linetype="dashed", 
             color = "gray", size=1.5) +
  geom_vline(xintercept = "2017-01", linetype="dotted", 
             color = "red", size=1.5) +
  geom_vline(xintercept = "2017-08", linetype="dotted", 
             color = "red", size=1.5) +
  geom_vline(xintercept = "2021-01", linetype="dashed", 
             color = "gray", size=1.5) +
  labs(title = "Posts Per Month-Year",
       x= "Year-Month",
       y="Number of Posts",
       fill = "Average Length")
ggsave("figs/all_frequency_length.png", width=20, height = 15)

all_ym <- all %>% 
  separate(date,
           into = c('m', 'd', 'y'),
           sep = '-',
           remove = F) %>% 
  mutate(date = as.Date(ISOdate(y, m, d)),
         ym = paste0(y, "-", m))

ggplot(all_ym, aes(x=ym, fill=forum)) +
  geom_bar(alpha=0.5) +
  theme_bw() + 
  theme(strip.background = element_blank(),
    strip.text.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_vline(xintercept = "2009-01", linetype="dashed", 
             color = "black", size=1) +
  geom_vline(xintercept = "2013-01", linetype="dashed", 
             color = "black", size=1) +
  geom_vline(xintercept = "2017-01", linetype="dotted", 
             color = "black", size=1) +
  geom_vline(xintercept = "2017-08", linetype="dotted", 
             color = "black", size=1) +
  geom_vline(xintercept = "2021-01", linetype="dashed", 
             color = "black", size=1) +
  labs(title = "Posts Over Time by Forum",
       x= "Year-Month",
       y="Number of Posts",
       fill = "Forum")
ggsave("figs/all_frequency_by_forum.png", width=25, height = 15)


ggplot(all_ym, aes(x=ym, fill=forum)) +
  geom_bar(alpha=0.5) +
  theme_bw() + 
  facet_grid(vars(forum)) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_vline(xintercept = "2009-01", linetype="dashed", 
             color = "black", size=1) +
  geom_vline(xintercept = "2013-01", linetype="dashed", 
             color = "black", size=1) +
  geom_vline(xintercept = "2017-01", linetype="dotted", 
             color = "black", size=1) +
  geom_vline(xintercept = "2017-08", linetype="dotted", 
             color = "black", size=1) +
  geom_vline(xintercept = "2021-01", linetype="dotted", 
             color = "black", size=1) +
  labs(title = "Posts Over Time by Forum",
       x= "Year-Month",
       y="Number of Posts",
       fill = "Forum")
ggsave("figs/all_frequency_wrap_forum.png", width=25, height = 15)
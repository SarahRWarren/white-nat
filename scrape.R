library(janitor)
library(textreadr)
#install.packages('xml2')
#install.packages('rvest')
library(xml2)
library(rvest)
library(tidyverse)

##IDEOLOGY AND PHILOSOPHY FORUM

#generate a url for each page of the National Socialism thread
ideo_philo_urls <- c("https://www.stormfront.org/forum/t451603/")

#generate a url for each page 
for(i in 2:509){
  ideo_philo_urls <- c(ideo_philo_urls, paste0("https://www.stormfront.org/forum/t451603-", 
                                               i,
                                               "/"))
}

ideology_forum <- data.frame(user = c(),
                             date = c(),
                             time = c(),
                             text = c())

for(i in 1:length(ideo_philo_urls)){
  page <- read_html(url(ideo_philo_urls[i]))
  
  #read the text from the posts 
  page_text_prelim <- page %>% 
    html_nodes("#posts .alt1") %>% 
    html_text()
  
  #extract the text from the posts. 
  #Every other index in this vector is the post
  #with the remaining indices being missing. 
  page_text <- page_text_prelim[seq(1, 20, 2)]
  
  
  page_date_time <- page %>% 
    html_nodes("#posts .thead:nth-child(1)") %>% 
    html_text() 
  
  page_date_time_prelim <- page_date_time %>% 
    data.frame() %>% 
    janitor::clean_names() %>% 
    mutate(date = stringr::str_extract(x, 
                                       "\\d{2}\\-\\d{2}\\-\\d{4}"),
           time = stringr::str_extract(x, 
                                       "\\d{2}\\:\\d{2}\\s[A-Z]{2}")) %>% 
    filter(!is.na(date)) %>%  
    select(date,
           time)
  
  page_date <- as.vector(page_date_time_prelim$date)
  page_time <- as.vector(page_date_time_prelim$time)
  
  page_user_prelim <- page %>% 
    html_nodes("#posts .alt2") %>% 
    html_text() %>% 
    data.frame() %>% 
    janitor::clean_names() %>% 
    mutate(text = as.character(x),
           user_time_detect = as.numeric(stringr::str_detect(text,
                                                             "Posts:")),
           user = stringr::str_extract(text,
                                       "([A-z0-9]+.)+")) %>% 
    filter(user_time_detect == 1) %>% 
    select(user)
  
  page_user <- as.vector(page_user_prelim$user)
  
  if(i < 509){
    
    page_df <- data.frame(user = as.character(page_user),
                          date = as.character(page_date),
                          time = as.character(page_time), 
                          text = as.character(page_text))
    
    
    ideology_forum <- rbind(ideology_forum, page_df)
  }
}

#This deals with that last loop that failed 

page_text <- as.vector(na.omit(page_text))
page_df <- data.frame(user = as.character(page_user),
                      date = as.character(page_date),
                      time = as.character(page_time), 
                      text = as.character(page_text))


ideology_forum <- rbind(ideology_forum, page_df)


##
#generate a url for each page of the Positive WN thread
pos_wn_urls <- c("https://www.stormfront.org/forum/t964243/")

#generate a url for each page 
for(i in 2:22){
  pos_wn_urls <- c(pos_wn_urls, paste0("https://www.stormfront.org/forum/t964243-", 
                                               i,
                                               "/"))
}

pos_wn_forum <- data.frame(user = c(),
                             date = c(),
                             time = c(),
                             text = c())

for(i in 1:length(pos_wn_urls)){
  page <- read_html(url(pos_wn_urls[i]))
  
  #read the text from the posts 
  page_text_prelim <- page %>% 
    html_nodes("#posts .alt1") %>% 
    html_text()
  
  #extract the text from the posts. 
  #Every other index in this vector is the post
  #with the remaining indices being missing. 
  page_text <- page_text_prelim[seq(1, 20, 2)]
  
  
  page_date_time <- page %>% 
    html_nodes("#posts .thead:nth-child(1)") %>% 
    html_text() 
  
  page_date_time_prelim <- page_date_time %>% 
    data.frame() %>% 
    janitor::clean_names() %>% 
    mutate(date = stringr::str_extract(x, 
                                       "\\d{2}\\-\\d{2}\\-\\d{4}"),
           time = stringr::str_extract(x, 
                                       "\\d{2}\\:\\d{2}\\s[A-Z]{2}")) %>% 
    filter(!is.na(date)) %>%  
    select(date,
           time)
  
  page_date <- as.vector(page_date_time_prelim$date)
  page_time <- as.vector(page_date_time_prelim$time)
  
  page_user_prelim <- page %>% 
    html_nodes("#posts .alt2") %>% 
    html_text() %>% 
    data.frame() %>% 
    janitor::clean_names() %>% 
    mutate(text = as.character(x),
           user_time_detect = as.numeric(stringr::str_detect(text,
                                                             "Posts:")),
           user = stringr::str_extract(text,
                                       "([A-z0-9]+.)+")) %>% 
    filter(user_time_detect == 1) %>% 
    select(user)
  
  page_user <- as.vector(page_user_prelim$user)
  
  if(i < 22){
    
    page_df <- data.frame(user = as.character(page_user),
                          date = as.character(page_date),
                          time = as.character(page_time), 
                          text = as.character(page_text))
    
    
    pos_wn_forum <- rbind(pos_wn_forum, page_df)
  }
}

pos_wn_forum <- rbind(pos_wn_forum, page_df)

##CONSERVATIVES
#generate a url for each page of the Positive WN thread
conservative_urls <- c("https://www.stormfront.org/forum/t1337471/")

#generate a url for each page 
for(i in 2:13){
  conservative_urls <- c(conservative_urls, paste0("https://www.stormfront.org/forum/t1337471-", 
                                       i,
                                       "/"))
}

conservative_forum <- data.frame(user = c(),
                           date = c(),
                           time = c(),
                           text = c())

for(i in 1:length(conservative_urls)){
  page <- read_html(url(conservative_urls[i]))
  
  #read the text from the posts 
  page_text_prelim <- page %>% 
    html_nodes("#posts .alt1") %>% 
    html_text()
  
  #extract the text from the posts. 
  #Every other index in this vector is the post
  #with the remaining indices being missing. 
  page_text <- page_text_prelim[seq(1, 20, 2)]
  
  
  page_date_time <- page %>% 
    html_nodes("#posts .thead:nth-child(1)") %>% 
    html_text() 
  
  page_date_time_prelim <- page_date_time %>% 
    data.frame() %>% 
    janitor::clean_names() %>% 
    mutate(date = stringr::str_extract(x, 
                                       "\\d{2}\\-\\d{2}\\-\\d{4}"),
           time = stringr::str_extract(x, 
                                       "\\d{2}\\:\\d{2}\\s[A-Z]{2}")) %>% 
    filter(!is.na(date)) %>%  
    select(date,
           time)
  
  page_date <- as.vector(page_date_time_prelim$date)
  page_time <- as.vector(page_date_time_prelim$time)
  
  page_user_prelim <- page %>% 
    html_nodes("#posts .alt2") %>% 
    html_text() %>% 
    data.frame() %>% 
    janitor::clean_names() %>% 
    mutate(text = as.character(x),
           user_time_detect = as.numeric(stringr::str_detect(text,
                                                             "Posts:")),
           user = stringr::str_extract(text,
                                       "([A-z0-9]+.)+")) %>% 
    filter(user_time_detect == 1) %>% 
    select(user)
  
  page_user <- as.vector(page_user_prelim$user)
  
  if(i < 13){
    
    page_df <- data.frame(user = as.character(page_user),
                          date = as.character(page_date),
                          time = as.character(page_time), 
                          text = as.character(page_text))
    
    
    conservative_forum <- rbind(conservative_forum, page_df)
  }
}

conservative_forum <- rbind(conservative_forum, page_df)







#BRAINWASH
#generate a url for each page of the Positive WN thread
brainwash_urls <- c("https://www.stormfront.org/forum/t432084/")

#generate a url for each page 
for(i in 2:103){
  brainwash_urls <- c(brainwash_urls, paste0("https://www.stormfront.org/forum/t432084-", 
                                                   i,
                                                   "/"))
}

brainwash_forum <- data.frame(user = c(),
                                 date = c(),
                                 time = c(),
                                 text = c())

for(i in 1:length(brainwash_urls)){
  page <- read_html(url(brainwash_urls[i]))
  
  #read the text from the posts 
  page_text_prelim <- page %>% 
    html_nodes("#posts .alt1") %>% 
    html_text()
  
  #extract the text from the posts. 
  #Every other index in this vector is the post
  #with the remaining indices being missing. 
  page_text <- page_text_prelim[seq(1, 20, 2)]
  
  
  page_date_time <- page %>% 
    html_nodes("#posts .thead:nth-child(1)") %>% 
    html_text() 
  
  page_date_time_prelim <- page_date_time %>% 
    data.frame() %>% 
    janitor::clean_names() %>% 
    mutate(date = stringr::str_extract(x, 
                                       "\\d{2}\\-\\d{2}\\-\\d{4}"),
           time = stringr::str_extract(x, 
                                       "\\d{2}\\:\\d{2}\\s[A-Z]{2}")) %>% 
    filter(!is.na(date)) %>%  
    select(date,
           time)
  
  page_date <- as.vector(page_date_time_prelim$date)
  page_time <- as.vector(page_date_time_prelim$time)
  
  page_user_prelim <- page %>% 
    html_nodes("#posts .alt2") %>% 
    html_text() %>% 
    data.frame() %>% 
    janitor::clean_names() %>% 
    mutate(text = as.character(x),
           user_time_detect = as.numeric(stringr::str_detect(text,
                                                             "Posts:")),
           user = stringr::str_extract(text,
                                       "([A-z0-9]+.)+")) %>% 
    filter(user_time_detect == 1) %>% 
    select(user)
  
  page_user <- as.vector(page_user_prelim$user)
  
  if(i < 103){
    
    page_df <- data.frame(user = as.character(page_user),
                          date = as.character(page_date),
                          time = as.character(page_time), 
                          text = as.character(page_text))
    
    
    brainwash_forum <- rbind(brainwash_forum, page_df)
  }
}

brainwash_forum <- rbind(brainwash_forum, page_df)

write_rds(ideology_forum, "data/unclean_natsoc.Rds")
write_rds(brainwash_forum, "data/unclean_brainwash.Rds")
write_rds(conservative_forum, "data/unclean_conservative.Rds")
write_rds(pos_wn_forum, "data/unclean_pos_wn.Rds")
library(janitor)
library(textreadr)
#install.packages('xml2')
#install.packages('rvest')
library(xml2)
library(rvest)
library(tidyverse)
###LADIES
#generate a url for each page of the National Socialism thread

ladies_urls <- c("https://www.stormfront.org/forum/t528601/")

#generate a url for each page 
for(i in 2:48){
  ladies_urls <- c(ideo_philo_urls, paste0("https://www.stormfront.org/forum/t528601-", 
                                           i,
                                           "/"))
}

ladies_forum <- data.frame(user = c(),
                           date = c(),
                           time = c(),
                           text = c())

for(i in 1:length(ladies_urls)){
  page <- read_html(url(ladies_urls[i]))
  
  #read the text from the posts 
  page_text_prelim <- page %>% 
    html_nodes("#posts .alt1") %>% 
    html_text()
  
  #extract the text from the posts. 
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
  
  if(i < 48){
    
    page_df <- data.frame(user = as.character(page_user),
                          date = as.character(page_date),
                          time = as.character(page_time), 
                          text = as.character(page_text))
    
    
    ladies_forum <- rbind(ladies_forum, page_df)
  }
}

#This deals with that last loop that failed 

page_text <- as.vector(na.omit(page_text))
page_df <- data.frame(user = as.character(page_user),
                      date = as.character(page_date),
                      time = as.character(page_time), 
                      text = as.character(page_text))


ladies_forum <- rbind(ladies_forum, page_df)




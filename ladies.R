library(janitor)
library(textreadr)
#install.packages('xml2')
#install.packages('rvest')
library(xml2)
library(rvest)
library(tidyverse)

##Notes: If you want to use this code, 
#clear your cookies between runs; 
#solves the error "error in open.connection(x, "rb") : 
#cannot open the connection scraping"


###LADIES

ladies_urls <- c("https://www.stormfront.org/forum/t437198/")

#generate a url for each page 
for(i in 2:171){
  ladies_urls <- c(ladies_urls, paste0("https://www.stormfront.org/forum/t437198-", 
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
  
  if(i < 171){
    
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



#Create an ID that matches all the post numbers from the forum. This way I can spot check if necessary. 
ladies_forum <- ladies_forum %>% 
  mutate(id = seq_along(user))

clean_ladies <- ladies_forum %>% 
  mutate(text_nore = stringr::str_replace_all(text, 
                                              "Re: Ladies, Post Your Introductions Here",
                                              ""),
         text_noquote = stringr::str_replace_all(text_nore, 
                                                 "Quote.(\\n)*.*(\\n)*((.*)|(\\n*))*\\n{2}",
                                                 ""),
         text_nobreak = stringr::str_replace_all(text_noquote,
                                                 "\\c*",
                                                 ""), 
         text_nopunct = stringr::str_replace_all(text_nobreak,
                                                 "[[:punct:]]*",
                                                 ""),
         length = str_count(text_nopunct, 
                            "\\w+"))

write_rds(clean_ladies, "data/clean_ladies_introduction.Rds")


##pregnancy thread

preg_urls <- c("https://www.stormfront.org/forum/t797093/")

#generate a url for each page 
for(i in 2:356){
  preg_urls <- c(preg_urls, paste0("https://www.stormfront.org/forum/t797093-", 
                                       i,
                                       "/"))
}

preg_forum <- data.frame(user = c(),
                           date = c(),
                           time = c(),
                           text = c())

for(i in 1:length(preg_urls)){
  page <- read_html(url(preg_urls[i]))
  
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
  
  if(i < 356){
    
    page_df <- data.frame(user = as.character(page_user),
                          date = as.character(page_date),
                          time = as.character(page_time), 
                          text = as.character(page_text))
    
    
    preg_forum <- rbind(preg_forum, page_df)
  }
}

#This deals with that last loop that failed 

page_text <- as.vector(na.omit(page_text))
page_df <- data.frame(user = as.character(page_user),
                      date = as.character(page_date),
                      time = as.character(page_time), 
                      text = as.character(page_text))


preg_forum <- rbind(preg_forum, page_df)


#Create an ID that matches all the post numbers from the forum. This way I can spot check if necessary. 
preg_forum <- preg_forum %>% 
  mutate(id = seq_along(user))

clean_preg <- preg_forum %>% 
  mutate(text_nore = stringr::str_replace_all(text, 
                                              "Re: WOMEN'S FORUM: The Pregnant/TTC thread",
                                              ""),
         text_noquote = stringr::str_replace_all(text_nore, 
                                                 "Quote.(\\n)*.*(\\n)*((.*)|(\\n*))*\\n{2}",
                                                 ""),
         text_nobreak = stringr::str_replace_all(text_noquote,
                                                 "\\c*",
                                                 ""), 
         text_nopunct = stringr::str_replace_all(text_nobreak,
                                                 "[[:punct:]]*",
                                                 ""),
         length = str_count(text_nopunct, 
                            "\\w+"))

write_rds(clean_preg, "data/clean_ladies_preg.Rds")

###contribute to white race
white_urls <- c("https://www.stormfront.org/forum/t877813/")

#generate a url for each page 
for(i in 2:17){
  white_urls <- c(white_urls, paste0("https://www.stormfront.org/forum/t877813-", 
                                   i,
                                   "/"))
}

white_forum <- data.frame(user = c(),
                         date = c(),
                         time = c(),
                         text = c())

for(i in 1:length(white_urls)){
  page <- read_html(url(white_urls[i]))
  
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
  
  if(i < 17){
    
    page_df <- data.frame(user = as.character(page_user),
                          date = as.character(page_date),
                          time = as.character(page_time), 
                          text = as.character(page_text))
    
    
    white_forum <- rbind(white_forum, page_df)
  }
}

#This deals with that last loop that failed 

page_text <- as.vector(na.omit(page_text))
page_df <- data.frame(user = as.character(page_user),
                      date = as.character(page_date),
                      time = as.character(page_time), 
                      text = as.character(page_text))


white_forum <- rbind(white_forum, page_df)


#Create an ID that matches all the post numbers from the forum. This way I can spot check if necessary. 
white_forum <- white_forum %>% 
  mutate(id = seq_along(user))

clean_white <- white_forum %>% 
  mutate(text_nore = stringr::str_replace_all(text, 
                                              "Re: Ladies, What Have You Done to Contribute the White Race?",
                                              ""),
         text_noquote = stringr::str_replace_all(text_nore, 
                                                 "Quote.(\\n)*.*(\\n)*((.*)|(\\n*))*\\n{2}",
                                                 ""),
         text_nobreak = stringr::str_replace_all(text_noquote,
                                                 "\\c*",
                                                 ""), 
         text_nopunct = stringr::str_replace_all(text_nobreak,
                                                 "[[:punct:]]*",
                                                 ""),
         length = str_count(text_nopunct, 
                            "\\w+"))

write_rds(clean_white, "data/clean_ladies_white.Rds")

write.csv(ladies, "data/ladies.csv")
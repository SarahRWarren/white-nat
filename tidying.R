library(tidyverse)

brainwash <- read_rds("data/saved/unclean_brainwash.Rds") %>%
  mutate(forum = "How We Get Brainwashed")
natsoc <- read_rds("data/saved/unclean_natsoc.Rds") %>%
  mutate(forum = "National Socialism")
poswn <- read_rds("data/saved/unclean_pos_wn.Rds") %>%
  mutate(forum = "Positive White Nationalism")
conservative <- read_rds("data/saved/unclean_conservative.Rds") %>%
  mutate(forum = "Conservatives")


##CLEAN
conservative <- conservative %>% 
  mutate(id = seq_along(user))

clean_con <- conservative %>% 
  mutate(text_nore = stringr::str_replace_all(text, 
                                              "Re: Conservatives",
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
write_rds(clean_con, "data/clean_conservatives.Rds")

natsoc <- natsoc %>% 
  mutate(id = seq_along(user))

natsoc_fix <- natsoc[-c(3294, 3481, 3552, 4102, 4308, 4434, 4908, 5015),]


clean_ns <- natsoc_fix %>% 
  mutate(text_nore = stringr::str_replace_all(text, 
                                              "Re: National Socialism",
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
write_rds(clean_ns, "data/clean_national_socialism.Rds")


pos_wn <- poswn %>% 
  mutate(id = seq_along(user))

clean_wn <- pos_wn %>% 
  mutate(text_nore = stringr::str_replace_all(text, 
                                              "Re: The way forward: Positive White Nationalism",
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
write_rds(clean_wn, "data/clean_positive_white_nationalism.Rds")

brainwash <- brainwash %>% 
  mutate(id = seq_along(user))
write_rds(brainwash, "data/broken_brainwash.Rds")
















#####breaking - DO NOT RUN
brainwash <- brainwash %>% 
  mutate(id = seq_along(user))

clean_brain <- brainwash %>% 
  mutate(text_nore = stringr::str_replace_all(text, 
                                              "Re: How We Get Brainwashed!",
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
write_rds(clean_brain, "data/clean_brainwash.Rds")


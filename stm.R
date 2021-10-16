library(stm)
library(tidyverse)

df <- read_rds("data/ideo_philo.Rds")
write.csv(df, "data/ideo_philo.csv")
all <- read_csv("data/ideo_philo.csv")

for_analysis <- all %>%
  select(user, date, time, text, length, forum, id) %>%
  rename(documents = text)

processed <- textProcessor(for_analysis$documents, metadata = for_analysis)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta,
                     lower.thresh = 15)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta
#plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))

###effect of forum and date
forum_date <- stm(out$documents, out$vocab, K = 20,
                       prevalence =~ forum + date, content =~ forum,
                       max.em.its = 75, data = out$meta, init.type = "Spectral")

windows()
plot(forum_date, type = "summary", xlim = c(0, .3))

#####by date only
by_date <- stm(out$documents, out$vocab, K = 20,
                  prevalence =~ date, content =~ date,
                  max.em.its = 75, data = out$meta, init.type = "Spectral")

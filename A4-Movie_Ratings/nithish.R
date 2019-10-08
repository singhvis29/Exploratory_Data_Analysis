library(tidyverse)
titleRatings = read_tsv("title.ratings.tsv", na = "\\N", quote = '')
titleBasics = read_tsv("title.basics.tsv", na = "\\N", quote = '')



RatingsWithBasics = titleBasics %>% inner_join(titleRatings) %>% 
  filter(titleType == "movie")

rm(titleBasics,titleRatings)


linearModel = lm(averageRating~ startYear+runtimeMinutes,data =RatingsWithBasics )

linearModel %>% summary()


ggplot(RatingsWithBasics)+geom_histogram(aes(x = runtimeMinutes))



transformed = RatingsWithBasics %>% 
  select(startYear,runtimeMinutes,numVotes,averageRating) %>% 
  mutate(startYear = as.integer(startYear)) %>% 
  mutate(startYear = startYear-min(startYear,na.rm = TRUE)) %>% 
  # mutate_all(log) %>%
  mutate_at(vars( -averageRating), funs(log(.))) %>%
  drop_na() %>% 
  filter_all(all_vars(is.finite(.)))




lm(averageRating~ startYear+runtimeMinutes,data = transformed) %>% summary()






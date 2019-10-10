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
  mutate_at(vars( -c(averageRating,numVotes)), funs(log(.))) %>%
  drop_na() %>% 
  filter_all(all_vars(is.finite(.)))



selectedVars = RatingsWithBasics %>% 
  select(startYear,runtimeMinutes,numVotes,averageRating) %>% 
  filter(runtimeMinutes>0) %>% 
  mutate(startYear = as.integer(startYear)) %>% 
  mutate(cutYears = cut(startYear,5,dig.lab = 4))%>% 
  drop_na()

outliers  =  boxplot(selectedVars$runtimeMinutes, plot=FALSE)$out

selectedVars = selectedVars[-which(selectedVars$runtimeMinutes %in% outliers),]





selectedVars = selectedVars %>% 
              mutate(cutRuntimeMinutes = cut(runtimeMinutes,
                        breaks = c(0,81,90,100,135),
                        dig.lab = 6))
summary(selectedVars)


sum(is.na(selectedVars$averageRating))


lm(averageRating~ startYear+runtimeMinutes,data = transformed) %>%
  summary()




#### Question 2


class(selectedVars$runtimeMinutes)

ggplot(selectedVars, aes(x = runtimeMinutes, y = averageRating)) + 
  geom_point() + 
  facet_grid(rows = vars(cutYears)) +
  geom_smooth(method = "lm", se = FALSE)



ggplot(selectedVars, aes(x = startYear, y = averageRating)) + 
  geom_point() + 
  facet_grid(rows = vars(cutRuntimeMinutes)) +
  geom_smooth(method = "lm", se = FALSE)+scale_x_log10()












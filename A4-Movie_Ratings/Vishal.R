library(dplyr)
library(tidyverse)
library(mgcv)
library(broom)


basics = read_tsv("title.basics.tsv/data.tsv", na = "\\N", quote = '')
ratings = read_tsv("title.ratings.tsv/data.tsv", na = "\\N", quote = '')

#rm(basics)
#rm(ratings)

title_ratings = basics %>% inner_join(ratings, by = 'tconst')

title_ratings = title_ratings %>%
  filter(titleType == "movie") %>%
  select(startYear, runtimeMinutes, numVotes, averageRating) %>%
  mutate(as.integer(startYear)) %>% 
  mutate(yearsinceStart = startYear-min(startYear,na.rm = TRUE)) %>% 
  drop_na()

title_ratings$log_runtimeMinutes = log10(title_ratings$runtimeMinutes)
title_ratings$sqrt_runtimeMinutes = sqrt(title_ratings$runtimeMinutes)

title_ratings$log_yearsinceStart = log10(title_ratings$yearsinceStart)
title_ratings$sqrt_yearsinceStart = sqrt(title_ratings$yearsinceStart)

title_ratings = title_ratings %>% 
  filter_all(all_vars(is.finite(.)))

lm_model_nt = lm(averageRating ~ runtimeMinutes + yearsinceStart, data = title_ratings)
print(summary(lm_model_nt))

lm_model_log = lm(averageRating ~ log_runtimeMinutes + log_yearsinceStart, data = title_ratings)
print(summary(lm_model_log))

lm_model_sqrt = lm(averageRating ~ sqrt_runtimeMinutes + sqrt_yearsinceStart, data = title_ratings)
print(summary(lm_model_sqrt))

##
ggplot(title_ratings, aes(x = runtimeMinutes, y = averageRating)) + geom_point() + geom_smooth()

##
title_ratings_lm = lm(averageRating ~ runtimeMinutes + yearsinceStart, data = title_ratings)
title_ratings_lm.df = augment(title_ratings_lm)

##
ggplot(title_ratings_lm.df, aes(x = runtimeMinutes, y = .resid)) + geom_point() + geom_smooth(se = FALSE)
ggplot(title_ratings_lm.df, aes(x = yearsinceStart, y = .resid)) + geom_point() + geom_smooth(se = FALSE)
ggplot(title_ratings_lm.df, aes(x = runtimeMinutes, y = .resid)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + facet_wrap(~cut_number(yearsinceStart, n = 4))
ggplot(title_ratings_lm.df, aes(x = yearsinceStart, y = .resid)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + facet_wrap(~cut_number(runtimeMinutes, n = 4))
ggplot(title_ratings_lm.df, aes(x = .fitted, y = abs(.resid))) + geom_point() + geom_smooth(method = "lm", se = FALSE)

##GAM
title_ratings.gam = gam(averageRating ~ s(runtimeMinutes) + s(yearsinceStart), data = title_ratings)
plot(title_ratings.gam)

summary(title_ratings.gam)

ggplot()


##without outliers
outliers  =  boxplot(title_ratings$runtimeMinutes, plot=FALSE)$out
title_ratings_wo_outliers = title_ratings[-which(title_ratings$runtimeMinutes %in% outliers),]

##
ggplot(title_ratings_wo_outliers, aes(x = runtimeMinutes, y = averageRating)) + geom_point() + geom_smooth()
 
##
title_ratings_lm_wo_outliers = lm(averageRating ~ runtimeMinutes + yearsinceStart, data = title_ratings_wo_outliers)
title_ratings_lm_wo_outliers.df = augment(title_ratings_lm_wo_outliers)

##
ggplot(title_ratings_lm_wo_outliers.df, aes(x = runtimeMinutes, y = .resid)) + geom_point() + geom_smooth(se = FALSE)
ggplot(title_ratings_lm_wo_outliers.df, aes(x = yearsinceStart, y = .resid)) + geom_point() + geom_smooth(se = FALSE)
ggplot(title_ratings_lm_wo_outliers.df, aes(x = runtimeMinutes, y = .resid)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + facet_wrap(~cut_number(yearsinceStart, n = 4))
ggplot(title_ratings_lm_wo_outliers.df, aes(x = yearsinceStart, y = .resid)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + facet_wrap(~cut_number(runtimeMinutes, n = 4))
ggplot(title_ratings_lm_wo_outliers.df, aes(x = .fitted, y = abs(.resid))) + geom_point() + geom_smooth(method = "lm", se = FALSE)

##GAM - without outliers
title_ratings_wo_outliers.gam = gam(averageRating ~ s(runtimeMinutes) + s(yearsinceStart), data = title_ratings_wo_outliers)
plot(title_ratings_wo_outliers.gam)

summary(title_ratings_wo_outliers.gam)

##weighted by number of weights
title_ratings_wo_outliers = title_ratings_wo_outliers %>% mutate(weighted_averageRating = (averageRating * numVotes))

##
title_ratings_wo_outliers.gam2 = gam(weighted_averageRating ~ s(runtimeMinutes) + s(yearsinceStart), data = title_ratings_wo_outliers)
plot(title_ratings_wo_outliers.gam2)

summary(title_ratings_wo_outliers.gam2)



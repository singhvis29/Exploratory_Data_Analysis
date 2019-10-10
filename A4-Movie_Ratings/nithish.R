library(tidyverse)
titleRatings = read_tsv("title.ratings.tsv", na = "\\N", quote = '')
titleBasics = read_tsv("title.basics.tsv", na = "\\N", quote = '')



RatingsWithBasics = titleBasics %>% inner_join(titleRatings) %>% 
  filter(titleType == "movie")

rm(titleBasics,titleRatings)




### without any transoformations
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
  mutate(zeroedStartYear = startYear-min(startYear,na.rm = TRUE)) %>% 
  
  drop_na()


## removing outliers in runtimeMinutes
outliers  =  boxplot(selectedVars$runtimeMinutes, plot=FALSE)$out

selectedVars = selectedVars[-which(selectedVars$runtimeMinutes %in% outliers),]



selectedVars = selectedVars %>% 
              mutate(cutRuntimeMinutes = cut(runtimeMinutes,
                        breaks = c(0,81,90,100,135),
                        dig.lab = 6))
summary(selectedVars)


sum(is.na(selectedVars$averageRating))



### linear model fitted on transformed
lm(averageRating~ startYear+runtimeMinutes,data = transformed) %>%
  summary()




#### Question 2
library(broom)
library(mgcv)

modelGAM = gam(averageRating ~ s(zeroedStartYear)+s(runtimeMinutes),data = selectedVars)
summary(modelGAM)




predModelGAM = augment(modelGAM) %>% 
  mutate(cutRuntimeMinutes = cut(runtimeMinutes,
                                breaks = c(0,81,90,100,135),
                                          dig.lab = 6)) %>% 
  mutate(cutYears = cut(startYear,5,dig.lab = 4))



##conditioned on years
ggplot(selectedVars, aes(x = runtimeMinutes, y = averageRating)) + 
  geom_point(size = 0.05) + 
  facet_grid(rows = vars(cutYears)) +
  geom_smooth(method = "gam", se = FALSE,span = 0.5)


### conditioned on runtime
ggplot(selectedVars, aes(x = zeroedStartYear, y = averageRating)) + 
  geom_point() + 
  facet_grid(rows = vars(cutRuntimeMinutes)) +
  geom_smooth(method = "gam", se = FALSE)






#### Question 3 ####

library(lattice)




gridForContour = expand.grid(zeroedStartYear = seq(0,125,1),
                             runtimeMinutes = seq(50,133,1))
  
  
contourGridPredictions = predict(modelGAM,newdata = gridForContour)

contourDf = data.frame(gridForContour,fit = as.vector(contourGridPredictions))


ggplot(contourDf, aes(x = zeroedStartYear, y = runtimeMinutes, z = fit)) + 
  geom_raster(aes(fill=fit)) + 
  scale_fill_distiller(palette = "RdBu") +
  geom_contour() + coord_fixed()



wireframe(averageRating ~ startYear + runtimeMinutes,
          data = selectedVars, drape = TRUE)




library(gapminder)
library(tidyverse)

gapminderDf = gapminder


##load data

lifeExpectancyDf  = read.csv('life_expectancy_years.csv')
gdpPerCapitaDf = read.csv('income_per_person_with_projections.csv' )


## getinng continent mapping from gapminder df


countryContinentMapping = gapminderDf %>% 
  select(country,continent) %>% 
  distinct(country,continent,.keep_all = TRUE)


expectancy_gdp = lifeExpectancyDf %>% 
  inner_join(gdpPerCapitaDf,by = c("country" = "country"),suffix =  c("_exp", "_gdp"))




##joining continent information
expectancy_gdp_cont = expectancy_gdp %>% 
  inner_join(countryContinentMapping,by = c("country" = "country"))



expectancy_gdp_cont = expectancy_gdp_cont %>% 
  select(country,continent,everything())



Exp_GDP_Cont_2018 = expectancy_gdp_cont %>% 
  select(country,continent,X2018_exp,X2018_gdp)
  # group_by(continent) %>% 
  # summarise(meanExpect_2018 = mean(X2018_exp,na.rm=TRUE),meanGDP_2018 = mean(X2018_gdp,na.rm=TRUE))

ggplot(Exp_GDP_Cont_2018, aes(x = X2018_exp, y = X2018_gdp)) + 
  
  geom_point(alpha = 0.3) +geom_smooth(method = "lm", se = FALSE)+ facet_grid(rows = "continent")






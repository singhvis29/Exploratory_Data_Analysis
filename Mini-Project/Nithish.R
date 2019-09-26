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


joinedDf_expectancy = inner_join(lifeExpectancyDf,countryContinentMapping)
joinedDf_expectancy = joinedDf_expectancy %>% select(country,continent,everything())




ExpectencyByContinent = joinedDf_expectancy %>% 
  select(continent,"2016.0") %>% 
  group_by(continent) %>% summarise(meanValues = mean(`2016.0`))

# ggplot(ExpectencyByContinent, aes(x = Con, y = lifeExp, color = continent)) + geom_point(alpha = 0.3) + 
#   scale_x_log10() + scale_color_manual(values = cb_palette) + 
#   geom_smooth(method = "lm", se = FALSE)


### gdp per capita data manips


gdpPerCapitaDf = gdpPerCapitaDf %>% rename(country = "GDP per capita")

joinedDf_gdp = gdpPerCapitaDf %>% 
  inner_join(countryContinentMapping,by = c('country'='country'))


GDPByContinent = joinedDf_gdp %>% 
  select(continent,"2015.0") %>% 
  group_by(continent) %>% summarise(meanValues = mean(`2015.0`))



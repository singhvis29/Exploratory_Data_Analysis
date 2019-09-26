library(gapminder)
library(tidyverse)

myDf = gapminder

lifeExpectancyDf = readxl::read_xlsx('indicator life_expectancy_at_birth.xlsx')



lifeExpectancyDf = lifeExpectancyDf %>% 
                    (function(x) {as.data.frame(t(x))})



lifeExpectancyDf = lifeExpectancyDf %>% mutate(Year = row.names(lifeExpectancyDf))
lifeExpectancyDf

colnames(lifeExpectancyDf) = as.character(lifeExpectancyDf[1,])

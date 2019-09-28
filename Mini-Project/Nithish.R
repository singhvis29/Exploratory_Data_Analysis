library(gapminder)
library(tidyverse)

gapminderDf = gapminder

cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

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

mean_gdp = mean(Exp_GDP_Cont_2018$X2018_exp)
mean_lifeexp = mean(Exp_GDP_Cont_2018$X2018_gdp, na.rm = TRUE)

exp_gdp_cont_agg_2018 = Exp_GDP_Cont_2018  %>%
  group_by(continent) %>%
  summarise(meanExpect_2018 = mean(X2018_exp,na.rm=TRUE),
            meanGDP_2018 = mean(X2018_gdp,na.rm=TRUE),
            varexp_2018 = var(X2018_exp, na.rm = TRUE),
            varGDP_2018 = var(X2018_gdp, na.rm = TRUE),
            cor_exp_gdp_2018 = cor(X2018_gdp, X2018_exp, use = "complete.obs"))

write.csv(exp_gdp_cont_agg_2018, "2018_continent_average.csv")

ggplot(Exp_GDP_Cont_2018, aes(x = X2018_exp, y = X2018_gdp)) + 
  geom_point(alpha = 0.3)+scale_color_manual(values = cb_palette)+
  geom_smooth(method = "lm", se = FALSE)+ facet_grid(rows = "continent") +
  scale_color_manual(values = cb_palette)





####Question 2


lifeExpectancyDf_cont = lifeExpectancyDf %>% 
  inner_join(countryContinentMapping,by = c("country" = "country"))

avgLifeExpectancy_cont = lifeExpectancyDf_cont %>% select(-country) %>% 
  group_by(continent) %>% summarise_all(mean)

avgLifeExp_Long = avgLifeExpectancy_cont %>% 
  gather(.,"Year","Expectancy",'X1800':"X2018") %>% 
  mutate(Year = str_replace(Year,'X','')) %>% 
  mutate(Year = as.integer(Year))

avgLifeExp_Long_postWW = avgLifeExp_Long %>% filter(Year > 1945)


ggplot(avgLifeExp_Long_postWW,aes(x = Year,y = Expectancy))+geom_point(alpha = 0.3)+scale_color_manual(values = cb_palette)+
  geom_smooth(method = "lm", se = FALSE)+ facet_grid(rows = "continent")









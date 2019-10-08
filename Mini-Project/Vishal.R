library(gapminder)
library(tidyverse)
library(lme4)
options(scipen=999)
library(standardize)
gapminderDf = gapminder

cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

##load data

lifeExpectancyDf  = read.csv('life_expectancy_years.csv')
gdpPerCapitaDf = read.csv('income_per_person_gdppercapita_ppp_inflation_adjusted.csv' )


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

#Exp_GDP_Cont_2018_norm = transform(data_header, z.v1 = (v1 - mean(v1, na.rm =T))/(2*sd(v1, na.rm =T)))

Exp_GDP_Cont_2018_norm = Exp_GDP_Cont_2018

lms_cont = lmList(X2018_exp ~ X2018_gdp | continent, data = Exp_GDP_Cont_2018)
summary(lms_cont)

ggplot(Exp_GDP_Cont_2018, aes(x = X2018_gdp, y = X2018_exp,color = continent)) + 
  
  geom_point(alpha = 0.3)+scale_color_manual(values = cb_palette)+
  geom_smooth(method = "lm", se = FALSE)+ facet_grid(rows = "continent")+
  theme(legend.position = "none")

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

avgLifeExpectancy_cont = lifeExpectancyDf_cont %>% 
  group_by(continent) %>% summarise_all(mean)

avgLifeExp_Long = avgLifeExpectancy_cont %>% 
  gather(.,"Year","Expectancy",'X1800':"X2018") %>% 
  mutate(Year = str_replace(Year,'X','')) %>% 
  mutate(Year = as.integer(Year))

avgLifeExp_Long_postWW = avgLifeExp_Long %>% filter(Year > 1945)


ggplot(avgLifeExp_Long_postWW,aes(x = Year,y = Expectancy,color = continent))+geom_point(alpha = 0.3)+scale_color_manual(values = cb_palette)+
  geom_smooth(method = "lm", se = FALSE)+ facet_grid(rows = "continent")+
  theme(legend.position = "none")

ggplot(avgLifeExp_Long_postWW,aes(x = Year,y = Expectancy,color = continent))+geom_point(alpha = 0.3)+scale_color_manual(values = cb_palette)+
  geom_smooth(method = "lm", se = FALSE)+ facet_grid(rows = "continent")+
  theme(legend.position = "none")

lms_cont_q2 = lmList(Expectancy ~ Year | continent, data = avgLifeExp_Long_postWW)
summary(lms_cont_q2)


#### Question 3



expectancyLong = lifeExpectancyDf %>% 
  gather(key = "Year",value = "Expectancy",'X1800':"X2018") %>% 
  mutate(Year = str_replace(Year,'X','')) %>% 
  mutate(Year = as.integer(Year)) %>% inner_join(countryContinentMapping,by = c("country" = "country")) %>% 
  select(country,continent,everything())
  
gdpLong = gdpPerCapitaDf %>% 
  gather(key = "Year",value = "GDP/Capita",'X1800':"X2018") %>% 
  mutate(Year = str_replace(Year,'X','')) %>% 
  mutate(Year = as.integer(Year)) %>% inner_join(countryContinentMapping,by = c("country" = "country")) %>% 
  select(country,continent,everything())


consolidatedDf = expectancyLong %>% 
              inner_join(gdpLong) %>% filter("Year">1945,"GDP/Capita" > 0) %>% 
              mutate(YearBins = cut(Year,6,dig.lab = 4))




ggplot(consolidatedDf,aes(y = Expectancy,x = `GDP/Capita`,color = continent))+
  scale_color_manual(values = cb_palette)+
  geom_smooth(method = "lm", se = FALSE)+facet_grid(rows = "YearBins")




  









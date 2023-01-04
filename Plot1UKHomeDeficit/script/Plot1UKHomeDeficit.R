#Clear environment if necessary 

#rm(list=ls())

#call packages from library

library(tidyverse)
library(scales)
library(ggimage)
library(owidR)
library(countrycode)

#Homes data imported from data folder - originally from statsita at 'https://www.statista.com/statistics/867687/total-number-dwellings-per-one-thousand-citizens-europe/'

homes <- read.csv('./Plot1UKHomeDeficit/data/HomesData.csv') %>%
  select(1,2)%>%
  mutate(HomesPerCapita = as.integer(HomesPerCapita))

#population data from our world in data package

population <- owid("population")%>%
  rename(Country=entity,
         Code=code)%>%
  filter(year==2021)%>%
  select(Country, Population, Code)

#UK population figure from owid 

UKpopulation <- population %>% 
  filter(Country == "United Kingdom")%>%
  rename(UKpop=Population)%>%
  select(UKpop)

#Combined dataset to do analysis

combined <- homes %>%
  left_join(population, by="Country")%>%
  cbind(UKpopulation)%>%
  mutate(dwellings = HomesPerCapita*Population/1000,
         ukeurohomes = HomesPerCapita*UKpop/1000,
         ukdeficit = ukeurohomes - 23817488)%>%
  arrange(ukdeficit)%>%
  filter(Country!="United Kingdom")

#Exploratory plot to look at home deficit 

plot1 <- ggplot(combined)+
  geom_bar(stat="identity", aes(x=Code, y=ukdeficit))+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6), expand=c(0,0))+
  theme_classic()+
  labs(x="",
       y="",
       title="UK home deficit compared to Europe",
       caption="Data from Statistica")

ggsave("./Plot1UKHomeDeficit/output/Exploratory Plot.png", plot=plot1, width=10, height=8)

#Change country codes for doing plot with flags    

combined2 <- combined %>%
  mutate(Code2 = countrycode(Code,"iso3c","iso2c"))

#Final plot with flags next to country codes and cleaned up visuals

plot2 <- ggplot(data=combined2)+ 
  geom_col(aes(y = reorder(Country, +ukdeficit), x = ukdeficit), width = .9, fill="#014d64")+
  geom_flag( aes(x=-600000, y=Country,image = Code2))+
  labs(x="",
       y="",
       title = "UK home deficit",
       subtitle = "The UK has to build substantially more homes to match the per capita levels of European countries",
       caption = "Data from Statista\nProduced by @JackD1801")+
  theme_minimal()+
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6), limits=c(-600000, 17000000), position="top", expand=c(0.02, 0))+
  theme(axis.title = element_blank()) + 
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) + 
  theme(plot.background = element_rect(fill = "lightyellow", color = NA)) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major.y = element_blank()) + 
  theme(panel.grid.major.x = element_line(linewidth = 0.7, color = "grey80")) + 
  theme(plot.title = element_text(size = 12, color = "grey20", face="bold")) + 
  theme(plot.subtitle = element_text(size = 9, color = "grey20")) + 
  theme(plot.caption = element_text(size = 9, face = "italic", vjust = -4, color="grey20")) + 
  theme(axis.text = element_text(size = 9))

ggsave("./Plot1UKHomeDeficit/output/Plot1UKHomeDeficit.png", plot=plot2, width=10, height=8)

#Clear memory in RStudio if needed

#rm(list = ls())

#call packages to make plot 

library(tidyverse)
library(gganimate)
library(gapminder)
library(ggthemes)
library(scales)
library(gifski)

#Country List variable to look at different options for selecting country 

CountryList <- gapminder %>% select(country) %>% distinct()

#Select country to plot 

SelectedCountry <- "Rwanda"

#Initial graph to be animated

graph1 <- ggplot(data=gapminder%>%filter(country==SelectedCountry), aes(x=gdpPercap, y=lifeExp, size=pop, colour=continent))+
  geom_point(data = gapminder, aes(x=gdpPercap, y=lifeExp, size=pop, colour=continent),alpha=0.5, stroke=0)+
  geom_point(aes(x=gdpPercap, y=lifeExp, size=pop*1.1), colour="black")+
  geom_point()+
  theme_fivethirtyeight()+
  scale_size(range=c(2,12), guide="none")+
  scale_x_log10( labels = label_dollar(prefix = "$"))+
  labs(title=paste("Life Expectancy vs GDP per Capita - ",SelectedCountry),
       x="Income per person (GDP / capita)",
       y="Life Expectancy (Years)",
       caption="Source: Gapminder",
       colour="Continent")+
  theme(axis.title=element_text(),
        text=element_text(),
        legend.text=element_text(size=10))+
  guides(color = guide_legend(override.aes = list(size = 4)))+
  scale_color_brewer(palette="Set2")+
  geom_text(label=SelectedCountry, 
            nudge_x=0.0,
            nudge_y=3,
            size=4,
            colour="grey20")

#Make animation 

graph1.animation = graph1 +
  transition_time(year)+
  labs(subtitle = "Year: {frame_time}")

animate(graph1.animation, height = 500, width = 800, fps=50, duration = 10, 
        end_pause = 60, res=100)

#Save animation 

anim_save("./Plot3GapminderRwanda/output/Plot3GapminderRwanda.gif")

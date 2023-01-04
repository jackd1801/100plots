#Clear global environment if necessary
#rm(list = ls())

#load necessary packages

library(tidyverse)
library(readxl)
library(janitor)
library(owidR)
library(countrycode)
library(ggthemes)
library(transformr)
library(scales)

#Data comes from World Bank here: https://data.worldbank.org/indicator/SM.POP.NETM

migrationdata <- read_excel("./Plot2NetMigration/data/NetMigration.xlsx")

#Population data from our world in data

population <- owid("population")%>%
              rename('Country Code' = code,
                     Year=year)

#Combine datasets for complete dataset

combined <- migrationdata%>%
          remove_empty(which = "cols") %>%
          pivot_longer(cols=starts_with(c("19", "20")), names_to = "Year", values_to = "Migration")%>%
          mutate(Year=as.integer(Year)) %>%
          left_join(population, by=c("Country Code", "Year")) %>%
          set_names(c("country", "code", "indicatorn", "indicatorc", "year", "migration", "entity", "pop"))%>%
          select(country, code, year, migration, pop) %>%
          mutate(migrationperc = migration/pop)%>%
          filter(year==2017)

mapdata <- map_data("world")%>%
            filter(region != "Antarctica") %>%
            mutate(code = countrycode(region, "country.name", "iso3c"))%>%
            full_join(combined)
  
plot1 <- ggplot(mapdata, aes(long,lat))+
        geom_polygon(aes(group=group, fill=migrationperc))+
        scale_fill_viridis_c(direction=-1, labels=percent)+
        theme_map() +
        labs(title="Net Migration as a % of population 2012-2017",
             fill="Percent Migration",
             caption = "Data from World Bank\nGraphic made by @JackD1801")+
        theme(legend.position=c(0.4, 0.00),
              legend.direction="horizontal",
              plot.title=element_text(size=20, color="grey20", hjust=.5, vjust=1),
              plot.caption=element_text(size=10, color="grey20"),
              legend.text = element_text(size=10, color="grey20"),
              legend.title = element_text(size=10, color="grey20"),
              legend.key.size = unit(1, 'cm'))

ggsave("./Plot2NetMigration/output/Plot2NetMigration.jpeg", plot1, width=14, height=6.5, dpi=800)


          






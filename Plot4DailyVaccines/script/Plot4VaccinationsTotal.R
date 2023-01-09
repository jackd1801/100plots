# Clear environment if necessary 
rm(list=ls())


#install packages

library(tidyverse)
library(gganimate)
library(gifski)
library(scales)
library(RcppRoll)

#load data 

raw <- read.csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=vaccinationsAgeDemographics&format=csv')



cumdoses <- raw %>%
          select(date, age, VaccineRegisterPopulationByVaccinationDate, cumPeopleVaccinatedFirstDoseByVaccinationDate, cumPeopleVaccinatedSecondDoseByVaccinationDate, cumPeopleVaccinatedThirdInjectionByVaccinationDate)%>%
          rename(pop = VaccineRegisterPopulationByVaccinationDate,
                 firstdose = cumPeopleVaccinatedFirstDoseByVaccinationDate,
                 seconddose = cumPeopleVaccinatedSecondDoseByVaccinationDate,
                 thirddose = cumPeopleVaccinatedThirdInjectionByVaccinationDate)%>%
          filter(age!="75+" & age!= "50+")%>%
          mutate(age=case_when(
            age%in%c("05_11", "12_15", "16_17")~"5-18",
            age%in%c("18_24", "25_29")~"18-29",
            age%in%c("30_34", "35_39", "40_44", "45_49")~"30-49",
            age%in%c("50_54", "55_59")~"50-59",
            age%in%c("60_64", "65_69")~"60-69",
            age%in%c("70_74", "75_79")~"70-79",
            age%in%c("80_84", "85_89", "90+")~"80+"
          ),
          date=as.Date(date))%>%
          group_by(date, age)%>%
          summarize(pop=sum(pop),
                    firstdose=sum(firstdose),
                    seconddose=sum(seconddose),
                    thirddose=sum(thirddose))%>%
          ungroup()%>%
          filter(date<="2022-02-01")

latestdate <- max(cumdoses$date)

plot1 <- ggplot(cumdoses)+
          geom_line(aes(x=date,y=firstdose, colour=age))+
          geom_line(aes(x=date,y=seconddose, colour=age), linetype="dashed")+
          geom_line(aes(x=date,y=thirddose, colour=age), linetype="dotted")+
          labs(x="",
               y="",
               title = "Total COVID-19 vaccine doses administered by dose type - UK",
               subtitle="First dose (solid), second dose (dashed), third dose (dotted)",
               caption="Data from coronavirus.data.gov\nProduced by @JackD1801")+ 
          scale_x_date()+
          scale_y_continuous(expand=c(0,0))+
          theme_classic()+
          theme(legend.position="")+
          geom_text(data=cumdoses%>%filter(date==latestdate), aes(label=age, x=date+6, y=firstdose,color=age), size=4)
          

plot1

newdoses <- raw %>%
  select(date, age, VaccineRegisterPopulationByVaccinationDate, newPeopleVaccinatedFirstDoseByVaccinationDate, newPeopleVaccinatedSecondDoseByVaccinationDate, newPeopleVaccinatedThirdInjectionByVaccinationDate)%>%
  rename(pop = VaccineRegisterPopulationByVaccinationDate,
         firstdose = newPeopleVaccinatedFirstDoseByVaccinationDate,
         seconddose = newPeopleVaccinatedSecondDoseByVaccinationDate,
         thirddose = newPeopleVaccinatedThirdInjectionByVaccinationDate)%>%
  filter(age!="75+" & age!= "50+")%>%
  mutate(age=case_when(
    age%in%c("05_11", "12_15", "16_17")~"5-18",
    age%in%c("18_24", "25_29")~"18-29",
    age%in%c("30_34", "35_39", "40_44", "45_49")~"30-49",
    age%in%c("50_54", "55_59")~"50-59",
    age%in%c("60_64", "65_69")~"60-69",
    age%in%c("70_74", "75_79")~"70-79",
    age%in%c("80_84", "85_89", "90+")~"80+"),
  date=as.Date(date))%>%
  group_by(age, date)%>%
  
#add the rolling sum
  summarize(pop=sum(pop),
            firstdose=sum(firstdose),
            seconddose=sum(seconddose),
            thirddose=sum(thirddose))%>%
  ungroup()%>%
  group_by(age)%>%
  arrange(date)%>%
  mutate(firstdoseroll = roll_sum(firstdose, n=7, fill=NA, align="left")/7,
         seconddoseroll = roll_sum(seconddose, n=7, fill=NA, align="left")/7,
         thirddoseroll = roll_sum(thirddose, n=7, fill=NA, align="left")/7)%>%
  filter(date<="2022-02-01")

plot2 <- ggplot(newdoses)+
  geom_line(aes(x=date,y=firstdoseroll, colour=age))+
  geom_line(aes(x=date,y=seconddoseroll, colour=age), linetype="dashed")+
  geom_line(aes(x=date,y=thirddoseroll, colour=age), linetype="dotted")+
  labs(x="",
       y="",
       title = "New COVID-19 vaccine doses administered by dose type - UK",
       subtitle="First dose (solid), second dose (dashed), third dose (dotted)",
       caption="Data from coronavirus.data.gov\nProduced by @JackD1801",
       colour="")+ 
  scale_x_date(date_labels = "%b-%y")+
  scale_y_continuous(expand=c(0,0), labels = label_comma())+
  theme_classic()+
  theme(legend.position="bottom")+
  guides(colour=guide_legend(nrow=1))
  #geom_text(data=newdoses, aes(label=age, x=date+6, y=firstdose, colour=age), size=4)
plot2

plot2.animation = plot2 + 
                  transition_reveal(date)+
                  view_follow(fixed_x=FALSE, fixed_y=FALSE)

plot2.animation

animate(plot2.animation, height=500, width=800, fps=30, duration = 25, 
        end_pause=60, res=100)

#tidy data version

tidydoses <- newdoses %>%
              select(date, age, firstdoseroll, seconddoseroll, thirddoseroll)%>%
              rename(First = firstdoseroll,
                     Second = seconddoseroll,
                     Third = thirddoseroll)%>%
              pivot_longer(cols=c("First", "Second", "Third"), names_to="dose", values_to="total")

plot3 <- ggplot(tidydoses)+
  geom_line(aes(x=date,y=total, colour=age, linetype=dose))+
  labs(x="",
       y="",
       title = "New COVID-19 vaccine doses administered by dose type - UK",
       subtitle="",
       caption="Data from coronavirus.data.gov\nProduced by @JackD1801",
       colour="")+ 
  scale_x_date(date_labels = "%b-%y")+
  scale_y_continuous(expand=c(0,0), labels = label_comma())+
  theme_classic()+
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin())+
  guides(colour=guide_legend(nrow=1))
#geom_text(data=newdoses, aes(label=age, x=date+6, y=firstdose, colour=age), size=4)
plot3

#1
Date1 = as.Date('22/11/1963', format='%d/%m/%Y', tz="AST")
Date2 = as.Date('21/02/1965', format='%d/%m/%Y', tz='AST')
Date3 = as.Date('4/4/1968', format='%d/%m/%Y', tz='AST')

Date1-Date2
Date2-Date3

#2
date2 = as.Date('15/1/2021', format='%d/%m/%Y', tz="America/Halifax")
length(seq(date2, length=20, by='day'))

#3
launch=as.POSIXct("1969-07-16 13:32:00")
land=as.POSIXct("1969-07-20 20:17:40")

difftime(launch,land, units="secs")

#For task 1
library(tidyverse)
data<-read.csv("Weather_Data_2017.csv")

sml_data<-data %>%  
  select(Date.and.Time, Hourly.Maximum.Gust) %>% 
  filter(Hourly.Maximum.Gust >34) %>% 
  mutate(Date.and.Time=(as.POSIXct(Date.and.Time)))

difftime(sml_data$Date.and.Time[-1], sml_data$Date.and.Time[-length(sml_data$Date.and.Time)], unit='hours') 


#For task 2
data %>% 
  mutate(Date.and.Time = (as.POSIXct(Date.and.Time)),
         Hourly.Maximum.Gust = as.numeric(Hourly.Maximum.Gust),
         gale=if_else(Hourly.Maximum.Gust>34,1,0)) %>% 
  select(Date.and.Time, Hourly.Maximum.Gust, gale) %>%
  ggplot(aes(x = Date.and.Time, y = Hourly.Maximum.Gust))+
  geom_point(aes(color=factor(gale)))+
  theme_classic()+
  guides(color=FALSE)


#Baseball series
mlb_pitching <- na.omit(read.csv("mlb2017_pitching.txt"))

mlb_pitching <- mlb_pitching %>% 
  mutate(Name_clean = str_extract(Name,"[A-Z][a-z]+ [A-Z][a-z]+"))

#7
mlb_pitching%>% 
  n_distinct(.$Name)

#8
length(grep("4.", mlb_pitching$Age))/length(mlb_pitching$Age)

#9
mlb_pitching$Name[grep("4.", mlb_pitching$Age)]

#10
mlb_pitching  %>% filter(Name_clean=="Al Alburquerque")


# Webscrapping =================================================================
#install.packages('rvest')

library('rvest')
url <- 'http://www.imdb.com/search/title?count=100&release_date=2019,2019&title_type=feature'

webpage <- read_html(url)

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
#url <- 'http://www.imdb.com/search/title?count=100&release_date=2019,2019&title_type=feature'
url <- 'https://raw.githubusercontent.com/Diego-Ibarra/biol3782/main/week8/imdb_100titles_2019.html'

webpage <- read_html(url)

#Using CSS selectors to scrape the rankings section
rank_data_html <- html_nodes(webpage,'.text-primary')

#Converting the ranking data to text
rank_data <- html_text(rank_data_html)

head(rank_data)


rank_data <- as.numeric(rank_data)

title_data_html <- html_nodes(webpage,'.lister-item-header a')

#Converting the title data to text
title_data <- html_text(title_data_html)

head(title_data)

#Using CSS selectors to scrape the description section
description_data_html <- html_nodes(webpage,'.ratings-bar+ .text-muted')

#Converting the description data to text
description_data <- html_text(description_data_html)

description_data %>% 
  head()

#Removing '\n'
description_data <- gsub("\n","",description_data)

head(description_data)



#Using CSS selectors to scrape the Movie runtime section
runtime_data_html <- html_nodes(webpage,'.text-muted .runtime')

#Converting the runtime data to text
runtime_data <- html_text(runtime_data_html)

head(runtime_data)




#Removing mins and converting it to numerical

runtime_data <- gsub(" min","",runtime_data)
runtime_data <- as.numeric(runtime_data)

head(runtime_data)





#Using CSS selectors to scrape the Movie genre section
genre_data_html <- html_nodes(webpage,'.genre')

#Converting the genre data to text
genre_data <- html_text(genre_data_html)

head(genre_data)





#Removing \n
genre_data <- gsub("\n","",genre_data)

#Removing excess spaces
genre_data <- gsub(" ","",genre_data)

head(genre_data)





#taking only the first genre of each movie
genre_data <- gsub(",.*","",genre_data)

#Convering each genre from text to factor
genre_data <- as.factor(genre_data)

head(genre_data)





#Using CSS selectors to scrape the IMDB rating section
rating_data_html <- html_nodes(webpage,'.ratings-imdb-rating strong')

#Converting the ratings data to text
rating_data <- html_text(rating_data_html)

str(rating_data)




#Converting ratings to numeric
rating_data <- as.numeric(rating_data)

head(rating_data)





#Using CSS selectors to scrape the votes section
votes_data_html <- html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)')

#Converting the votes data to text
votes_data <- html_text(votes_data_html)

#Removing commas
votes_data <- gsub(",","",votes_data)

#Converting votes to numerical
votes_data <- as.numeric(votes_data)

head(votes_data)




#Using CSS selectors to scrape the directors section
directors_data_html <- html_nodes(webpage,'.text-muted+ p a:nth-child(1)')

#Converting the directors data to text
directors_data <- html_text(directors_data_html)

#Converting directors data into factors
directors_data <- as.factor(directors_data)

head(directors_data)




#Using CSS selectors to scrape the actors section
actors_data_html <- html_nodes(webpage,'.lister-item-content .ghost+ a')

#Converting the gross actors data to text
actors_data <- html_text(actors_data_html)

#Converting actors data into factors
actors_data <- as.factor(actors_data)

head(actors_data)




#Using CSS selectors to scrape the metascore section
metascore_data_html <- html_nodes(webpage,'.metascore')

#Converting the runtime data to text
metascore_data <- html_text(metascore_data_html)

#Removing extra space in metascore
metascore_data <- gsub(" ","",metascore_data)

head(metascore_data)

#converting metascore to numerical
metascore_data <- as.numeric(metascore_data)

summary(metascore_data)




#Using CSS selectors to scrape the gross revenue section
gross_data_html <- html_nodes(webpage,'.ghost~ .text-muted+ span')

#Converting the gross revenue data to text
gross_data <- html_text(gross_data_html)

gross_data  %>% 
  head()




#Removing '$' and 'M' signs
gross_data <- gsub("M","",gross_data)

gross_data <- substring(gross_data,2,6)

gross_data  %>% 
  head()


gross_data <- as.numeric(gross_data)

length(gross_data)
summary(gross_data)


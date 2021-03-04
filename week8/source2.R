library('tidyverse')



#for question below 12

#Specifying the url for desired website to be scraped
url <- 'https://raw.githubusercontent.com/Diego-Ibarra/biol3782/main/week8/imdb_100titles_2016.html'

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the rankings section
rank_data_html <- html_nodes(webpage,'.text-primary')

#Converting the ranking data to numeric
rank_data <- html_text(rank_data_html) %>% 
  as.numeric()

#Let's have a look at the rankings
head(rank_data)



#For questions below 13

#Using CSS selectors to scrape the title section
title_data_html <- html_nodes(webpage,'.lister-item-header a')

#Converting the title data to text
title_data <- html_text(title_data_html)

#Let's have a look at the title
head(title_data)



#For questions below 14
tail(title_data)



#For questions below 15
length(title_data)





# 16
#Using CSS selectors to scrape the description section
description_data_html <- html_nodes(webpage,'.ratings-bar+ .text-muted')

#Converting the description data to text
description_data <- html_text(description_data_html)

description_data <- gsub("\n","",description_data)

#For questions below 16
head(description_data, 19)

#For questions below 17
head(title_data, 17)
title_data[17]


# ---------
#Using CSS selectors to scrape the Movie runtime section
runtime_data_html <- html_nodes(webpage,'.text-muted .runtime')

#Converting the runtime data to text
runtime_data <- html_text(runtime_data_html)

runtime_data<-gsub(" min","",runtime_data)
runtime_data<-as.numeric(runtime_data)

# ---------
#Using CSS selectors to scrape the Movie genre section
genre_data_html <- html_nodes(webpage,'.genre')

#Converting the genre data to text
genre_data <- html_text(genre_data_html)

#Data-Preprocessing: removing \n
genre_data<-gsub("\n","",genre_data)

#Data-Preprocessing: removing excess spaces
genre_data<-gsub(" ","",genre_data)

#taking only the first genre of each movie
genre_data<-gsub(",.*","",genre_data)

#Convering each genre from text to factor
genre_data<-as.factor(genre_data)

#Let's have another look at the genre data
head(genre_data)

# -------------
#Using CSS selectors to scrape the IMDB rating section
rating_data_html <- html_nodes(webpage,'.ratings-imdb-rating strong')

#Converting the ratings data to text
rating_data <- html_text(rating_data_html)

#Data-Preprocessing: converting ratings to numerical
rating_data <- as.numeric(rating_data)

#Let's have another look at the ratings data
head(rating_data)

# ----------------
#Using CSS selectors to scrape the votes section
votes_data_html <- html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)')

#Converting the votes data to text
votes_data <- html_text(votes_data_html)

#Data-Preprocessing: removing commas
votes_data <- gsub(",","",votes_data)

#Data-Preprocessing: converting votes to numerical
votes_data <- as.numeric(votes_data)

#Let's have another look at the votes data
head(votes_data)

# -------------
#Using CSS selectors to scrape the directors section
directors_data_html <- html_nodes(webpage,'.text-muted+ p a:nth-child(1)')

#Converting the directors data to text
directors_data <- html_text(directors_data_html)

#Data-Preprocessing: converting directors data into factors
directors_data<-as.factor(directors_data)

head(directors_data)

# ------------------
#Using CSS selectors to scrape the actors section
actors_data_html <- html_nodes(webpage,'.lister-item-content .ghost+ a')

#Converting the gross actors data to text
actors_data <- html_text(actors_data_html)

#Data-Preprocessing: converting actors data into factors
actors_data <- as.factor(actors_data)

head(actors_data)

# ---------
#Using CSS selectors to scrape the metascore section  - 19
metascore_data_html <- html_node(html_nodes(webpage, '.lister-item-content'), '.metascore')

#Converting the runtime data to text
metascore_data <- html_text(metascore_data_html)

#Data-Preprocessing: converting metascore to numerical
metascore_data <- as.numeric(metascore_data)

#Let's have another look at length of the metascore data

length(metascore_data)

summary(metascore_data)


# 20
#Using CSS selectors to scrape the gross revenue section
gross_data_html <- html_node(html_nodes(webpage, '.lister-item-content'), '.sort-num_votes-visible span:nth-child(5)')

#Converting the gross revenue data to text
gross_data <- html_text(gross_data_html)

#Removing '$' and 'M' signs
gross_data <- gsub("M","",gross_data)

gross_data <- substring(gross_data,2,6)

#converting gross_data to numerical
gross_data <- as.numeric(gross_data)

length(gross_data)

head(gross_data)

summary(gross_data)


# 21 and 22
#Combining all the lists to form a data frame

movies_df <- data.frame(Rank = rank_data, Title = title_data,
                        Description = description_data, Runtime = runtime_data,
                        Genre = genre_data, Rating = rating_data,
                        Metascore = metascore_data, 
                        Votes = votes_data,
                        Gross_Earning_in_Mil = gross_data,
                        Director = directors_data, Actor = actors_data)

str(movies_df)



#For question below 23
movies_df[56,]


#For question below 24
movies_df[73,]

#For question below 25
n_distinct(movies_df$Director)

#For question below 26
movies_df[movies_df$Director %in% movies_df$Director[duplicated(movies_df$Director)],]

#For question below 27
movies_df[50,]$Description  %>% 
  nchar()

#For question below 28
movies_df %>% 
  mutate(letter=str_extract(Director, "[A-Z]"))  %>% 
  group_by(letter) %>% 
  summarize(n=n())



#For question below 29
movies_df %>% 
  mutate(letter=str_extract(Director, "[a-z]"))  %>% 
  group_by(letter) %>% 
  summarize(n=n())

# or
movies_df %>% 
  mutate(letter=str_extract(Director, "^[A-Z]e.*"))  %>% 
  group_by(letter) %>% 
  summarize(n=n())




#For question below 30 , 31
movies_df %>% 
  filter(Actor =='Lily James')

#For question below 32
movies_df %>%
  arrange(desc(Runtime)) %>% 
  select(Runtime,Genre, Title) %>% 
  head()

#For question below 33, 34
movies_df %>%
  filter(Runtime >= 130 & Runtime <= 160) %>%
  arrange(desc(Votes)) %>% 
  select(Runtime,Genre, Votes, Title, Director) %>% 
  head()

#For question below 35, 36
movies_df %>%
  filter(Runtime >= 100 & Runtime <= 120) %>%
  select(Genre, Gross_Earning_in_Mil) %>% 
  group_by(Genre) %>% 
  na.omit() %>% 
  summarize(sum=sum(Gross_Earning_in_Mil))

#For question below 37
movies_df %>%
  filter(Runtime >= 100 & Runtime <= 120 & Genre == 'Crime') %>%
  head()

#For question below 38
movies_df %>%
  select(Genre, Gross_Earning_in_Mil) %>% 
  group_by(Genre) %>% 
  na.omit() %>% 
  summarize(sum=sum(Gross_Earning_in_Mil))

#For the question below 39
ggplot(movies_df, aes(x = Votes, y = Gross_Earning_in_Mil))+
  geom_point(aes(size = Rating, col = Genre))+
  theme_classic()

# 40 and 41
movies_df %>%
  filter(!is.na(Gross_Earning_in_Mil)) %>%
  group_by(Genre) %>%
  summarize(aveGross = mean(Gross_Earning_in_Mil), aveVotes = mean(Votes))


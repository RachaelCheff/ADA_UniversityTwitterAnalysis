setwd("C:/Users/Rachael Cheff/Desktop/AppliedDataAnalytics/2017-Fall-University_Study/")

library(ggplot2)
library(plyr)

words_count <- read.csv("university_words_count.csv",header=FALSE)
university_profiles <- read.csv("university_profiles.csv",header=TRUE)
university_twitter <- read.csv("university_only_twitter.csv",header=TRUE)

names(words_count) <- c("university_twitter_profile","word","count")
# This csv was created without headers, so I add some here.

state_counts <- data.frame(table(university_profiles$state))

University_Location <- university_profiles$state

# Number of Universities by State Plot
ggplot(mapping = aes(University_Location, colour=university_profiles$state)) +
  ggtitle("Count of WUE Universities by State")+
  geom_histogram(stat = "Count")+
  theme(axis.text.x=element_blank(),legend.title = element_text(colour = "black"))+
  scale_color_discrete(name="States")

  
# Top Words by Frequency Plot
all_Uni_words_count <- data.frame(table(words_count$word))
all_Uni_words_count <- order(all_Uni_words_count$Freq)


top_words <- subset(all_Uni_words_count,Freq>10000)

ggplot(top_words, aes(x = reorder(Var1, -Freq), y =Freq,colour=Freq)) +
  ggtitle("Top Words Used in WUE University Twitter Descriptions")+
  geom_col()+
  theme(legend.position = "none")


# Top Words Cats Vs Griz


Griz <- read.csv("C:/Users/Rachael Cheff/Desktop/AppliedDataAnalytics/2017-Fall-University_Study/UniversityWordCountData_Cleaned/umontana.csv",header = FALSE)
Griz$university <- "UM"

Cats <- read.csv("C:/Users/Rachael Cheff/Desktop/AppliedDataAnalytics/2017-Fall-University_Study/UniversityWordCountData_Cleaned/montanastate.csv",header = FALSE)
Cats$university <- "MSU"

CatsvGriz <- rbind(Cats,Griz)
names(CatsvGriz)<- (c("word","count","university"))
CatsvGriz <- subset(CatsvGriz,word != "a")
CatsvGriz <- subset(CatsvGriz, word!="the")
CatsvGriz <- subset(CatsvGriz, word != "in") 
CatsvGriz <- subset(CatsvGriz, word!= "and")
CatsvGriz <- subset(CatsvGriz, word!= "for")
CatsvGriz <- subset(CatsvGriz, word!= "of")
CatsvGriz <- subset(CatsvGriz, word!= "s")




ggplot(CatsvGriz,aes(word,count,colour=university)) +
  geom_col()

# Followers counts histogram

top_no_followers <- subset(university_twitter,followers_count > 20000)

ggplot(top_no_followers,aes(x=reorder(screen_name,-followers_count), y=followers_count))+
  geom_col()+
  ggtitle("WUE Universities with over 20,000 Followers")

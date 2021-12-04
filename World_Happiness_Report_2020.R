#project
#Analysis of the World Happiness Report 2020

####Loading packages
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("caTools")
library(caTools)
install.packages("ggplot2")
library(ggplot2)
install.packages("reshape2")
library(reshape2)
install.packages("tidyr")
library(tidyr)
install.packages("ggthemes")
library(ggthemes)
install.packages("corrgram")
library(corrgram)
install.packages("corrplot")
library(corrplot)
install.packages("rpart")
library(rpart)
install.packages("plotrix")
library(plotrix)


#Loading Data set(happiness_2020)
happiness_2020 <- read.csv("2020.csv")
print(getwd())

#Next let's check for missing values
colSums(is.na(happiness_2020))

#structure of the data
str(happiness_2020)

#dropping unnecessary columns
happiness_2020 <- happiness_2020[, -c(2,4,5,6,13,14,15,16,17,18,19)]
happiness_2020
head(happiness_2020)

#Renaming columns for convenience
happiness_2020 <- happiness_2020 %>% rename(c("Country" = "Country.name" ,
                          "Score" = "Ladder.score",
                          "GDP" = "Logged.GDP.per.capita", 
                          "Family" = "Social.support",
                          "Health" = "Healthy.life.expectancy", 
                          "Freedom" = "Freedom.to.make.life.choices",
                          "Corruption" = "Perceptions.of.corruption",
                          "Dystopia.residual" = "Dystopia...residual"))
head(happiness_2020)

#Adding country ranks in the data set according to their scores
happiness_2020 <- happiness_2020 %>% mutate(Rank = row_number())
options(max.print=100000)
head(happiness_2020)

#Correlation matrix
#To find out which factors correlate the most with the happiness quotient
str(happiness_2020)
num.cols <- sapply(happiness_2020,is.numeric) 
cor.data <- cor(happiness_2020[,num.cols]) 
print(cor.data)

#graph 1
#correlation matrix
cor <- cor(happiness_2020)
str(happiness_2020)
colnames(happiness_2020)
head(happiness_2020) 
summary(happiness_2020)
print(corrplot(cor.data, method = 'color'))

#Hence, happiness most strongly correlates with the attributes of GDP, Health, Social Support(Family) and Freedom.

# Creating a new column for continents
happiness_2020$Continent <- NA
happiness_2020$Continent[which(happiness_2020$Country %in% c("Israel", "United Arab Emirates", "Singapore", "Thailand", "Taiwan Province of China",
                                         "Qatar", "Saudi Arabia", "Kuwait", "Bahrain", "Malaysia", "Uzbekistan", "Japan",
                                         "South Korea", "Turkmenistan", "Kazakhstan", "Turkey", "Hong Kong S.A.R., China", "Philippines",
                                         "Jordan", "China", "Pakistan", "Indonesia", "Azerbaijan", "Lebanon", "Vietnam",
                                         "Tajikistan", "Bhutan", "Kyrgyzstan", "Nepal", "Mongolia", "Palestinian Territories",
                                         "Iran", "Bangladesh", "Myanmar", "Iraq", "Sri Lanka", "Armenia", "India", "Georgia",
                                         "Cambodia", "Afghanistan", "Yemen", "Syria"))] <- "Asia"
happiness_2020$Continent[which(happiness_2020$Country %in% c("Norway", "Denmark", "Iceland", "Switzerland", "Finland",
                                         "Netherlands", "Sweden", "Austria", "Ireland", "Germany",
                                         "Belgium", "Luxembourg", "United Kingdom", "Czech Republic",
                                         "Malta", "France", "Spain", "Slovakia", "Poland", "Italy",
                                         "Russia", "Lithuania", "Latvia", "Moldova", "Romania",
                                         "Slovenia", "North Cyprus", "Cyprus", "Estonia", "Belarus",
                                         "Serbia", "Hungary", "Croatia", "Kosovo", "Montenegro",
                                         "Greece", "Portugal", "Bosnia and Herzegovina", "Macedonia",
                                         "Bulgaria", "Albania", "Ukraine"))] <- "Europe"
happiness_2020$Continent[which(happiness_2020$Country %in% c("Canada", "Costa Rica", "United States", "Mexico",  
                                         "Panama","Trinidad and Tobago", "El Salvador", "Belize", "Guatemala",
                                         "Jamaica", "Nicaragua", "Dominican Republic", "Honduras",
                                         "Haiti"))] <- "North America"
happiness_2020$Continent[which(happiness_2020$Country %in% c("Chile", "Brazil", "Argentina", "Uruguay",
                                         "Colombia", "Ecuador", "Bolivia", "Peru",
                                         "Paraguay", "Venezuela"))] <- "South America"
happiness_2020$Continent[which(is.na(happiness_2020$Continent))] <- "Africa"


# Moving the continent column's position in the data set to the second column
happiness_2020 <- happiness_2020 %>% select(Country,Continent, everything())
head(happiness_2020)

#graph 2
#Happiness score distribution across different continents using a scatter plot
gg1 <- ggplot(happiness_2020,
              aes(x=Continent,
                  y=Score,
                  color=Continent))+
  geom_point() + theme_bw() +
  theme(axis.title = element_text(family = "Helvetica", size = (8)))
gg1

#graph 8
mean(happiness_2020$GDP)
mean(happiness_2020$Family)
mean(happiness_2020$Health)#due to the pandemic the values were calculated for 100, so we modified it
mean(happiness_2020$Freedom)
mean(happiness_2020$Generosity)#negative value so,it is considered as 0.00001
mean(happiness_2020$Corruption)
#3d pie chart
library(plotrix)
slices<-c(9.295706,0.8087211,6.444553,0.7833602,0.00001,0.7331202)
pct<-round(slices/sum(slices)*100)
lbls<-paste(c("GDP","Family","Health","Freedom","Generosity","Corruption")," ",pct,"%",sep="")
pie3D(slices,labels=lbls,explode=0.3,
      main="3D Pie Chart")

#graph 3
#Top 10 countries (countries with highest happiness score)
gg2 <- ggplot(happiness_2020[1:10,], aes(x = reorder(Country, Score), y=Score, fill = Country)) +  
  ggtitle("Top 10 happiest countries in 2020", subtitle = "9 of the happiest countries are in Europe") + 
  geom_bar(stat="identity", width=0.7) + theme(plot.title = element_text(size=20)) + coord_flip()
gg2

#graph 4
#Bottom 10 countries (countries with least happiness score)
gg3 <- ggplot(happiness_2020[143:153,], aes(x = reorder(Country, -Score), y=Score, fill = Country)) + 
  ggtitle("Top 10 saddest countries in 2020") + geom_bar(stat="identity", width=0.7) + 
  theme(plot.title = element_text(size=20)) + coord_flip()
gg3

#graph 5
#What is the average health across the different regions
gg4 <- ggplot(happiness_2020, aes(x=Continent, y = Health))+
  geom_boxplot()+
  geom_violin(aes(fill=Continent))+
  theme_minimal()+
  stat_summary(geom = 'point', fun = 'mean', color='red')+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
gg4

#graph 6
#What is the average GDP across the different regions
gg5 <- ggplot(happiness_2020, aes(x=Continent, y = GDP))+
  geom_boxplot()+
  geom_violin(aes(fill=Continent))+
  theme_minimal()+
  stat_summary(geom = 'point', fun = 'mean', color='red')+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
gg5

#graph 7
#What is the average social support(family) across the different regions
gg6 <- ggplot(happiness_2020, aes(x=Continent, y = Family))+
  geom_boxplot()+
  geom_violin(aes(fill=Continent))+
  theme_minimal()+
  stat_summary(geom = 'point', fun = 'mean', color='red')+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
gg6

#Linear Regression Model
s=sample.split(happiness_2020,SplitRatio=0.7)
train=subset(happiness_2020,split=T)
test=subset(happiness_2020,split=F)

MODEL1=lm(Score~GDP,data=happiness_2020) #creating a regression model
p=predict(MODEL1,test) 
RSE=sigma(MODEL1)/mean(test$Score) 
summary(MODEL1)
acc<-sqrt(mean((test$Score-p)^2))
acc

#predicting the value
a<-data.frame(GDP=10.67856)
result=predict(MODEL1,a)
print(result)
acc<-sqrt(mean((test$Score-p)^2))
acc

MODEL2=lm(Score~Health,data=happiness_2020) 
a1<-data.frame(Health=74.40250)
result2=predict(MODEL2,a1)
RSE=sigma(MODEL2)/mean(test$Score)
p2=predict(MODEL2,test) 
acc2<-sqrt(mean((test$Score-p2)^2))
acc2
print(result2)
summary(MODEL2)

MODEL3=lm(Score~Family,data=happiness_2020) 
a2<-data.frame(Family=0.9559908)
result3=predict(MODEL3,a2)
RSE=sigma(MODEL3)/mean(test$Score) 
p3=predict(MODEL3,test) 
acc3<-sqrt(mean((test$Score-p3)^2))
acc3
print(result3)
summary(MODEL3)


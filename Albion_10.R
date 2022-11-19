Albion<- read.csv("Albion.csv", header = TRUE)
head(Albion)
str(Albion)
library(dplyr)

######## ALBION YEAR/MONTH ######## 

data <- Albion 
data$Day <- strftime(data$Date, "%d")
data$Month <- strftime(data$Date, "%b")
data$Month <- as.character(data$Month)
data$Year <- strftime(data$Date, "%Y")    
data$Month 



filteredyears <- filter(data, Year==1980 | Year==1985 | Year==1990 | Year==1995 | Year==2000 | Year==2005 | Year==2010 | Year==2015 | Year==2020 | Year==2022)


######## DATA AGGREGATE ######## 

data_aggr1 <-aggregate(CPUE ~ Month + Year,
                       data,
                       FUN = sum)

######## MEAN YEARLY CPUE ######## 

Mean_YearlyCPUE = data %>%
  group_by(Year) %>%
  summarise(Mean=mean(CPUE))
Mean_YearlyCPUE

######## July-October YEARLY MEAN CPUE ######## 

filtered07_10 <- data %>% filter(between(Month, 07, 10))
filtered07_10

Mean_YearlyCPUE07_10 = filtered07_10 %>%
  group_by(Year) %>%
  summarise(Mean=mean(CPUE))
Mean_YearlyCPUE07_10

######## April-June YEARLY MEAN CPUE ######## 

filtered04_06 <- data%>% filter( between(Month, 04, 06) )
filtered04_06

Mean_YearlyCPUE04_06 = filtered04_06 %>%
  group_by(Year) %>%
  summarise(Mean=mean(CPUE))
Mean_YearlyCPUE04_06

Mean_YearlyCPUE04_06

######## May-July YEARLY MEAN CPUE ######## 

filtered04_07 <- data%>% filter( between(Month, 04, 07) )
filtered04_07

Mean_YearlyCPUE04_07 = filtered04_07 %>%
  group_by(Year) %>%
  summarise(Mean=mean(CPUE))
Mean_YearlyCPUE04_07

##########################################################################

######## Graphs ######## 
################################################
######## April-June YEARLY MEAN CPUE PLOT ######## 

library(ggplot2)


Mean_YearlyCPUE04_06$real_year = as.numeric(as.character(Mean_YearlyCPUE04_06$Year))

plot1 <- Mean_YearlyCPUE04_06 %>%
  ggplot(aes(x=real_year, y=Mean, group=1))+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(title="April & June mean Chinook CPUE: 1980-2022", 
       x="Year",
       y="Mean CPUE")
plot1

################################################
######## April-July YEARLY MEAN CPUE PLOT ######## 

Mean_YearlyCPUE04_07$real_year = as.numeric(as.character(Mean_YearlyCPUE04_07$Year))
plot2 <- Mean_YearlyCPUE04_07 %>%
  ggplot(aes(x=real_year, y=Mean, group=1))+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(title="April-July Mean Chinook CPUE: 1980-2022", 
       x="Year",
       y="Mean CPUE")
plot2

################################################
######## YEARLY MEAN CPUE PLOT ######## 

Mean_YearlyCPUE$real_year = as.numeric(as.character(Mean_YearlyCPUE$Year))
plot3 <- Mean_YearlyCPUE %>%
  ggplot(aes(x=real_year, y=Mean, group=1))+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(title="Mean Chinook CPUE: 1980-2022", 
       x="Year",
       y="Mean CPUE")
plot3

################################################


install.packages("forecast")
library(lubridate)
library(ggplot2)
library(zoo)
library(ggplot2)
library(dplyr)


p <- ggplot(filteredyears, aes(x = Date, y = CPUE, color = as.factor(Year))) + 
  geom_line() +
  geom_point()

p +   scale_x_date(date_labels = "%b/%d")






require(ggplot2)
gg <- ggplot(data, aes(x=Month, y=CPUE, 
                      group=Year, colour=Year))
gg + geom_point() + geom_line()

ggplot(data, aes(x = Month, y = CPUE, color = as.factor(Year))) + 
  geom_line()

?geom_line


dat1 = data(data.frame(date = seq(as.Date("2015-01-15"), as.Date("2015-12-15"), "1 month"),
                  value = cumsum(rnorm(CPUE))
dat1$date = as.yearmon(dat1$date)))

dat2 = data.frame(date = seq(as.Date("2016-01-15"), as.Date("2016-12-15"), "1 month"),
                  value = cumsum(rnorm(12)))
dat2$date = as.yearmon(dat2$date)





































##########################################################################
######## NOT WORKING/UNFINISHED ######## 
################################################

######## May-June YEARLY MEAN CPUE PLOT ######## 

data$real_year = as.numeric(as.character(data$Year))

plot10 <- data %>%
  ggplot(aes(x=real_year, y=Mean, group=1))+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(title="April & June mean Chinook CPUE: 1980-2022", 
       x="Year",
       y="Mean CPUE")
plot10

################################################

Mean_YearlyCPUE05_07 %>%
  ggplot(aes(x=Year, fill=Year))+
  geom_bar(aes(y=stat(Mean_YearlyCPUE05_07)/sum(Mean_YearlyCPUE05_07)))+
  labs(title="Average CPUE May-June", 
       x="Year",
       y="Mean CPUE")

################################################

Mean_YearlyCPUE05_06 %>%
  my_dates <- as.Date(Year, tryFormats = c(1980:2022))

data %>%
  ggplot(aes(x=Year, y=CPUE))+
  geom_line(color="darkgreen") +
  scale_x_date(date_breaks = "5 years",
               date_labels = "%y")

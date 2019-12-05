
# BEIJING MULTI-SITE AIR-QUALITY ANALYSIS
# CAIO CESAR WASEM
# 05/12/2019

install.packages("ggpubr")
install.packages("hexbin")
install.packages("hrbrthemes")

library(readr)
library(plyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(hexbin)
library(hrbrthemes)

#Set directory 
setwd("F:\\Documents\\BeijingAirQuality\\CSV")

#Import .csv
files <- list.files(pattern = "*.csv", full.names = TRUE)
beijingData <- ldply(files, read_csv)

#Verify variables present in the dataframe
str(beijingData)

#Create a function to delete NA
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}

#Remove NA (From 420768 rows to 382168)
beijingData <- delete.na(beijingData)

#Create Air Quality Index columns for PM2.5 and PM10
beijingData["AQI25"] <- NA
beijingData$AQI25[beijingData$PM2.5<=30] <- 1
beijingData$AQI25[(beijingData$PM2.5>30)&(beijingData$PM2.5<=60)] <-2
beijingData$AQI25[(beijingData$PM2.5>60)&(beijingData$PM2.5<=90)] <-3
beijingData$AQI25[(beijingData$PM2.5>90)&(beijingData$PM2.5<=120)] <-4
beijingData$AQI25[(beijingData$PM2.5>120)&(beijingData$PM2.5<=250)] <-5
beijingData$AQI25[beijingData$PM2.5>250] <- 6

beijingData["AQI10"] <- NA
beijingData$AQI10[beijingData$PM10<=50] <- 1
beijingData$AQI10[(beijingData$PM10>50)&(beijingData$PM10<=100)] <-2
beijingData$AQI10[(beijingData$PM10>100)&(beijingData$PM10<=250)] <-3
beijingData$AQI10[(beijingData$PM10>250)&(beijingData$PM10<=350)] <-4
beijingData$AQI10[(beijingData$PM10>350)&(beijingData$PM10<=430)] <-5
beijingData$AQI10[beijingData$PM10>430] <- 6

#Change variables to factors
beijingData$wd <- as.factor(beijingData$wd)
beijingData$station <- as.factor(beijingData$station)
beijingData$AQI10 <- as.factor(beijingData$AQI10)
beijingData$AQI25 <- as.factor(beijingData$AQI25)
beijingData$year <- as.factor(beijingData$year)
beijingData$month <- as.factor(beijingData$month)

#Count New Number of Measurements per Year
beijingData %>% 
  count(year)

#Test Correlations
cor(beijingData$PM2.5,beijingData$PM10, method = "pearson")  #High Correlation

cor(beijingData$PM2.5,beijingData$SO2, method = "pearson")   #Low Correlation
cor(beijingData$PM2.5,beijingData$NO2, method = "pearson")   #Moderate Correlation
cor(beijingData$PM2.5,beijingData$CO, method = "pearson")    #High Correlation
cor(beijingData$PM2.5,beijingData$O3, method = "pearson")    #Negligible Correlation
cor(beijingData$PM2.5,beijingData$TEMP, method = "pearson")  #Negligible Correlation
cor(beijingData$PM2.5,beijingData$PRES, method = "pearson")  #Negligible Correlation
cor(beijingData$PM2.5,beijingData$DEWP, method = "pearson")  #Negligible Correlation
cor(beijingData$PM2.5,beijingData$RAIN, method = "pearson")  #Negligible Correlation
cor(beijingData$PM2.5,beijingData$WSPM, method = "pearson")  #Negligible Correlation

cor(beijingData$PM10,beijingData$SO2, method = "pearson")   #Low Correlation
cor(beijingData$PM10,beijingData$NO2, method = "pearson")   #Moderate Correlation
cor(beijingData$PM10,beijingData$CO, method = "pearson")    #High Correlation
cor(beijingData$PM10,beijingData$O3, method = "pearson")    #Negligible Correlation
cor(beijingData$PM10,beijingData$TEMP, method = "pearson")  #Negligible Correlation
cor(beijingData$PM10,beijingData$PRES, method = "pearson")  #Negligible Correlation
cor(beijingData$PM10,beijingData$DEWP, method = "pearson")  #Negligible Correlation
cor(beijingData$PM10,beijingData$RAIN, method = "pearson")  #Negligible Correlation
cor(beijingData$PM10,beijingData$WSPM, method = "pearson")  #Negligible Correlation

#Plot Moderate to Very High Correlations

#PM2.5 x PM10
ggscatter(beijingData, x="PM10", y="PM2.5",
          shape = 20, color = "PM10",
          add="reg.line",
          add.params = list(color="blue", fill="gray"),
          conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "PM10 Concentration(ug/m³)", ylab = "PM2.5 Concentration(ug/m³)",
          )

#PM2.5 x Toxic Gases
ggscatter(beijingData, x="PM2.5", y="NO2",
          shape = 20, color = "PM2.5",
          add="reg.line",
          add.params = list(color="blue", fill="gray"),
          conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "PM2.5 Concentration(ug/m³)", ylab = "NO2 Concentration(ug/m³)",
)

ggscatter(beijingData, x="PM2.5", y="CO",
          shape = 20, color = "PM2.5",
          add="reg.line",
          add.params = list(color="blue", fill="gray"),
          conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "PM2.5 Concentration(ug/m³)", ylab = "CO Concentration(ug/m³)",
)

#PM10 x Toxic Gases
ggscatter(beijingData, x="PM10", y="NO2",
          shape = 20, color = "PM10",
          add="reg.line",
          add.params = list(color="blue", fill="gray"),
          conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "PM10 Concentration(ug/m³)", ylab = "NO2 Concentration(ug/m³)",
)

ggscatter(beijingData, x="PM10", y="CO",
          shape = 20, color = "PM10",
          add="reg.line",
          add.params = list(color="blue", fill="gray"),
          conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "PM10 Concentration(ug/m³)", ylab = "CO Concentration(ug/m³)",
)

#Concentration x Year
ggplot(beijingData, aes(x=year, y=PM10)) + 
  geom_boxplot(color="blue", fill="lightblue", alpha=0.4) +
  labs(x="Year",y="PM10 Concentration (ug/m³)")

ggplot(beijingData, aes(x=year, y=PM2.5)) + 
  geom_boxplot(color="blue", fill="lightblue", alpha=0.4) +
  labs(x="Year",y="PM2.5 Concentration (ug/m³)")

beijingData %>% 
  count(year, AQI10) %>%  
  ggplot(mapping = aes(x = year, y = AQI10)) +
  geom_tile(mapping = aes(fill = n)) +
  labs(x="Year",y="AQI Index - PM10")

beijingData %>% 
  count(year, AQI25) %>%  
  ggplot(mapping = aes(x = year, y = AQI25)) +
  geom_tile(mapping = aes(fill = n)) +
  labs(x="Year",y="AQI Index - PM2.5")

filter(beijingData, beijingData$month == 1) %>%
  count(year, AQI10) %>%  
  ggplot(mapping = aes(x = year, y = AQI10)) +
  geom_tile(mapping = aes(fill = n)) +
  labs(x="January of each year",y="AQI Index - PM10")

filter(beijingData, beijingData$month != 1 & beijingData$month != 2) %>% 
  count(year, AQI25) %>%  
  ggplot(mapping = aes(x = year, y = AQI25)) +
  geom_tile(mapping = aes(fill = n)) +
  labs(x="March to December for each year",y="AQI Index - PM25")

#PM x Month
p <- ggplot(data = beijingData, mapping = aes(x = month, y = PM10)) + 
  geom_point(mapping = aes(color=AQI10),position = "jitter") + 
  facet_wrap(~year, nrow=1) +
  labs(x="Month",y="PM10 Concentration (ug/m³)")
p + scale_color_brewer(palette ="Blues")


p <- ggplot(data = beijingData, mapping = aes(x = month, y = PM2.5)) + 
  geom_point(mapping = aes(color=AQI25),position = "jitter") + 
  facet_wrap(~year, nrow=1) +
  labs(x="Month",y="PM2.5 Concentration (ug/m³)")
p + scale_color_brewer(palette ="Blues")

#Temperature x Month
ggplot(beijingData, aes(x=month, y=TEMP)) + 
  geom_boxplot(color="blue", fill="lightblue", alpha=0.4) +
  labs(x="Month",y="Temperature (Cº)")

#Prevailing Wind Direction
ggplot(data = beijingData, mapping = aes(x = wd)) + 
  geom_bar(position = 'identity', fill = "navy", alpha=.7) +
  labs(x="Wind Direction", y="")

ggplot(data = beijingData, mapping = aes(x = wd)) + 
  geom_bar(position = 'identity', fill = "navy", alpha=.7) +
  facet_wrap(~month, nrow = 6) +
  labs(x="Wind Direction", y="")

#Wind Direction x AQI
beijingData %>%
  count(wd, AQI10) %>%  
  ggplot(mapping = aes(x = wd, y = AQI10)) +
  geom_tile(mapping = aes(fill = n)) +
  labs(x="Wind Direction",y="AQI Index - PM10")

#Wind Direction in Hazardous Situations
ggplot(data = filter(beijingData, beijingData$AQI10==4 | beijingData$AQI10==5 | beijingData$AQI10==6), 
      mapping = aes(x = wd)) + 
      geom_bar(position = 'identity', fill = "navy", alpha=.7) +
      labs(x="Wind Direction in Hazardous Situations", y="")

beijingData %>%
  count(wd, AQI25) %>%  
  ggplot(mapping = aes(x = wd, y = AQI25)) +
  geom_tile(mapping = aes(fill = n)) +
  labs(x="Wind Direction",y="AQI Index - PM2.5")

filter(beijingData, beijingData$AQI10==4 | beijingData$AQI10==5 | beijingData$AQI10==6) %>%
  count(wd, AQI10) %>%  
  ggplot(mapping = aes(x = wd, y = AQI10)) +
  geom_tile(mapping = aes(fill = n)) +
  labs(x="Wind Direction",y="AQI Index - PM10")

filter(beijingData, beijingData$AQI10==5 | beijingData$AQI10==6) %>%
  count(wd, AQI10) %>%  
  ggplot(mapping = aes(x = wd, y = AQI10)) +
  geom_tile(mapping = aes(fill = n)) +
  labs(x="January of each year",y="AQI Index - PM10")

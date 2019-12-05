
# BEIJING MULTI-SITE AIR-QUALITY ANALYSIS
# CAIO CESAR WASEM
# 25/11/2019

install.packages("ggpubr")

library(readr)
library(plyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(ggpubr)

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
beijingData$AQI10[beijingData$PM2.5<=50] <- 1
beijingData$AQI10[(beijingData$PM2.5>50)&(beijingData$PM2.5<=100)] <-2
beijingData$AQI10[(beijingData$PM2.5>100)&(beijingData$PM2.5<=250)] <-3
beijingData$AQI10[(beijingData$PM2.5>250)&(beijingData$PM2.5<=350)] <-4
beijingData$AQI10[(beijingData$PM2.5>350)&(beijingData$PM2.5<=430)] <-5
beijingData$AQI10[beijingData$PM2.5>430] <- 6

#Change variables to factors
beijingData$wd <- as.factor(beijingData$wd)
beijingData$station <- as.factor(beijingData$station)
beijingData$AQI10 <- as.factor(beijingData$AQI10)
beijingData$AQI25 <- as.factor(beijingData$AQI25)
beijingData$year <- as.factor(beijingData$year)
beijingData$month <- as.factor(beijingData$month)

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

# TESTE
# ggscatter(beijingData[1:2000,], x="PM10", y="PM2.5",
#           shape = 20, color = "PM10",
#           add="reg.line",
#           add.params = list(color="blue", fill="gray"),
#           conf.int = TRUE,
#           cor.coef = TRUE, cor.method = "pearson",
#           xlab = "PM10 Concentration", ylab = "PM2.5 Concentration",
#           )

#Plot Moderate to Very High Correlations

#PM2.5 x PM10
ggscatter(beijingData, x="PM2.5", y="PM10",
          shape = 20, color = "PM2.5",
          add="reg.line",
          add.params = list(color="blue", fill="gray"),
          conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "PM2.5 Concentration(ug/m³)", ylab = "PM10 Concentration(ug/m³)",
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
          xlab = "PM2.5 Concentration(ug/m³)", ylab = "NO2 Concentration(ug/m³)",
)

ggscatter(beijingData, x="PM10", y="CO",
          shape = 20, color = "PM10",
          add="reg.line",
          add.params = list(color="blue", fill="gray"),
          conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "PM2.5 Concentration(ug/m³)", ylab = "CO Concentration(ug/m³)",
)

ggscatter(filter(beijingData, beijingData$SO2<200), x="SO2", y="PM2.5", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(filter(beijingData, beijingData$SO2<200), x="SO2", y="PM10", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(filter(beijingData, beijingData$NO2<250), x="NO2", y="PM2.5", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(filter(beijingData, beijingData$NO2<250), x="NO2", y="PM10", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(beijingData, x="CO", y="PM2.5", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(beijingData, x="CO", y="PM10", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(filter(beijingData, beijingData$O3<450), x="O3", y="PM2.5", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(filter(beijingData, beijingData$O3<450), x="O3", y="PM10", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

#PM x Temperatura e Pressão
ggscatter(beijingData, x="TEMP", y="PM2.5", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(beijingData, x="TEMP", y="PM10", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(beijingData, x="PRES", y="PM2.5", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(beijingData, x="PRES", y="PM10", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

#PM x Orvalho
ggscatter(beijingData, x="DEWP", y="PM2.5", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(beijingData, x="DEWP", y="PM10", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")
#PM x Precipitação
ggscatter(filter(beijingData, beijingData$RAIN>0), x="RAIN", y="PM2.5", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(filter(beijingData, beijingData$RAIN>0), x="RAIN", y="PM10", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

#PM x Velocidade do Vento
ggscatter(filter(beijingData, beijingData$WSPM<10), x="WSPM", y="PM2.5", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")
ggscatter(filter(beijingData, beijingData$WSPM<10), x="WSPM", y="PM10", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

#AQI x Direção do Vento
ggplot(data = beijingData, mapping = aes(x = AQI10, y=..prop..,group=1)) + 
  geom_bar(position = "identity") +
  facet_wrap(~wd)

ggplot(data = beijingData, mapping = aes(x = AQI25, y=..prop..,group=1)) + 
  geom_bar(position = "identity") +
  facet_wrap(~wd)

#Direção do Vento Predominante 
ggplot(data = beijingData, mapping = aes(x = wd)) + 
  geom_bar(position = 'identity')

#Direção do Vento Predominante a cada mês
ggplot(data = beijingData, mapping = aes(x = wd)) + 
  geom_bar(position = 'identity') +
  facet_wrap(~month)

#PM x year
ggplot(data = beijingData, mapping = aes(x = year, y = PM2.5)) + 
  geom_point(position = "jitter")

#PM x month
ggplot(data = beijingData, mapping = aes(x = month, y = PM2.5)) + 
  geom_point(mapping = aes(color=AQI25),position = "jitter") + 
  facet_wrap(~year, nrow=1)

#PM x hour
ggplot(data = beijingData, mapping = aes(x = hour, y = PM2.5)) + 
  geom_point(position = "jitter")+
  geom_smooth()

ggplot(data = beijingData, mapping = aes(x = hour, y = PM10)) + 
  geom_point(position = "jitter")+
  geom_smooth()

#AQI x Ano
ggplot(data = beijingData)+
  geom_bar(mapping = aes(x=AQI10, y=..prop..,group=1))+
  facet_wrap(~year, nrow=1)

ggplot(data = beijingData)+
  geom_bar(mapping = aes(x=AQI25, y=..prop..,group=1))+
  facet_wrap(~year, nrow=1)

#AQI x Distrito
ggplot(data = beijingData)+
  geom_bar(mapping = aes(x=AQI25, y=..prop..,group=1))+
  facet_wrap(~station, nrow=3)

ggplot(data = beijingData)+
  geom_bar(mapping = aes(x=AQI10, y=..prop..,group=1))+
  facet_wrap(~station, nrow=3)

#AQI x Mês
ggplot(data = beijingData)+
  geom_bar(mapping = aes(x=AQI10, y=..prop..,group=1))+
  facet_wrap(~month, nrow=3)

ggplot(data = beijingData)+
  geom_bar(mapping = aes(x=AQI25, y=..prop..,group=1))+
  facet_wrap(~month, nrow=3)

#Temperatura x Mês
ggplot(data = beijingData) + 
  stat_summary(
    mapping = aes(x = month, y = TEMP),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

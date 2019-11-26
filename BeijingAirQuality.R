
# BEIJING MULTI-SITE AIR-QUALITY ANALYSIS
# CAIO CESAR WASEM
# 25/11/2019

install.packages("ggpubr") #Para teste de correlação

library(readr)
library(plyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(ggpubr)

#Definir diretório com as tabelas
setwd("F:\\Documents\\BeijingAirQuality")

#Importar base de dados
files <- list.files(pattern = "*.csv", full.names = TRUE)
dataBeijing <- ldply(files, read_csv)

#Verificar variáveis do dataframe
str(dataBeijing)

#Função para deletar NA
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}

#Remover NA (De 420768 linhas para 382168)
dataBeijing <- delete.na(dataBeijing)

#Separar em níveis de perigo (n1 é o melhor, n6 é o pior)
# pm25.n1 <- dataBeijing[which(dataBeijing$PM2.5<=30),]
# pm25.n2 <- dataBeijing[which((dataBeijing$PM2.5>30)&(dataBeijing$PM2.5<=60)),]
# pm25.n3 <- dataBeijing[which((dataBeijing$PM2.5>60)&(dataBeijing$PM2.5<=90)),]
# pm25.n4 <- dataBeijing[which((dataBeijing$PM2.5>90)&(dataBeijing$PM2.5<120)),]
# pm25.n5 <- dataBeijing[which((dataBeijing$PM2.5>120)&(dataBeijing$PM2.5<=250)),]
# pm25.n6 <- dataBeijing[which(dataBeijing$PM2.5>250),]
# 
# pm10.n1 <- dataBeijing[which(dataBeijing$PM10<=50),]
# pm10.n2 <- dataBeijing[which((dataBeijing$PM10>50)&(dataBeijing$PM10<=100)),]
# pm10.n3 <- dataBeijing[which((dataBeijing$PM10>100)&(dataBeijing$PM10<=250)),]
# pm10.n4 <- dataBeijing[which((dataBeijing$PM10>250)&(dataBeijing$PM10<=350)),]
# pm10.n5 <- dataBeijing[which((dataBeijing$PM10>350)&(dataBeijing$PM10<=430)),]
# pm10.n6 <- dataBeijing[which(dataBeijing$PM10>430),]

#Criar colunas com o nível de perigo para cada particulado
dataBeijing["AQI25"] <- NA
dataBeijing$AQI25[dataBeijing$PM2.5<=30] <- 1
dataBeijing$AQI25[(dataBeijing$PM2.5>30)&(dataBeijing$PM2.5<=60)] <-2
dataBeijing$AQI25[(dataBeijing$PM2.5>60)&(dataBeijing$PM2.5<=90)] <-3
dataBeijing$AQI25[(dataBeijing$PM2.5>90)&(dataBeijing$PM2.5<=120)] <-4
dataBeijing$AQI25[(dataBeijing$PM2.5>120)&(dataBeijing$PM2.5<=250)] <-5
dataBeijing$AQI25[dataBeijing$PM2.5>250] <- 6

dataBeijing["AQI10"] <- NA
dataBeijing$AQI10[dataBeijing$PM2.5<=50] <- 1
dataBeijing$AQI10[(dataBeijing$PM2.5>50)&(dataBeijing$PM2.5<=100)] <-2
dataBeijing$AQI10[(dataBeijing$PM2.5>100)&(dataBeijing$PM2.5<=250)] <-3
dataBeijing$AQI10[(dataBeijing$PM2.5>250)&(dataBeijing$PM2.5<=350)] <-4
dataBeijing$AQI10[(dataBeijing$PM2.5>350)&(dataBeijing$PM2.5<=430)] <-5
dataBeijing$AQI10[dataBeijing$PM2.5>430] <- 6

#Transformar variáveis para factors
dataBeijing$wd <- as.factor(dataBeijing$wd)
dataBeijing$station <- as.factor(dataBeijing$station)
dataBeijing$AQI10 <- as.factor(dataBeijing$AQI10)
dataBeijing$AQI25 <- as.factor(dataBeijing$AQI25)
dataBeijing$year <- as.factor(dataBeijing$year)
dataBeijing$month <- as.factor(dataBeijing$month)

#PM x PM
ggscatter(dataBeijing, x="PM10", y="PM2.5", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

#PM x Gases

ggscatter(filter(dataBeijing, dataBeijing$SO2<200), x="SO2", y="PM2.5", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(filter(dataBeijing, dataBeijing$SO2<200), x="SO2", y="PM10", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(filter(dataBeijing, dataBeijing$NO2<250), x="NO2", y="PM2.5", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(filter(dataBeijing, dataBeijing$NO2<250), x="NO2", y="PM10", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(dataBeijing, x="CO", y="PM2.5", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(dataBeijing, x="CO", y="PM10", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(filter(dataBeijing, dataBeijing$O3<450), x="O3", y="PM2.5", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(filter(dataBeijing, dataBeijing$O3<450), x="O3", y="PM10", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

#PM x Temperatura e Pressão
ggscatter(dataBeijing, x="TEMP", y="PM2.5", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(dataBeijing, x="TEMP", y="PM10", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(dataBeijing, x="PRES", y="PM2.5", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(dataBeijing, x="PRES", y="PM10", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

#PM x Orvalho
ggscatter(dataBeijing, x="DEWP", y="PM2.5", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(dataBeijing, x="DEWP", y="PM10", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")
#PM x Precipitação
ggscatter(filter(dataBeijing, dataBeijing$RAIN>0), x="RAIN", y="PM2.5", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(filter(dataBeijing, dataBeijing$RAIN>0), x="RAIN", y="PM10", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

#PM x Velocidade do Vento
ggscatter(filter(dataBeijing, dataBeijing$WSPM<10), x="WSPM", y="PM2.5", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")
ggscatter(filter(dataBeijing, dataBeijing$WSPM<10), x="WSPM", y="PM10", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

#AQI x Direção do Vento
ggplot(data = dataBeijing, mapping = aes(x = AQI10, y=..prop..,group=1)) + 
  geom_bar(position = "identity") +
  facet_wrap(~wd)

ggplot(data = dataBeijing, mapping = aes(x = AQI25, y=..prop..,group=1)) + 
  geom_bar(position = "identity") +
  facet_wrap(~wd)

#PM x year
ggplot(data = dataBeijing, mapping = aes(x = year, y = PM2.5)) + 
  geom_point(position = "jitter")

#PM x month
ggplot(data = dataBeijing, mapping = aes(x = month, y = PM2.5)) + 
  geom_point(mapping = aes(color=AQI25),position = "jitter") + 
  facet_wrap(~year, nrow=1)

#PM x hour
ggplot(data = dataBeijing, mapping = aes(x = hour, y = PM2.5)) + 
  geom_point(position = "jitter")+
  geom_smooth()

ggplot(data = dataBeijing, mapping = aes(x = hour, y = PM10)) + 
  geom_point(position = "jitter")+
  geom_smooth()

#AQI x Ano
ggplot(data = dataBeijing)+
  geom_bar(mapping = aes(x=AQI10, y=..prop..,group=1))+
  facet_wrap(~year, nrow=1)

ggplot(data = dataBeijing)+
  geom_bar(mapping = aes(x=AQI25, y=..prop..,group=1))+
  facet_wrap(~year, nrow=1)

#AQI x Distrito
ggplot(data = dataBeijing)+
  geom_bar(mapping = aes(x=AQI25, y=..prop..,group=1))+
  facet_wrap(~station, nrow=3)

ggplot(data = dataBeijing)+
  geom_bar(mapping = aes(x=AQI10, y=..prop..,group=1))+
  facet_wrap(~station, nrow=3)

#AQI x Mês
ggplot(data = dataBeijing)+
  geom_bar(mapping = aes(x=AQI10, y=..prop..,group=1))+
  facet_wrap(~month, nrow=3)

ggplot(data = dataBeijing)+
  geom_bar(mapping = aes(x=AQI25, y=..prop..,group=1))+
  facet_wrap(~month, nrow=3)

#Temperatura x Mês
ggplot(data = dataBeijing) + 
  stat_summary(
    mapping = aes(x = month, y = TEMP),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

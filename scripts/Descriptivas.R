#=========================================================================================================#
#                             Universidad de los Andes - Colombia
#                              Big Data and ML for Applied Economics
#                                 Proyecto: Predicting Dengue spread  
#                                 by: Nicolás Velásquez                               
#=========================================================================================================#

#=========================================================#
# Script creado por: Nicolás Velásquez                    #
# Documentación y soporte: juan.velasquez@uniandes.edu.co #
#=========================================================#

#================#
#### Paquetes ####
#================#
rm(list = ls())

library(skimr)
library(ggcorrplot)
library(stargazer)
library(boot)
library(tidyverse)   # Manejo de datos y visualización
library(skimr)       # Descriptivas
library(dplyr)
library(ggplot2)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(lubridate)
library(recolorize)

setwd(r'(C:\Users\juan.velasquez\OneDrive - Universidad de los Andes\Maestria\Semestres\2022-2\BIG DATA & MACHINE LEARNING FOR APPLIED ECONOMICS\Trabajo\Predicting-dengue-spread\Dashboard)')

#Cargar los datos 
dengue_labels_test = read.csv("stores/predictions.csv")
dengue_features_test = read.csv("stores/dengue_features_test.csv")
dengue_labels_train = read.csv("stores/dengue_labels_train.csv")
dengue_features_train = read.csv("stores/dengue_features_train.csv")
# merge two data frames by ID and Country
#Ojo el inicio y final de fechas para pred y train de IQ y SJ son distintos. El selector debe tener esto en cuenta. 
total_pred <- merge(dengue_labels_test,dengue_features_test,by=c("city","year","weekofyear"))
total_pred$ref= "Prediction"
total_train <- merge(dengue_labels_train,dengue_features_train,by=c("city","year","weekofyear"))
total_train$ref= "Real"
total_train$lower.bound = NA
total_train$upper.bound = NA
total_train$margin = NA
total<-rbind(total_train,total_pred)
# Since my time is currently a factor, I have to convert it to a date-time format!
total$week_start_date <- ymd(total$week_start_date)
#Cahnge SJ and IQ for real names
total <-  total %>% dplyr::mutate(
  city = dplyr::if_else(
    city == "iq", "Iquitos", "San Juan"
  )
)

total_iq <- total_train %>% filter(city == 'iq')
total_sj <- total_train %>% filter(city == 'sj')
skim(total_iq)

# Check correlations (as scatterplots), distribution and print corrleation coefficient

data_plot <- total_iq %>% select(-c("city", "ref","week_start_date","lower.bound","upper.bound","margin"))
data_plot <- na.omit(data_plot)
corr <- round(cor(data_plot), 1)

ggcorrplot(corr, method = "circle", type = "lower", lab = TRUE) +
  ggtitle("Correlograma de base de Dengue Iquitos") +
  theme_minimal() +
  labs(y='Variables')+
  theme(legend.position = "none")

data_plot_sj <- total_sj %>% select(-c("city", "ref","week_start_date","lower.bound","upper.bound","margin"))
data_plot_sj <- na.omit(data_plot_sj)
corr_sj <- round(cor(data_plot_sj), 1)

ggcorrplot(corr_sj, method = "circle", type = "lower", lab = TRUE) +
  ggtitle("Correlograma de base de Dengue San Juan") +
  theme_minimal()+
  labs(y='Variables')+
  theme(legend.position = "none")

############################## Histogramas y gráficos de barras##################

ggplot(total_iq, aes(x = total_cases)) +
  geom_histogram(bins = 20, fill = "darkorange") +
  ggtitle("Casos Totales Dengue Iquitos") +
  labs(x = "Casos Totales", y = "Cantidad") +
  theme_bw()


ggplot(total_sj, aes(x = total_cases)) +
  geom_histogram(bins = 20, fill = "darkred") +
  ggtitle("Casos Totales San Juan") +
  labs(x = "Casos Totales", y = "Cantidad") +
  theme_bw()

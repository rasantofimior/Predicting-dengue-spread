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

library(shiny)       # Para poder hacer la aplicación
library(shinythemes) # Para acceder a temas predeterminados
library(readxl)      # Lectura de archivos de Excel
library(tidyverse)   # Manejo de datos y visualización
library(plotly)      # Visualización interactiva
library(agricolae)   # Tablas de frecuencias agrupadas
library(skimr)       # Descriptivas
library(dplyr)
library(ggplot2)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(lubridate)
library(recolorize)

#=================#
#### Preámbulo ####
#=================#
#setwd(r'(C:\Users\juan.velasquez\OneDrive - Universidad de los Andes\Maestria\Semestres\2022-2\BIG DATA & MACHINE LEARNING FOR APPLIED ECONOMICS\Trabajo\Predicting-dengue-spread\Dashboard)')

#Cargar los datos 
dengue_labels_test = read.csv("stores/predictions.csv")
dengue_features_test = read.csv("stores/dengue_features_test.csv")
dengue_labels_train = read.csv("stores/dengue_labels_train.csv")
dengue_features_train = read.csv("stores/dengue_features_train.csv")
# merge two data frames by ID and Country
#Ojo el inicio y final de fechas para pred y train de IQ y SJ son distintos. El selector debe tener esto en cuenta. 
total_pred <- merge(dengue_labels_test,dengue_features_test,by=c("city","year","weekofyear"))
total_pred$data= "Prediction"
total_train <- merge(dengue_labels_train,dengue_features_train,by=c("city","year","weekofyear"))
total_train$data= "Real"
total_train$lower.bound = total_train$total_cases
total_train$upper.bound = total_train$total_cases
total_train$margin = total_train$total_cases
total<-rbind(total_train,total_pred)
# Since my time is currently a factor, I have to convert it to a date-time format!
total$week_start_date <- ymd(total$week_start_date)
#Cahnge SJ and IQ for real names
total <-  total %>% dplyr::mutate(
  city = dplyr::if_else(
    city == "iq", "Iquitos", "San Juan"
  )
)
#=================#
#### Shiny App ####
#=================#

ui <- fluidPage(
  # Definir tema
  theme = shinytheme('united'),
  
  # Encabezado
  fluidRow( column(1, tags$img(src = 'mosco_plus.png',
                               width = '80px', 
                               height = '50px')),
           column(8, h1(strong('Forcast Dashboard'))),
           column(1, tags$img(src = 'Uniandes.png',
                              width = '200px', 
                              height = '90px'))),
  
  
  # Título de la página
  headerPanel('Predicting Dengue Spread using Machine Learning'),
  
  sidebarLayout(
    sidebarPanel(
      # Panel 1
        # Widget desplegable para seleccionar la ciudad
      #OJO Dejar las ciudadde con nombre completo
        selectInput(inputId = 'city_selected',
                    label = '1. Please select city',
                    choices = unique(total$city),
                    selected = 'Total'
                    ),
        # Widget Date range
        dateInput(inputId = "start_date", 
                  label= "2. Please select start date", 
                  value = "1990-04-20" 
                  ),
        
        dateInput(inputId = "end_date", 
                  label= "3. Please select end date", 
                  value = "2013-06-25" 
                  ),
        #Options for calculations
       # div(id = 'all',
        #radioButtons('calculate',
        #             label = '4. Calculation',
        #             choices = c('No', 
        #                         'Calculate  Predictions'),
        #             selected = 'No'),
        
    #), # Close div() function
    
    ###### Reset button ######
    submitButton('Apply Changes'),
    #actionButton('resetAll', 
    #             label = 'Click to Reset Options'),
    
    
    ###### Loading message appears ######
    conditionalPanel(condition = '$("html").hasClass("shiny-busy")',
                     tags$div(id = 'loadmessage',
                              'Calculating...')
    ), # Close conditional panel for loading message
    p("Designed and built by: Camilo Bonilla, Rafael Santofimio and Nicolás Velásquez.")
    ),# Close SidebarPanel  depende del selector de la ciudad
    mainPanel( 
        plotlyOutput('serie_grafico'),
        p("Note: For the use of the forecast panel, 
          choose a city between San Juan and Iquitos and a time period (start date and end date),finally click the button Apply Changes. 
          These cities have different forecast breackpoints 09-07-2010 & 29-04-2008 respectively, 
          so the graph will display it accordingly. Predictions are presented with 95% confidence interval.")
    )# Close MainPanel
  )# Close SideBarlayout
)# Close fluidPage


# data %>% ggplot(aes(y = predict_mod_peak_female$fit, x = age, color = female)) +
#   geom_point() +
#   labs(
#     x = "Edad",
#     y = "Log Salario mensual predicho",
#     title = "log Salarios mensual vs. Edad (95% IC)"
#   ) +
#   geom_ribbon(
#     aes(ymin = predict_mod_peak_female$lwr, ymax = predict_mod_peak_female$upr),
#     alpha = 0.2
#   )
# 

server = function(input, output) {
  output$txtout <- renderText({
    paste(input$txt, input$slider, format(input$week_start_date), sep = ", ")
  })
  output$table <- renderTable({
    head(cars, 4)
  })
  
  # output$mapa_grafico =  renderImage(
  #   ## Map graph
  #   
  #   if (input$city_selected == 'Iquitos') {
  #     img <- readImage("www/iquitos.png", resize = NULL, rotate = NULL)
  #     plotImageArray(img)
  #   }
  #   else {
  #     img <- readImage("san_juan.png", resize = NULL, rotate = NULL)
  #     plotImageArray(img)
  #   } 
  # )
  #   
  output$serie_grafico= renderPlotly({
  ## Serie graph
    total_iq <- total %>% filter(city == 'Iquitos'& week_start_date >= input$start_date & week_start_date <= input$end_date)
    total_sj <- total %>% filter(city == 'San Juan'& week_start_date >= input$start_date & week_start_date <= input$end_date)
  # By city
  if (input$city_selected == 'Iquitos') {
    graph = reactive({
     ggplot(data = total_iq,aes(y = total_cases, x = week_start_date, color = data)) +
        geom_line() +
        labs(
          x = "Fecha",
          y = "Total Cases Iquitos, Perú(95% IC)"
        ) +
        geom_ribbon(
          aes(ymin = lower.bound, ymax = upper.bound),
          alpha = 0.2
        )
    })
  } # Close conditional clause
  # By sj
  else {
    graph = reactive({
      ggplot(data = total_sj,aes(y = total_cases, x = week_start_date, color = data)) +
        geom_line() +
        labs(
          x = "Fecha",
          y = "Total Cases San Juan, Puerto Rico(95% IC)"
        ) +
        geom_ribbon(
          aes(ymin = lower.bound, ymax = upper.bound),
          alpha = 0.2
        )
    })
  } # Close else clause

  ggplotly(graph()) %>% layout(title = list(text = 'Prediction Dengue Cases'))

  }) # Close renderPlotly for residuals graph
}

shinyApp(ui, server)
  
  
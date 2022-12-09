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
#=================#
#### Preámbulo ####
#=================#
setwd(r'(C:\Users\juan.velasquez\OneDrive - Universidad de los Andes\Maestria\Semestres\2022-2\BIG DATA & MACHINE LEARNING FOR APPLIED ECONOMICS\Trabajo\Predicting-dengue-spread\Dashboard)')

#Cargar los datos OJO->(cambiar predictions por el real)
dengue_labels_test = read.csv("stores/test_format.csv")
dengue_features_test = read.csv("stores/dengue_features_test.csv")
dengue_labels_train = read.csv("stores/dengue_labels_train.csv")
dengue_features_train = read.csv("stores/dengue_features_train.csv")
# merge two data frames by ID and Country
#Ojo el inicio y final de fechas para pred y train de IQ y SJ son distintos. El selector debe tener esto en cuenta. 
total_pred <- merge(dengue_labels_test,dengue_features_test,by=c("city","year", "weekofyear"))
total_pred$ref= "Prediction"
total_train <- merge(dengue_labels_train,dengue_features_train,by=c("city","year", "weekofyear"))
total_train$ref= "Real"
total<-rbind(total_train,total_pred)
# Since my time is currently a factor, I have to convert it to a date-time format!
total$week_start_date <- ymd(total$week_start_date)
# Variables numéricas
#total_num = total %>%
#  select(where(is.numeric))
# Variables Region and date
#total_cat= total %>%
#  select(where(is.character))

#=================#
#### Shiny App ####
#=================#

ui <- fluidPage(
  # Definir tema
  theme = shinytheme('united'),
  
  # Encabezado
  fluidRow(column(1, tags$img(src = 'mosco.png',
                              width = '120px', 
                              height = '70px')),
           column(8, h1(strong('Big Data and ML Dashboard')))),
  
  
  # Título de la página
  headerPanel('Predicting Dengue Spread'),
  
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
        div(id = 'all',
        radioButtons('calculate',
                     label = '4. Calculation',
                     choices = c('No', 
                                 'Calculate  Predictions'),
                     selected = 'No'),
        
    ), # Close div() function
    
    ###### Reset button ######
    actionButton('resetAll', 
                 label = 'Click to Reset Options'),
    
    
    ###### Loading message appears ######
    conditionalPanel(condition = '$("html").hasClass("shiny-busy")',
                     tags$div(id = 'loadmessage',
                              'Calculating...')
    ), # Close conditional panel for loading message
    ),# Close SidebarPanel  depende del selector de la ciudad
    mainPanel(
      splitLayout(
        plotlyOutput('mapa_grafico'),
        plotlyOutput('serie_grafico')
      )
    )# Close MainPanel
  )# Close SideBarlayout
)# Close fluidPage

server = function(input, output) {
  output$txtout <- renderText({
    paste(input$txt, input$slider, format(input$week_start_date), sep = ", ")
  })
  output$table <- renderTable({
    head(cars, 4)
  })
  
  output$mapa_grafico <-  renderImage({
    ## Map graph
    if (input$city_selected != 'iq') {
      img(
        src = "iquitos.png",
        alt = "Iquitos Map",
        width = 100, height = 100
      )
    }
    else {
      img(
        src = "san_juan.png",
        alt = "An Juan Map",
        width = 100, height = 100
      )
    }
  },deleteFile=TRUE
  )
    
    
  output$serie_grafico= renderPlotly({
  ## Serie graph

  # By city
  if (input$city_selected != 'iq') {
    total_iq <- subset(total,
                       city = "iq" & week_start_date >= input$start_date & week_start_date <= input$end_date,
                       select = c(total_cases,week_start_date,ref))
    graph = reactive({
      ggplot(data = total_iq, aes(x=week_start_date, y=total_cases, color = ref)) +
        geom_area(fill="#69b3a2", alpha=0.5) +
        geom_line(color="#69b3a2") +
        ylab("Casos Totales de Dengue Iquitos, Perú(95% IC)") +
        theme_ipsum()
    })
  } # Close conditional clause
  # By sj
  else {
    total_sj <- subset(total,
                       city = "sj" & week_start_date >= input$start_date & week_start_date <= input$end_date,
                       select = c(total_cases,week_start_date,ref))
    graph = reactive({
      ggplot(data = total_sj, aes(x=week_start_date, y=total_cases, color = ref)) +
        geom_area(fill="#69b3a2", alpha=0.5) +
        geom_line(color="#69b3a2") +
        ylab("Casos Totales de Dengue San Juan, Puerto Rico(95% IC)") +
        theme_ipsum()
    })
  } # Close else clause

  ggplotly(graph()) %>% layout(title = list(text = 'Predition Total Dengue Cases'))

  }) # Close renderPlotly for residuals graph
}

shinyApp(ui, server)
  
  
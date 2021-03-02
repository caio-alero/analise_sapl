library(shiny)
library(shinydashboard)
library(dplyr)
library(reshape2)
library(DT)
library(wesanderson)
library(echarts4r)
#library(echarts4r.maps)
#library(apexcharter)
#library(highcharter)
#library(shinycssloaders)
library(shinyWidgets)
library(countup)
library(waiter)
library(sever)


source('ui.R', local = TRUE, encoding = 'UTF-8')
source('server.R', encoding = 'UTF-8')
source('valuebox_functions.R', encoding = 'UTF-8')

# run app 
shinyApp(ui, server)


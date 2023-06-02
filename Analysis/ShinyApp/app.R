###################
### BIBLIOTECAS ###
###################

library(tidyverse)
library(readxl)
library(reshape2)
library(ggpubr)
library(tm)
library(plotly)
library(scales)
library(shiny)
library(shinydashboard)
library(shinyWidgets)

################
### DATASETS ###
################

# rm(list = ls())

base_seguros <- read.csv2("~/Documents/GitHub/Mercado-Seguros/Dataset/base_seguros.csv",
                          fileEncoding = "latin1")

###############
### OPTIONS ###
###############

cor_graficos <- "#00688B"

###########
### APP ###
###########

header <- dashboardHeader(title = "SES - SUSEP")

# USER INTERFACE # 

source("~/Documents/GitHub/Mercado-Seguros/R/ShinyApp/ui_sidebar.R")
source("~/Documents/GitHub/Mercado-Seguros/R/ShinyApp/ui_body.R")

ui <- 
  dashboardPage(header = header,
                sidebar = sidebar,
                body= body)

# SERVER #

server <- shinyServer(function(input, output) {
  
} 
)


# APP #

shinyApp(ui, server)


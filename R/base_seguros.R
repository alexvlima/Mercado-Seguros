###################
### BIBLIOTECAS ###
###################

library(tidyverse)

################
### DATASETS ###
################

# rm(list = ls())

# Arquivo com o histórico dos dados de seguros #
seguros <- 
  read.csv2("ses_seguros.csv",
            sep = ";", dec = ",", header = T,
            fileEncoding = "latin1")

# Arquivo com os ramos #
ramos <- 
  read.csv2("ses_ramos.csv", 
  sep = ";", dec = ",", header = T, 
  fileEncoding = "latin1")

# Arquivo de entidades #
entidade <- 
  read.csv2("ses_grupos_economicos.csv", 
  sep = ";", dec = ",", header = T,
  fileEncoding = "latin1")

# Arquivo com o grupo de cada ramo #
gr_ramo <- 
  read.csv2("ses_gruposramos.csv", 
  sep = ";", dec = ",", header = T,
  fileEncoding = "latin1")

###############
### AJUSTES ###
###############

# Reduzindo a base para dados a partir de 2015 #
seguros <- 
  seguros %>%
  filter(damesano > 201500)

base <- 
  seguros %>%
  left_join(entidade, by = c("coenti" = "coenti", 
                             "damesano" = "damesano", 
                             "cogrupo" = "cogrupo")) %>%
  left_join(ramos, by = "coramo")

rm(seguros, entidade, ramos)


base$GRAID <- as.numeric(substr(base$noramo, 1, 2))

glimpse(base)
glimpse(gr_ramo)

base %>%
  dplyr::select(noramo, GRAID) %>%
  print()



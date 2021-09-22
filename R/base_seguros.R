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

# Acrescenta as informações de entidade e ramo #
base <- 
  seguros %>%
  left_join(entidade, by = c("coenti" = "coenti", 
                             "damesano" = "damesano", 
                             "cogrupo" = "cogrupo")) %>%
  left_join(ramos, by = "coramo")

rm(seguros, entidade, ramos)

# Ajuste para pegar o grupo do ramo #
base$GRAID <- as.numeric(substr(base$noramo, 1, 2))

# Acrescenta a info do grupo do ramo #
base <- 
  base %>%
  left_join(gr_ramo, by = "GRAID") %>%
  mutate(gr_ramo = GRANOME) %>%
  dplyr::select(-GRAID, -GRANOME, -GRACODIGO)

rm(gr_ramo)

# Acrescenta o ano
base_seguros$ano <- base_seguros$damesano%/%100

##############
### EXPORT ###
##############

write.csv2(base, "~/Documents/GitHub/Mercado-Seguros/Dataset/base_seguros.csv",
           fileEncoding = "latin1", row.names = FALSE)

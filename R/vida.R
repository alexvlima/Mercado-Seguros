#################
### DIRETORIO ###
#################

getwd()
setwd("/Users/alexvlima/Library/Mobile Documents/com~apple~CloudDocs/CaixaSeguradora/Mercado_Seguros/BaseCompleta/")

##################
### BIBLIOTECA ###
##################

library(tidyverse)

###############
### DATASET ###
###############

rm(list = ls())

# pl <- read_csv2("Ses_pl_margem.csv")
ses_seguros <- read_csv2("Ses_seguros.csv")
ramos <- read_csv2("Ses_ramos.csv")
grupo <- read_csv2("Ses_grupos_economicos.csv")

# glimpse(pl)
# 
# grupo %>% print(n = 100)
# pl %>% filter(coenti == "05631")

############
### VIDA ###
############

flag <- str_detect(ramos$noramo, pattern = "Vida")
ramos[flag, ]
flag2 <- str_detect(ramos$noramo, pattern = "VIDA")
ramos[flag2, ]

df <-          
  ses_seguros %>% 
  filter(damesano > 201512, coramo %in% c("0991", "1391", "0996", "1396", "1198", "0993", "2293")) %>% 
  dplyr::select(damesano, coenti, cogrupo, coramo, premio_direto, premio_de_seguros, premio_ganho, premio_emitido2, desp_com)

grupo <- 
  grupo %>%
  filter(damesano > 201512)

df <- 
  df %>%
  left_join(grupo, by = c("coenti" = "coenti", "damesano" = "damesano", "cogrupo" = "cogrupo"))
  
glimpse(df)

vida <- 
  df %>%
  group_by(damesano, noenti, nogrupo) %>%
  summarize(premio_ganho = sum(premio_ganho), 
            premio_emitido = sum(premio_emitido2),
            despesa_comercial = sum(desp_com))

# prestamista <- 
#   df %>%
#   filter(coramo == "0977") %>% 
#   group_by(nogrupo) %>%
#   summarize(premio_de_seguros = sum(premio_de_seguros),
#             premio_direto = sum(premio_direto),
#             premio_ganho = sum(premio_ganho),
#             premio_emitido = sum(premio_emitido2),
#             despesa_comercial = sum(desp_com)) %>%
#   print(n = 70)

write_csv2(vida, "../premio_vida.csv")



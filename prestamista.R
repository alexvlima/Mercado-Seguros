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

###################
### PRESTAMISTA ###
###################

flag <- str_detect(ramos$noramo, pattern = "Prestamista")
ramos[flag, ]

df <-          
  ses_seguros %>% 
  filter(damesano > 202000, coramo %in% c("0977", "1377")) %>% 
  dplyr::select(damesano, coenti, cogrupo, coramo, premio_direto, premio_de_seguros, premio_ganho, premio_emitido2, desp_com)

grupo <- 
  grupo %>%
  filter(damesano> 202000)

df <- 
  df %>%
  left_join(grupo, by = c("coenti" = "coenti", "damesano" = "damesano", "cogrupo" = "cogrupo"))
  
glimpse(df)

prestamista <- 
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

write_csv2(prestamista, "../premio_prestamista.csv")

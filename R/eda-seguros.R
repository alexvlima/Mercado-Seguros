#################
### LIBRARIES ###
#################

library(tidyverse)

################
### DATASETS ###
################

# rm(list = ls())

base_seguros <- read.csv2("~/Documents/GitHub/Mercado-Seguros/Dataset/base_seguros.csv",
                          fileEncoding = "latin1")

glimpse(base_seguros)

################
### EXEMPLOS ###
################

# QUAIS OS GRUPOS #

base_seguros %>%
  select(nogrupo) %>%
  distinct()

# SEGURO VIAGEM #

base_seguros %>% 
  filter(str_detect(noramo, pattern = "Viagem")) %>% 
  select(coramo,noramo, gr_ramo) %>%
  distinct()

# 1369 - Viagem (Individual)
# 0544 - R.C.T.Viagem Intern-Pes Trans ou ñ (Auto)
# 0969 -  Viagem (Coletivo)

base_seguros %>%
  filter(coramo == 1369) %>%
  group_by(ano) %>%
  summarise(sinistro = sum(sinistro_ocorrido, na.rm = T),
            premio_ganho = sum(premio_ganho, na.rm = T),
            comissao = sum(desp_com, na.rm = T),
            premio_emitido = sum(premio_emitido2, na.rm = T)) %>%
  mutate(sinistralidade = sinistro / premio_ganho, 
         comissao = comissao / premio_emitido) %>%
  ggplot(aes(x = ano, y = sinistralidade)) +
  geom_col(fill = "#002364") +
  geom_text(aes(label = round(sinistralidade*100,0)), vjust=-0.3, size = 3) +
  scale_x_continuous("", breaks = c(2015:2021)) +
  scale_y_continuous("", labels = scales::percent) +
  ggtitle("Sinistralidade - Seguro Viagem Individual") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

base_seguros %>%
  filter(coramo == 1369) %>%
  group_by(ano) %>%
  summarise(sinistro = sum(sinistro_ocorrido, na.rm = T),
            premio_ganho = sum(premio_ganho, na.rm = T),
            comissao = sum(desp_com, na.rm = T),
            premio_emitido = sum(premio_emitido2, na.rm = T)) %>%
  mutate(sinistralidade = sinistro / premio_ganho, 
         comissao = comissao / premio_emitido) %>%
  ggplot(aes(x = ano, y = comissao)) +
  geom_col(fill = "#50B4AA") +
  geom_text(aes(label = round(comissao*100,0)), vjust=-0.3, size = 3) +
  scale_x_continuous("", breaks = c(2015:2021)) +
  scale_y_continuous("", labels = scales::percent) +
  ggtitle("Comissão - Seguro Viagem Individual") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# SEGURO PRESTAMISTA #

base_seguros %>% 
  filter(str_detect(noramo, pattern = "Garantia")) %>% 
  select(coramo,noramo, gr_ramo) %>%
  distinct()

base_seguros %>%
  filter(coramo == 0977) %>%
  group_by(ano) %>%
  summarise(sinistro = sum(sinistro_ocorrido, na.rm = T),
            premio_ganho = sum(premio_ganho, na.rm = T),
            comissao = sum(desp_com, na.rm = T),
            premio_emitido = sum(premio_emitido2, na.rm = T)) %>%
  mutate(sinistralidade = sinistro / premio_ganho, 
         comissao = comissao / premio_emitido) %>%
  ggplot(aes(x = ano, y = sinistralidade)) +
  geom_col(fill = "#002364") +
  geom_text(aes(label = round(sinistralidade*100,0)), vjust=-0.3, size = 3) +
  scale_x_continuous("", breaks = c(2015:2021)) +
  scale_y_continuous("", labels = scales::percent) +
  ggtitle("Sinistralidade - Seguro Prestamista Coletivo") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

base_seguros %>%
  filter(coramo == 0977) %>%
  group_by(ano) %>%
  summarise(sinistro = sum(sinistro_ocorrido, na.rm = T),
            premio_ganho = sum(premio_ganho, na.rm = T),
            comissao = sum(desp_com, na.rm = T),
            premio_emitido = sum(premio_emitido2, na.rm = T)) %>%
  mutate(sinistralidade = sinistro / premio_ganho, 
         comissao = comissao / premio_emitido) %>%
  ggplot(aes(x = ano, y = comissao)) +
  geom_col(fill = "#50B4AA") +
  geom_text(aes(label = round(comissao*100,0)), vjust=-0.3, size = 3) +
  scale_x_continuous("", breaks = c(2015:2021)) +
  scale_y_continuous("", labels = scales::percent) +
  ggtitle("Comissão - Seguro Prestamista Coletivo") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Vendo uma entidade específica - CARDIF #
base_seguros %>% 
   filter(str_detect(noenti, pattern = "CARDIF")) %>% 
   select(noenti, nogrupo) %>%
   distinct()
 
base_seguros %>%
   filter(coramo == 0977, str_detect(nogrupo, "CARDIF")) %>%
   group_by(ano) %>%
   summarise(sinistro = sum(sinistro_ocorrido, na.rm = T),
             premio_ganho = sum(premio_ganho, na.rm = T),
             comissao = sum(desp_com, na.rm = T),
             premio_emitido = sum(premio_emitido2, na.rm = T)) %>%
   mutate(sinistralidade = sinistro / premio_ganho, 
          comissao = comissao / premio_emitido)


  
 

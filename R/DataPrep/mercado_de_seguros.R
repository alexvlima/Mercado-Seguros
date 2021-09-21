###################
### BIBLIOTECAS ###
###################

library(tidyverse)

################
### DATASETS ###
################

ramos <- read.csv2("ses_ramos.csv", sep = ";", dec = ",", header = T)
grupo.eco <- read.csv2("ses_grupos_economicos.csv", sep = ";", dec = ",", header = T)
grupo.ramo <- read.csv2("ses_gruposramos.csv", sep = ";", dec = ",", header = T)
seg <- read.csv2("ses_seguros.csv", sep = ";", dec = ",", header = T)
seg.uf <- read.csv2("ses_uf2.csv", sep = ";", dec = ",", header = T)

###############
### AJUSTES ###
###############

ramos <- ramos %>% 
  mutate(graid = substr(ramos$noramo, 1, 2))

ramos$graid <- as.numeric(ramos$graid)

grupo.eco <- grupo.eco %>% 
  mutate(ano = substr(grupo.eco$damesano, 1, 4),
         mes = substr(grupo.eco$damesano, 5, 6))

#SEGUROS GERAL

base.seg <- seg %>%
  left_join(ramos, by = c("coramo" = "coramo")) %>%
  left_join(grupo.eco, by = c("cogrupo" = "cogrupo", "damesano" = "damesano", "coenti" = "coenti")) %>% 
  left_join(grupo.ramo, by = c("graid" = "graid"))




base.seg <- base.seg %>% 
  select(damesano, nogrupo, granome, premio_direto, sinistro_direto, sinistro_ocorrido) %>% 
  mutate(ano = substr(base.seg$damesano, 1, 4),
         mes = substr(base.seg$damesano, 5, 6))

base.seg <- unique(base.seg %>% 
  select(ano, mes, nogrupo, granome, premio_direto, sinistro_direto, sinistro_ocorrido) %>% 
  group_by(ano, mes, nogrupo, granome) %>% 
  mutate(premio = sum(premio_direto),
         sinistro = sum(sinistro_direto, sinistro_ocorrido)) %>% 
  select(ano, mes, nogrupo, granome, premio, sinistro) %>% 
  na.omit())


base.seg$nogrupo <- iconv(base.seg$nogrupo, "latin1", "UTF-8")
base.seg$granome <- iconv(base.seg$granome, "latin1", "UTF-8")




#SEGUROS UF

base.seg.uf <- seg.uf %>%
  left_join(ramos, by = c("ramos" = "coramo")) %>%
  left_join(grupo.eco, by = c("damesano" = "damesano", "coenti" = "coenti")) %>% 
  left_join(grupo.ramo, by = c("graid" = "graid", "gracodigo" = "gracodigo"))


base.seg.uf <- base.seg.uf %>% 
  select(damesano, nogrupo, granome, UF, premio_dir, sin_dir) %>%
  mutate(ano = substr(base.seg.uf$damesano, 1, 4),
         mes = substr(base.seg.uf$damesano, 5, 6))

base.seg.uf <- unique(base.seg.uf %>% 
  select(ano, mes, nogrupo, granome, UF, premio_dir, sin_dir) %>% 
  group_by(ano, mes, nogrupo, granome, UF) %>% 
  mutate(premio = sum(premio_dir),
         sinistro = sum(sin_dir)) %>% 
  select(ano, mes, nogrupo, granome, UF, premio, sinistro) %>% 
  na.omit())


base.seg.uf$nogrupo <- iconv(base.seg.uf$nogrupo, "latin1", "UTF-8")
base.seg.uf$granome <- iconv(base.seg.uf$granome, "latin1", "UTF-8")

#PREVIDENCIA


datas <- matrix(0,ncol = 2, nrow = 252)

meses <- c("01","02","03","04","05","06","07","08","09","10","11","12")

anos <- c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009",
          "2010","2011","2012","2013","2014","2015","2016","2017","2018","2019",
          "2020")

mult.anos <- sort(rep(anos,12))

mult.meses <- rep(meses,21)

for(i in 1:length((mult.anos))){
  datas[i,1] <- mult.anos[i]
  datas[i,2] <- mult.meses[i]
}

datas <- as.data.frame(datas)
names(datas) <- c("ano", "mes")




#PGBL RESGATE

pgbl.resgates <- read.csv2("ses_pgbl_resgates.csv", sep = ";", header = T)

pgbl.resgates$damesano <- as.factor(pgbl.resgates$damesano)

pgbl.resgates<- pgbl.resgates %>% 
  mutate(ano = substr(pgbl.resgates$damesano,1,4),
         mes = substr(pgbl.resgates$damesano,5,6))

pgbl.resgates <- unique(pgbl.resgates %>% 
                          group_by(ano, mes, coenti) %>% 
                          mutate(resgate = sum(resg_total, resg_parcial, Resg_Pag_programado),
                                 resg_total = sum(resg_total),
                                 resg_parcial = sum(resg_parcial),
                                 Resg_Pag_programado = sum(Resg_Pag_programado)
                          ))

pgbl.resgates <- pgbl.resgates %>% 
  select(ano, mes, coenti, resg_total, resg_parcial, resgate)


#PGBL CONTRIBUICAO

pgbl.contri <- read.csv2("ses_pgbl_contrib.csv", sep = ";", header = T)

pgbl.contri$damesano <- as.factor(pgbl.contri$damesano)

pgbl.contri <- pgbl.contri %>% 
  mutate(ano = substr(pgbl.contri$damesano,1,4),
         mes = substr(pgbl.contri$damesano,5,6))


pgbl.contri <- pgbl.contri %>% 
  select(ano, mes, coenti, valor) %>% 
  group_by(ano, mes, coenti) %>% 
  summarise(contribuicao = sum(valor))

#PGBL


pgbl <- datas %>% 
  left_join(pgbl.resgates, by = c("ano" = "ano", "mes" = "mes")) %>% 
  left_join(pgbl.contri, by = c("ano" = "ano", "mes" = "mes", "coenti" = "coenti")) %>%
  left_join(grupo.eco, by = c("ano" = "ano", "mes" = "mes", "coenti" = "coenti")) %>% 
  na.omit() %>%
  select(ano, mes, nogrupo, resgate, contribuicao)

pgbl <- pgbl %>% 
  group_by(ano, mes, nogrupo) %>% 
  mutate(resgate = sum(resgate),
         contribuicao = sum(contribuicao))


names(pgbl) <- c("ano", "mes", "nogrupo","pgbl_resg", "pgbl_contribuicao")


#PGBL UF


pgbl.uf <- read.csv2("ses_pgbl_uf.csv", sep = ";", header = T)

names(pgbl.uf) <- c("coenti", "damesano", "uf", "contrib", "benefpago",
                    "resgpago", "numpartic", "numbenef", "numresg")


pgbl.uf$damesano <- as.factor(pgbl.uf$damesano)

pgbl.uf <- pgbl.uf %>% 
  mutate(ano = substr(pgbl.uf$damesano,1,4),
         mes = substr(pgbl.uf$damesano,5,6)) %>% 
  left_join(grupo.eco, by = c("ano" = "ano", "mes" = "mes", "coenti" = "coenti")) %>% 
  select(ano, mes, nogrupo, uf, contrib, resgpago, numpartic, numresg) %>% 
  na.omit()


#PREV UF

prev.uf <- read.csv2("ses_prev_uf.csv", sep = ";", header = T)

names(prev.uf) <- c("coenti", "damesano", "uf", "contrib", "benefpago",
                    "resgpago", "numpartic", "numbenef", "numresg")


prev.uf$damesano <- as.factor(prev.uf$damesano)

prev.uf <- prev.uf %>% 
  mutate(ano = substr(prev.uf$damesano,1,4),
         mes = substr(prev.uf$damesano,5,6)) %>%
  left_join(grupo.eco, by = c("ano" = "ano", "mes" = "mes", "coenti" = "coenti")) %>% 
  select(ano, mes, nogrupo, uf, contrib, resgpago, benefpago, numpartic, numresg, numbenef, coenti) %>% 
  na.omit()


prev.uf$nogrupo <- iconv(prev.uf$nogrupo, "latin1", "UTF-8")



#PREV

prev <- prev.uf %>% 
  group_by(ano, mes, nogrupo) %>% 
  summarise(prev_contribuicao = sum(contrib), prev_beneficio = sum(benefpago), prev_resgates = sum(resgpago),
            prev_qtd_contribuintes = sum(numpartic), prev_qtd_beneficiarios = sum(numbenef), prev_qtd_resgatantes = sum(numresg))

View(prev)

#VGBL RESGATE

vgbl.resg <- read.csv2("ses_vgbl_resgates.csv", sep = ";", header = T)

vgbl.resg$damesano <- as.factor(vgbl.resg$damesano)

vgbl.resg <- vgbl.resg %>% 
  mutate(ano = substr(vgbl.resg$damesano,1,4),
         mes = substr(vgbl.resg$damesano,5,6)) %>% 
  left_join(grupo.eco, by = c("ano" = "ano", "mes" = "mes", "coenti" = "coenti"))



vgbl.resg <- unique(vgbl.resg %>% 
              group_by(ano, mes, nogrupo) %>% 
              mutate(resgate = format(sum(resg_total, resg_parcial, Resg_Pag_programado), scientific = F)) %>% 
                na.omit())


vgbl.resg <- vgbl.resg %>% 
  select(ano, mes, nogrupo, resgate)

names(vgbl.resg) <- c("ano", "mes", "nogrupo", "vgbl_resgate")


#PREV + PGBL + VGBL

base.prev <- prev %>% 
  left_join(pgbl, by = c("ano" = "ano", "mes" = "mes", "nogrupo" = "nogrupo") ) %>% 
  left_join(vgbl.resg, by = c("ano" = "ano", "mes" = "mes", "nogrupo" = "nogrupo"))



base.prev <- unique(base.prev %>% 
  mutate(vgbl_contribuicao = prev_contribuicao - pgbl_contribuicao) %>% 
  na.omit())

base.prev$nogrupo <- iconv(base.prev$nogrupo, "latin1", "UTF-8")

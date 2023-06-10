
# DIRETORIO ---------------------------------------------------------------

# getwd()
setwd("C:\\Users\\seg13004\\OneDrive - CNP Seguros Holding Brasil\\Documentos\\2022\\052_Eficiencia_Seguradoras")

# BIBLIOTECAS -------------------------------------------------------------

library(tidyverse)
library(deaR)

# DATASET -----------------------------------------------------------------

source("~/conexao_sql_atuarial.R")

inputs <- dbGetQuery(sql_atuarial,
                     "SELECT
                      		DES_ENTIS
                          ,NUM_PERIOD
                          ,NUM_QUADRO
                          ,COD_CMPID
                          ,DES_CMPTIT
                          ,VDR_VALOR 
                      FROM 
                      	[WORK_ANALISE].[dbo].[SES_BALANCO]   
                      WHERE	1=1 
                      		  AND DES_PERIOD = 'ANO' 
                      		  AND NUM_PERIOD IN (2018,2019,2020,2021)
                      	  	AND COD_CMPID IN (3333,4069,6202,6314,6315,11237)
                      ORDER BY 
                        NUM_PERIOD, DES_ENTIS, NOM_QUADRO, NUM_ORDER
                     ")

seguros <- dbGetQuery(sql_atuarial,
                      "SELECT DISTINCT
	                        	seg.DAMESANO
                            ,seg.COENTI
                            ,grupos.NOENTI
                            ,seg.CORAMO
                            ,ramos.NORAMO
                            ,seg.PREMIO_EMITIDO2
                            ,seg.SINISTRO_OCORRIDO
                       FROM 
                       	  SUSEP_SES..SES_SEGUROS seg 
                       LEFT JOIN  
                       	  SUSEP_SES..SES_RAMOS ramos
                       		  ON seg.coramo = ramos.coramo
                       LEFT JOIN 
                       	  SUSEP_SES..SES_GRUPOS_ECONOMICOS grupos
                       		  ON grupos.COENTI = seg.COENTI AND grupos.DAMESANO = seg.DAMESANO
                       WHERE  1=1 
                       	      AND ramos.coramo IN (114,118) -- RESIDENCIAL e EMPRESARIAL  
                       	      AND seg.DAMESANO > 201800")

provisoes <- 
  dbGetQuery(sql_atuarial,
            "SELECT DISTINCT
            	prov.DAMESANO
            	,prov.COENTI
            	,grupos.NOENTI
            	,prov.CORAMO
            	,ramos.NORAMO
            	,prov.PPNG
            	,prov.PSL
            	,prov.IBNR
            	,prov.IBNER
            	,prov.REC_SINLIQ
            FROM 
            	SUSEP_SES..SES_PROVRAMOS prov 
            LEFT JOIN  
            	SUSEP_SES..SES_RAMOS ramos
            		ON prov.coramo = ramos.coramo
            LEFT JOIN 
            	SUSEP_SES..SES_GRUPOS_ECONOMICOS grupos
            		ON grupos.COENTI = prov.COENTI AND grupos.DAMESANO = prov.DAMESANO
            WHERE 1=1 
            	  AND ramos.coramo IN (114,118) -- RESIDENCIAL e EMPRESARIAL  
            	  AND prov.DAMESANO > 201800
            	  AND RIGHT(prov.DAMESANO,2) = 12
            ORDER BY
            	prov.COENTI
            	,prov.DAMESANO")

rm(drv, sql_atuarial)

# OUTPUTS
# Invested assets (PPNG ... SES_PROVRAMOS)
# Losses incurred (SES SEGUROS)
# Reinsurance reserve (reservas de resseguro .. SES_PROVRAMOS)
# Own reserve (SES_PROVRAMOS)

# INPUTS 
# Labour cost (CMPID = 6314+6315)
# Non-labour cost (cmpid = 6202+4069+11237)
# Equity Capital (CMPID = 3333)

# Obs: coloquei premio emitido para criar um indicador dos inputs de acordo com o ramo escolhido

# AJUSTES -----------------------------------------------------------------

# INPUTS #

glimpse(inputs)

inputs <- 
  inputs |>
  filter(NUM_PERIOD == 2021) |>
  mutate(COD_CMPID = as.numeric(COD_CMPID),
         COENTI = as.numeric(str_extract(DES_ENTIS, pattern = '[0-9]+'))) |>
  select(-NUM_QUADRO, -NUM_PERIOD, -DES_ENTIS, -DES_CMPTIT) |>
  spread(COD_CMPID,VDR_VALOR)

colnames(inputs) <- c("COENTI","EQUITY","DESPESA_DO","DESPESA_DA","DESP_PESSOAL","DESP_TERCEIRO","CUSTO_AQUISICAO")

inputs <- 
  inputs |>
  mutate(LABOUR_COST = DESP_PESSOAL + DESP_TERCEIRO,
         NON_LABOUR_COST = DESPESA_DO + DESPESA_DA + CUSTO_AQUISICAO - DESP_PESSOAL - DESP_TERCEIRO) |>
  select(COENTI, EQUITY, LABOUR_COST, NON_LABOUR_COST)

# OUTPUTS #

glimpse(seguros)

seguros <- 
  seguros |>
  filter(DAMESANO > 202100, DAMESANO < 202200, 
         CORAMO == 114) |>
  mutate(COENTI = as.numeric(COENTI),
         CORAMO = as.numeric(CORAMO)) |>
  group_by(COENTI, NOENTI) |>
  summarise(PREMIUM = sum(PREMIO_EMITIDO2, na.rm = T),
            LOSSES_INCURRED = sum(SINISTRO_OCORRIDO, na.rm = T))

glimpse(provisoes)

provisoes <- 
  provisoes |>
  filter(DAMESANO == 202112,
         CORAMO == 114) |> 
  mutate(COENTI = as.numeric(COENTI),
         REINSURANCE_RES = REC_SINLIQ,
         OWN_RES = PSL + IBNR + IBNER,
         INVESTED_ASSETS = PPNG) |>
  select(COENTI, INVESTED_ASSETS, REINSURANCE_RES, OWN_RES)


# DATASET FINAL -----------------------------------------------------------

df <- 
  provisoes |> 
  left_join(seguros, by = "COENTI") |>
  left_join(inputs, by = "COENTI") |>
  mutate(EQUITY = EQUITY,
         LABOUR_COST = -LABOUR_COST,
         NON_LABOUR_COST = -NON_LABOUR_COST) |>
  select(-PREMIUM) %>%
  filter(EQUITY >= 0, LABOUR_COST >= 0, NON_LABOUR_COST >= 0,
         INVESTED_ASSETS > 0, OWN_RES >= 0, REINSURANCE_RES >= 0)

df <- df[complete.cases(df),]

glimpse(df)
summary(df)
nrow(df)

# ANALISE DE EFICIENCIA - DEA  --------------------------------------------

df_dea <- 
  read_data(df, 
          inputs = c("EQUITY","LABOUR_COST","NON_LABOUR_COST"),
          outputs = c("INVESTED_ASSETS","REINSURANCE_RES","OWN_RES"))

glimpse(df_dea)

dea_crs <- model_basic(df_dea, orientation = "oo", rts = "crs")
eff <- round(1/efficiencies(dea_crs),2)
eff
df$eff_crs <- eff

dea_vrs <- model_basic(df_dea, orientation = "oo", rts = "vrs")
eff <- round(1/efficiencies(dea_vrs),2)
eff

df$eff_vrs <- eff

df %>% filter(eff == 1)

dea_vrs$DMU$`5631`
1/1.310951

dea_vrs$DMU$`5193`
1/28.5243

# references(dea_vrs)$`5631`
targets(dea_vrs)$target_output[,1]
targets(dea_vrs)$target_output[,2]
targets(dea_vrs)$target_output[,3]

# 5631 - CAIXA SEGURADORA
# 5193 - PREVISUL
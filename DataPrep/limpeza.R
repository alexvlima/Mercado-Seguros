# dados previdencia ---------------------------------------------------------------

dados_prev <- read.csv("base_prev.csv",sep = ";")

dados_prev <- dados_prev %>% select(-X,-mes,-coenti,-damesano,-cogrupo,-noenti)


nogrupo <- c()
prev_contribuicao <- c()
prev_beneficio <- c()
prev_resgates <- c()
prev_qtd_contruibuintes <- c()
prev_qtd_beneficiarios <- c()
prev_qtd_resgatantes <- c()
ano <- c()

for (i in unique(dados_prev$nogrupo)){
  for (k in unique(dados_prev$ano)){
    df <- filter(dados_prev, nogrupo == i  & ano == k)
    prev_contribuicao <- c(prev_contribuicao,sum(df$prev_contribuicao))
    prev_beneficio <- c(prev_beneficio,sum(df$prev_beneficio))
    prev_resgates <- c(prev_resgates,sum(df$prev_resgates))
    prev_qtd_contruibuintes <- c(prev_qtd_contruibuintes,sum(df$prev_qtd_contruibuintes))
    prev_qtd_beneficiarios <- c(prev_qtd_beneficiarios,sum(df$prev_qtd_beneficiarios))
    prev_qtd_resgatantes <- c(prev_qtd_resgatantes,sum(df$prev_qtd_resgatantes))
    nogrupo <- c(nogrupo,i)
    ano <- c(ano,k)
  }
}

dados_prev <- data.frame(ano,nogrupo,prev_contribuicao,prev_beneficio,prev_resgates,prev_qtd_resgatantes,prev_qtd_contruibuintes,prev_qtd_beneficiarios)

dados_prev$nogrupo <- gsub(" ", "", levels(dados_prev$nogrupo), fixed = TRUE)

write_delim(dados_prev, "DADOS_PREV.csv",delim = ";", col_names = T)
# dados seguro ------------------------------------------------------------------ 

dados_seguro <- read.csv("dados_seguros.csv",sep = ";")

anos <- dados_seguro$damesano%/%100
dados_seguro['anos'] <- anos

#as.Date(dados_seguro$damesano, format = "%YYYY%mm")

premio <- c()
sinistro <- c()
nogrupo <- c()
granome <- c()
anos <- c()

for (i in unique(dados_seguro$nogrupo)){
  for (k in unique(dados_seguro$granome)){
    for (j in unique(dados_seguro$anos)){
      df <- filter(dados_seguro, nogrupo == i & granome == k & anos == j)
      premio <- c(premio,sum(df$premio))
      sinistro <- c(sinistro,sum(df$sinistro))
      nogrupo <- c(nogrupo,i)
      granome <- c(granome,k)
      anos <- c(anos,j)
    }
  }
}

dados_seg <- data.frame(nogrupo,granome,premio,sinistro,anos)

dados_seg$nogrupo <- gsub(" ", "", levels(dados_seg$nogrupo), fixed = TRUE)

dados_seg$granome <- gsub(" ", "", levels(dados_seg$granome), fixed = TRUE)

write_delim(dados_seg, "DADOS_SEG.csv",delim = ";", col_names = T)
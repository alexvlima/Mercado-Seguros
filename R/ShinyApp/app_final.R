# Pacotes -----------------------------------------------------------------
library(tidyverse)
library(readxl)
library(reshape2)
library(ggpubr)
library(tm)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(scales)
require(dplyr)
# Dados -------------------------------------------------------------------

dados_prev <- read.csv("DADOS_PREV_.csv",sep = ";")
dados_prev <- dados_prev %>%
  mutate(prev_contribuicao = stringr::str_replace(prev_contribuicao, ",", "."))%>%
  mutate(prev_beneficio = stringr::str_replace(prev_beneficio, ",", "."))%>%
  mutate(prev_resgates = stringr::str_replace(prev_resgates, ",", "."))

dados_prev$nogrupo <- as.character(dados_prev$nogrupo)
dados_prev$prev_contribuicao <- as.numeric(dados_prev$prev_contribuicao)
dados_prev$prev_beneficio <- as.numeric(dados_prev$prev_beneficio)
dados_prev$prev_resgates <- as.numeric(dados_prev$prev_resgates)
dados_prev$nogrupo <- gsub(" ", "", (dados_prev$nogrupo), fixed = TRUE)
dados_prev$ano <- as.integer(dados_prev$ano)
dados_prev$nogrupo <- gsub(" ", "", (dados_prev$nogrupo), fixed = TRUE)


dados_seg <- read.csv("DADOS_SEG.csv",sep = ";")
dados_seg_ <- read.csv("DADOS_SEG_.csv",sep = ";")
dados_seg$nogrupo <- gsub(" ", "", dados_seg_$nogrupo, fixed = TRUE)
dados_seg$granome <- gsub(" ", "", dados_seg_$granome, fixed = TRUE)
dados_seg$premio <- dados_seg$premio/100000
dados_seg$sinistro <- dados_seg$sinistro/100000



# especificacoes
cor_graficos <- "#00688B"

# App ---------------------------------------------------------------------

sidebar<-dashboardSidebar(
  sidebarMenu(id="teste",
              menuItem("Página Inicial", tabName = "geral", icon = icon("balance-scale-left")),
              menuItem("Seguros", tabName = "seg", icon = icon("donate")),
              menuItem("Previdência", tabName = "prev", icon = icon("comments-dollar"))
  ),
  
  conditionalPanel(
    condition = "input.teste == 'prev'",
    selectInput('grupo_p', 'Escolha o(s) grupo(s):', 
                choices = c(unique(dados_prev$nogrupo)),
                multiple = TRUE
    ),
    
    selectInput('escolha', 'Escolha quais dados deseja visualizar:', 
                choices = c("Valores","Nº Usuários","Proporção"),
                multiple = FALSE, selected = "Valores"
    ),
    
    sliderTextInput("data_prev", "Período:", 
                choices = sort(unique(dados_prev$ano)),
                selected =  c((min(dados_prev$ano)), (max(dados_prev$ano))))
    
  ),
  
  conditionalPanel(
    condition = "input.teste == 'seg'",
    selectInput('grupo', 'Escolha a(s) organização(ões):', 
                choices = c(unique(dados_seg$nogrupo)),
                multiple = TRUE
    ),
    
    selectInput('tipo', 'Tipo de seguro:', 
                choices = c(unique(dados_seg$granome)),
                multiple = TRUE
    ),
    
    sliderTextInput("data_seg", "Período:", 
                    choices = sort(unique(dados_seg$anos)),
                    selected = c((min(dados_seg$anos)), (max(dados_seg$anos)))
    ),
    
    switchInput(
      inputId = "por_tipo",
      label = "por tipo", 
      labelWidth = "80px",
      onLabel = "Sim",
      offLabel = "Não"
    )
    
  )
)
header<-dashboardHeader(title = "Mercado de Seguros")


body<-dashboardBody(
  
  # Página Inicial ----------------------------------------------------------
  
  tabItems(
    tabItem(tabName = "geral",
            fluidRow(
              column(width = 12,
                     
                     
                     infoBoxOutput("info_1", width = 6),
                     infoBoxOutput("info_2", width = 6)
                    
              ),
              
              column(width = 12,
                     
                     
                     infoBoxOutput("info_3", width = 4),
                     infoBoxOutput("info_4", width = 4),
                     infoBoxOutput("info_5", width = 4)
                     
              ),
              
          
              tabBox(
                title = "Resumo de valores (em reais)",
                id = "tab1_", width = 12,
                tabPanel("Seguro", plotlyOutput("grafico")),
                tabPanel("Previdência", plotlyOutput("grafico0"))
              ),
              
            )            
    ),
    
    
    # Seguros --------------------------------------------------
    
    tabItem(tabName = "seg",
            
            fluidRow(
              
              column(width = 12,
                     
                     infoBoxOutput("pre_total", width = 6),
                     infoBoxOutput("sin_total", width = 6)
              ),
              
              
              # grafico de barras
              
              tabBox(
                title = "Organizações (médias anuais)",
                id = "tabset0", width = 12,
                tabPanel("Prêmio (em reais)", plotlyOutput("grafico_1")),
                tabPanel("Sinistro (em reais)", plotlyOutput("grafico_2"))
                
              ),
              
              # grafico de linhas
              
              tabBox(
                title = "Série histórica",
                id = "tabset1", width = 12,
                tabPanel("Prêmio (em reais)", plotlyOutput("grafico_3")),
                tabPanel("Sinistro (em reais)", plotlyOutput("grafico_4"))
              )
              
            )             
    ),
    
    # Previdencia -------------------------------------------------------------
    
    tabItem(tabName = "prev",
            fluidRow(
              
              column(width = 12,
                     
                     infoBoxOutput("cont_total", width = 4),
                     infoBoxOutput("ben_total", width = 4),
                     infoBoxOutput("res_total", width = 4),
              ),
              
              # grafico de colunas
              
              tabBox(
                title = "Comparação entre as entidades (médias anuais)",
                id = "tabset3", width = 12,
                tabPanel("Contribuições", plotlyOutput("grafico_7")),
                tabPanel("Benefícios",    plotlyOutput("grafico_8")),
                tabPanel("Resgates",      plotlyOutput("grafico_9"))
              ),
              
              tabBox(
                title = "Série histórica",
                id = "tabset2", width = 12,
                tabPanel("Contribuições", plotlyOutput("grafico_5", height = "400px")),
                tabPanel("Benefícios", plotlyOutput("grafico_6", height = "400px")),
                tabPanel("Resgates", plotlyOutput("grafico_6_2", height = "400px"))
                
              )
              
            )
    )
  )
)

ui<-dashboardPage(sidebar = sidebar,header = header,body= body)

server <- shinyServer(function(input, output) {
  
  # Pagina Inicial -------------------------------------------------------------------------------
  
  
  output$info_1 <- renderInfoBox({
    
    p <- infoBox(
      "Prêmio total (em milhões de reais)", round(sum(dados_seg$premio)/1000000,4) ,
      color = "purple", fill = TRUE,icon=icon("file-invoice-dollar")
    )
    
    p
  })
  
  output$info_2 <- renderInfoBox({
    p <- infoBox(
      "Sinistro total (em milhões de reais)", round(sum(dados_seg$sinistro)/1000000,4) ,
      color = "purple", fill = TRUE,icon=icon("money-check-alt")
    )
    p
  })
  
  output$info_3 <- renderInfoBox({
    
    p <- infoBox(
      "Resgate total (em reais)", sum(dados_prev$prev_resgates) ,
      color = "orange", fill = TRUE,icon=icon("money-check-alt")
    )
    p
  })
  
  output$info_4 <- renderInfoBox({
    
    p <- infoBox(
      "Benefícios totais (em reais)", sum(dados_prev$prev_beneficio) ,
      color = "orange", fill = TRUE,icon=icon("file-invoice-dollar")
    )
    
    p
  })
  
  output$info_5 <- renderInfoBox({
    
    p <- infoBox(
      "Contribuição total (em reais)", sum(dados_prev$prev_contribuicao) ,
      color = "orange", fill = TRUE,icon=icon("money-check-alt")
    )
    
    p
  })
  
  
  # GRAFICOS

  output$grafico0 <-renderPlotly({
    
    c <- sum(dados_prev$prev_contribuicao)
    r <- sum(dados_prev$prev_resgates)
    b <- sum(dados_prev$prev_beneficio)
    
      p <- plot_ly(
        labels = c("Previdência","Contribuições","Benefícios","Resgates"),
        parents = c("", "Previdência", "Previdência","Previdência"),
        values = c(c+r+b,c,b,r),
        type = 'sunburst'
      )
    
    p
  })
  
  output$grafico <-renderPlotly({
    
    pre <- round(sum(dados_seg$premio)/1000000,2)
    seg <- round(sum(dados_seg$sinistro)/1000000,2)
    
      p <- plot_ly(
        labels = c("Seguro","Prêmio","Sinistro"),
        parents = c("", "Seguro", "Seguro"),
        values = c(pre+seg,pre,seg ),
        type = 'sunburst'
      )
    
    p
  })
  
  
  # Seguros -----------------------------------------------------------------
  
  # APLICANDO FILTROS 
  
  
  data_filtro_seg <- reactive({
    
    # filtrando anos
    
    data <- dados_seg %>% filter(anos >= input$data_seg[1] & anos <= input$data_seg[2])
    
    # filtrando pelo nogrupo
    
    if ((length(input$grupo) != 0)){
      data <- data %>% filter(nogrupo %in% input$grupo)
    }
    else{
      data <- data %>% filter(nogrupo %in% unique(data$nogrupo))
    }
    
    
    # filtrando por granome
    
    if ((length(input$tipo) != 0)){
      data <- data %>% filter(granome %in% input$tipo)
    }
    
    data 
  })
  # INFORMACOES
  
  output$pre_total <- renderInfoBox({
    infoBox(
      "Prêmio total (em milhões de reais)", round(sum(data_filtro_seg()$premio)/1000000,4) ,
      color = "purple", fill = TRUE,icon=icon("file-invoice-dollar")
    )
  })
  
  output$sin_total <- renderInfoBox({
    infoBox(
      "Sinistro total (em milhões de reais)", round(sum(data_filtro_seg()$sinistro)/1000000,4) ,
      color = "purple", fill = TRUE,icon=icon("money-check-alt")
    )
  })
  # GRAFICO DE COLUNAS - PREMIO
  
  data1_seg <- reactive({
    
    grupos <- unique(data_filtro_seg()$nogrupo)
    pre <- c()
    
    for (i in grupos){
      df <- data_filtro_seg() %>% filter(nogrupo == i)
      p <- sum(df$premio)/length(df$premio)
      pre <- c(pre, p)
    }
    
    
    grupos_ <- c()
    m <- median(pre)
    
    
    for (k in c(1:length(grupos))){
      if (pre[k] < m){
        grupos_ <- c(grupos_,"Outros")
      }
      else{
        grupos_ <- c(grupos_,grupos[k])
      }
    }
    
    data <- data.frame(grupos_,pre)
    pre_ <- c()
    
    for (i in grupos_){
      d <- data %>% filter(grupos_ == i)
      p <- sum(d$pre)/length(d$pre)
      pre_ <- c(pre_, p)
    }
    
    
    if ((length(input$grupo) == 0)){
      grupos <- grupos_
      pre <- pre_
      data <- data.frame(grupos,pre)
    }
    else{
      data <- data.frame(grupos,pre)
    }
    
    unique(data)
    
  })
  
  data1_seg_tipo <- reactive({
    
    grupos <- unique(data_filtro_seg()$nogrupo)
    tipo <- unique(data_filtro_seg()$granome)
    
    g <- c()
    t <- c()
    pre <- c()
    
    for (i in grupos){
      for (j in tipo){
        df <- data_filtro_seg() %>% filter(nogrupo == i & granome == j)
        p <- sum(df$premio)/length(df$premio)
        pre <- c(pre, p)
        g <- c(g,i)
        t <- c(t,j)
      }
    }
    
    grupos <- g
    pre <- pre
    Tipo <- t
    
    data <- data.frame(grupos,pre,Tipo) %>% na.exclude()
    
    t <- data$Tipo
    pre <- data$pre
    g <- data$grupos
    g <- as.vector(g)
    
    g_ <- c()
    m <- median(pre)
    
    
    for (k in 1:length(g)){
      if (pre[k] < m){
        g_ <- c(g_,"Outros")
      }
      else{
        g_ <- c(g_,g[k])
      }
    }
    
    data1 <- data.frame(g_,pre,t)
    pre_ <- c()
    g__ <- c()
    t_ <- c()
    
    for (i in unique(g_)){
      for (j in unique(t)){
        df <- data1 %>% filter(g_ == i & t == j)
        p <- sum(df$pre)/length(df$pre)
        pre_ <- c(pre_, p)
        g__ <- c(g__,i)
        t_ <- c(t_,j)
      }
    }
    
    if (length(input$grupo) == 0){
      grupos <- g__
      pre <- pre_
      Tipo <- t_
    }
    else{
      grupos <- g
      pre <- pre
      Tipo <- t
    }
    
    data <- data.frame(grupos,pre,Tipo) %>% na.exclude()
    data
    
  })
  
  output$grafico_1 <- renderPlotly({
    
    p <- data1_seg() %>% 
      ggplot(aes(x=reorder(grupos, pre), y=pre,text = paste0('Grupo: ', grupos, '\n', 'Número: ', round(pre,2) ))) +
      geom_bar(stat="identity", fill=cor_graficos) +
      labs(x="Organizações", y="Prêmio (/100.000)") +
      theme_bw() +
      theme(axis.title.y=element_text(colour="black", size=12),
            axis.title.x = element_text(colour="black", size=12),
            axis.text = element_text(colour = "black", size=9.5),
            panel.border = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(angle = 90, hjust = 1)) 
    
    if (input$por_tipo == TRUE & length(input$tipo) > 0) {
      p <- data1_seg_tipo() %>% 
        ggplot(aes(x=reorder(grupos, pre), y=pre,fill=Tipo,text = paste0('Grupo: ', grupos, '\n', 'Número: ', round(pre,2) ))) +
        geom_col(position = "stack") +
        labs(x="Organizações", y="Prêmio (/100.000)",fill="Tipo") +
        scale_fill_brewer(palette = "RdBu", direction = -1) +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.text.x = element_text(angle = 90, hjust = 1)) 
    }
    
    ggplotly(p , tooltip = "text")
  })
  
  # GRAFICO DE BARRAS - SINISTRO
  
  data2_seg <- reactive({
    
    grupos <- unique(data_filtro_seg()$nogrupo)
    sin <- c()
    
    for (i in grupos){
      df <- data_filtro_seg() %>% filter(nogrupo == i)
      p <- sum(df$sinistro)/length(df$sinistro)
      sin <- c(sin, p)
    }
    
    
    grupos_ <- c()
    m <- median(sin)
    
    
    for (k in c(1:length(grupos))){
      if (sin[k] < m){
        grupos_ <- c(grupos_,"Outros")
      }
      else{
        grupos_ <- c(grupos_,grupos[k])
      }
    }
    
    data <- data.frame(grupos_,sin)
    sin_ <- c()
    
    for (i in grupos_){
      d <- data %>% filter(grupos_ == i)
      p <- sum(d$sin)/length(d$sin)
      sin_ <- c(sin_, p)
    }
    
    
    if ((length(input$grupo) == 0)){
      grupos <- grupos_
      sin <- sin_
      data <- data.frame(grupos,sin)
    }
    else{
      data <- data.frame(grupos,sin)
    }
    unique(data)
    
  })
  
  data2_seg_tipo <- reactive({
    
    grupos <- unique(data_filtro_seg()$nogrupo)
    tipo <- unique(data_filtro_seg()$granome)
    
    g <- c()
    t <- c()
    sin <- c()
    
    for (i in grupos){
      for (j in tipo){
        df <- data_filtro_seg() %>% filter(nogrupo == i & granome == j)
        p <- sum(df$sinistro)/length(df$sinistro)
        sin <- c(sin, p)
        g <- c(g,i)
        t <- c(t,j)
      }
    }
    
    grupos <- g
    sin <- sin
    Tipo <- t
    
    data <- data.frame(grupos,sin,Tipo) %>% na.exclude()
    
    t <- data$Tipo
    sin <- data$sin
    g <- data$grupos
    g <- as.vector(g)
    
    g_ <- c()
    m <- median(sin)
    
    
    for (k in 1:length(g)){
      if (sin[k] < m){
        g_ <- c(g_,"Outros")
      }
      else{
        g_ <- c(g_,g[k])
      }
    }
    
    data1 <- data.frame(g_,sin,t)
    sin_ <- c()
    g__ <- c()
    t_ <- c()
    
    for (i in unique(g_)){
      for (j in unique(t)){
        df <- data1 %>% filter(g_ == i & t == j)
        p <- sum(df$sin)/length(df$sin)
        sin_ <- c(sin_, p)
        g__ <- c(g__,i)
        t_ <- c(t_,j)
      }
    }
    
    if (length(input$grupo) == 0){
      grupos <- g__
      sin <- sin_
      Tipo <- t_
    }
    else{
      grupos <- g
      sin <- sin
      Tipo <- t
    }
    
    data <- data.frame(grupos,sin,Tipo) %>% na.exclude()
    data
    
  })
  
  output$grafico_2 <- renderPlotly({
    
    p <- data2_seg() %>% 
      ggplot(aes(x=reorder(grupos, sin), y=sin,text = paste0('Grupo: ', grupos, '\n', 'Número: ', round(sin,2) ))) +
      geom_bar(stat="identity", fill=cor_graficos) +
      labs(x="Organizações", y="Sinistro (/100.000)") +
      theme_bw() +
      theme(axis.title.y=element_text(colour="black", size=12),
            axis.title.x = element_text(colour="black", size=12),
            axis.text = element_text(colour = "black", size=9.5),
            panel.border = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(angle = 90, hjust = 1)) 
    
    if (input$por_tipo == TRUE & length(input$tipo) > 0) {
      p <- data2_seg_tipo() %>% 
        ggplot(aes(x=reorder(grupos, sin), y=sin,fill=Tipo,text = paste0('Grupo: ', grupos, '\n', 'Número: ', round(sin,2) ))) +
        geom_col(position = "stack") +
        labs(x="Organizações", y="Sinistro (/100.000)",fill="Tipo") +
        scale_fill_brewer(palette = "RdBu", direction = -1) +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.text.x = element_text(angle = 90, hjust = 1)) 
    }
    
    ggplotly(p , tooltip = "text")
  })
  
  
  # GRAFICO DE LINHAS - PREMIO
  
  data3_seg <- reactive({
    
    a <- unique(data_filtro_seg()$anos)
    pre <- c()
    
    for (i in a){
      df <- data_filtro_seg() %>% filter(anos == i)
      p <- sum(df$premio)
      pre <- c(pre, p)
    }
    
    
    data <- data.frame(a,pre)
    
  })
  
  data3_seg_tipo <- reactive({
    
    anos <- unique(data_filtro_seg()$anos)
    tipo <- unique(data_filtro_seg()$granome)
    
    a <- c()
    t <- c()
    pre <- c()
    
    for (i in anos){
      for (j in tipo){
        df <- data_filtro_seg() %>% filter(anos == i & granome == j)
        p <- sum(df$premio)
        pre <- c(pre, p)
        a <- c(a,i)
        t <- c(t,j)
      }
    }
    
    Tipo <- t
    data <- data.frame(a,pre,Tipo)
    
    unique(data)
    
  })
  
  
  output$grafico_3 <- renderPlotly({
    
    p <- data3_seg() %>% 
      ggplot(aes(x=a, y=pre, group=1,text = paste0('Ano: ', a, '\n', 'Número: ', round(pre,2) ))) +
      geom_line(size=1,colour=cor_graficos) + geom_point(colour=cor_graficos,size=2) +
      labs(x="Ano", y="Prêmio (/100.000)") +
      #scale_x_date(labels = date_format("%aaaa/%mm")) +
      scale_x_continuous(breaks = seq(2000,2021,1) )+
      theme_bw() +
      theme(axis.title.y=element_text(colour="black", size=12),
            axis.title.x = element_text(colour="black", size=12),
            axis.text = element_text(colour = "black", size=9.5),
            panel.border = element_blank(),
            axis.line = element_line(colour = "black"))
    
    if (input$por_tipo == TRUE & length(input$tipo) > 0){
      p <- data3_seg_tipo() %>% 
        ggplot(aes(x=a, y=pre,group=Tipo,colour=Tipo,text = paste0('Ano: ', a, '\n', 'Número: ', round(pre,2) ))) +
        geom_line(size=1) + geom_point(size=2) +
        labs(x="Ano", y="Prêmio (/100.000)") +
        #scale_x_date(labels = date_format("%aaaa/%mm")) +
        scale_x_continuous(breaks = seq(2000,2021,1) )+
        scale_fill_brewer(palette = "RdBu", direction = -1) +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"))
    }
    
    
    ggplotly(p , tooltip = "text")
  })
  
  # GRAFICO LINHAS - SINISTRO
  
  data4_seg <- reactive({
    
    a <- unique(data_filtro_seg()$anos)
    sin <- c()
    
    for (i in a){
      df <- data_filtro_seg() %>% filter(anos == i)
      s <- sum(df$sinistro)
      sin <- c(sin, s)
    }
    
    
    data <- data.frame(a,sin)
    
  })
  
  data4_seg_tipo <- reactive({
    
    anos <- unique(data_filtro_seg()$anos)
    tipo <- unique(data_filtro_seg()$granome)
    
    a <- c()
    t <- c()
    sin <- c()
    
    for (i in anos){
      for (j in tipo){
        df <- data_filtro_seg() %>% filter(anos == i & granome == j)
        p <- sum(df$sinistro)
        sin <- c(sin, p)
        a <- c(a,i)
        t <- c(t,j)
      }
    }
    
    Tipo <- t
    data <- data.frame(a,sin,Tipo)
    
    unique(data)
    
  })
  
  output$grafico_4 <- renderPlotly({
    
    p <- data4_seg() %>% 
      ggplot(aes(x=a, y=sin, group=1,text = paste0('Ano: ', a, '\n', 'Número: ', round(sin,2) ))) +
      geom_line(size=1,colour=cor_graficos) + geom_point(colour=cor_graficos,size=2) +
      labs(x="Ano", y="Sinistro (/100.000)") +
      #scale_x_date(labels = date_format("%aaaa/%mm")) +
      scale_x_continuous(breaks = seq(2000,2021,1) )+
      theme_bw() +
      theme(axis.title.y=element_text(colour="black", size=12),
            axis.title.x = element_text(colour="black", size=12),
            axis.text = element_text(colour = "black", size=9.5),
            panel.border = element_blank(),
            axis.line = element_line(colour = "black"))
    
    if (input$por_tipo == TRUE & length(input$tipo) > 0){
      p <- data4_seg_tipo() %>% 
        ggplot(aes(x=a, y=sin,group=Tipo,colour=Tipo,text = paste0('Ano: ', a, '\n', 'Número: ', round(sin,2) ))) +
        geom_line(size=1) + geom_point(size=2) +
        labs(x="Ano", y="Sinistro (/100.000)") +
        #scale_x_date(labels = date_format("%aaaa/%mm")) +
        scale_x_continuous(breaks = seq(2000,2021,1) )+
        theme_bw() +
        scale_fill_brewer(palette = "RdBu", direction = -1) +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"))
    }
    
    
    ggplotly(p , tooltip = "text")
  })
  
  
  # Previdencia -------------------------------------------------------------
  
  # FILTROS
  
  data_filtro_prev<-reactive({
    data <- dados_prev %>% filter(ano >= input$data_prev[1] & ano <= input$data_prev[2])
    
    if ((length(input$grupo_p) != 0)){
      data <- data %>% filter(nogrupo %in% input$grupo_p)
    }
    else{
      data <- data %>% filter(nogrupo %in% unique(data$nogrupo))
    }
    
    data
  })
  
  # INFORMACOES
  
  output$res_total <- renderInfoBox({
    infoBox(
      "Resgate total (em reais)", sum(data_filtro_prev()$prev_resgates) ,
      color = "orange", fill = TRUE,icon=icon("money-check-alt")
    )
  })
  
  output$ben_total <- renderInfoBox({
    infoBox(
      "Benefícios totais (em reais)", sum(data_filtro_prev()$prev_beneficio) ,
      color = "orange", fill = TRUE,icon=icon("file-invoice-dollar")
    )
  })
  
  output$cont_total <- renderInfoBox({
    infoBox(
      "Contribuição total (em reais)", sum(data_filtro_prev()$prev_contribuicao) ,
      color = "orange", fill = TRUE,icon=icon("money-check-alt")
    )
  })
  
  # SERIE HISTORICA
  
  #contribuição
  data5_prev <- reactive({
    
    a <- unique(data_filtro_prev()$ano)
    sin <- c()
    
    for (i in a){
      df <- data_filtro_prev() %>% filter(ano == i)
      s <- sum(df$prev_contribuicao)
      sin <- c(sin, s)
    }
    
    
    data <- data.frame(a,sin)
    data
  })
  
  data5_prev_num <- reactive({
    
    a <- unique(data_filtro_prev()$ano)
    sin <- c()
    
    for (i in a){
      df <- data_filtro_prev() %>% filter(ano == i)
      s <- sum(df$prev_qtd_contruibuintes)
      sin <- c(sin, s)
    }
    
    
    data <- data.frame(a,sin)
    data
  })
  
  data5_prev_prop <- reactive({
    
    a <- unique(data_filtro_prev()$ano)
    sin <- c()
    
    for (i in a){
      df <- data_filtro_prev() %>% filter(ano == i)
      s <- sum(df$prev_contribuicao)/sum(df$prev_qtd_contruibuintes)
      sin <- c(sin, s)
    }
    
    
    data <- data.frame(a,sin)
    data
  })
  
  output$grafico_5<-renderPlotly({
    
    p <- data5_prev() %>% 
      ggplot(aes(x=a, y=sin, group=1,text = paste0('Ano: ', a, '\n', 'Valor total: ', round(sin,2) ))) +
      geom_line(size=1,colour="orange") + geom_point(colour="orange",size=2) +
      labs(x="Ano", y="Contribuição (reais)") +
      #scale_x_date(labels = date_format("%aaaa/%mm")) +
      scale_x_continuous(breaks = seq(2000,2021,1) )+
      theme_bw() +
      theme(axis.title.y=element_text(colour="black", size=12),
            axis.title.x = element_text(colour="black", size=12),
            axis.text = element_text(colour = "black", size=9.5),
            panel.border = element_blank(),
            axis.line = element_line(colour = "black"))
    
    if(input$escolha == "Nº Usuários"){
      p <- data5_prev_num() %>% 
        ggplot(aes(x=a, y=sin, group=1,text = paste0('Ano: ', a, '\n', 'Valor total: ', round(sin,2) ))) +
        geom_line(size=1,colour="orange") + geom_point(colour="orange",size=2) +
        labs(x="Ano", y="Quantidade de contribuintes") +
        #scale_x_date(labels = date_format("%aaaa/%mm")) +
        scale_x_continuous(breaks = seq(2000,2021,1) )+
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"))
    }
    
    if(input$escolha == "Proporção"){
      p <- data5_prev_prop() %>% 
        ggplot(aes(x=a, y=sin, group=1,text = paste0('Ano: ', a, '\n', 'Valor total: ', round(sin,2) ))) +
        geom_line(size=1,colour="orange") + geom_point(colour="orange",size=2) +
        labs(x="Ano", y="Contribuição (reais)/ nº de contribuintes") +
        #scale_x_date(labels = date_format("%aaaa/%mm")) +
        scale_x_continuous(breaks = seq(2000,2021,1) )+
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"))
    }
    
    
    ggplotly(p , tooltip = "text")
  })
  
  #benefício
  
  data6_prev <- reactive({
    
    a <- unique(data_filtro_prev()$ano)
    sin <- c()
    
    for (i in a){
      df <- data_filtro_prev() %>% filter(ano == i)
      s <- sum(df$prev_beneficio)
      sin <- c(sin, s)
    }
    
    
    data <- data.frame(a,sin)
    data
  })
  
  data6_prev_num <- reactive({
    
    a <- unique(data_filtro_prev()$ano)
    sin <- c()
    
    for (i in a){
      df <- data_filtro_prev() %>% filter(ano == i)
      s <- sum(df$prev_qtd_beneficiarios)
      sin <- c(sin, s)
    }
    
    
    data <- data.frame(a,sin)
    data
  })
  
  data6_prev_prop <- reactive({
    
    a <- unique(data_filtro_prev()$ano)
    sin <- c()
    
    for (i in a){
      df <- data_filtro_prev() %>% filter(ano == i)
      s <- sum(df$prev_beneficio)/sum(df$prev_qtd_beneficiarios)
      sin <- c(sin, s)
    }
    
    
    data <- data.frame(a,sin)
    data
  })
  
  output$grafico_6<-renderPlotly({
    
    p <- data6_prev() %>% 
      ggplot(aes(x=a, y=sin, group=1,text = paste0('Ano: ', a, '\n', 'Valor total: ', round(sin,2) ))) +
      geom_line(size=1,colour="orange") + geom_point(colour="orange",size=2) +
      labs(x="Ano", y="Benefício (reais)") +
      #scale_x_date(labels = date_format("%aaaa/%mm")) +
      scale_x_continuous(breaks = seq(2000,2021,1) )+
      theme_bw() +
      theme(axis.title.y=element_text(colour="black", size=12),
            axis.title.x = element_text(colour="black", size=12),
            axis.text = element_text(colour = "black", size=9.5),
            panel.border = element_blank(),
            axis.line = element_line(colour = "black"))
    
    if(input$escolha == "Nº Usuários"){
      p <- data6_prev_num() %>% 
        ggplot(aes(x=a, y=sin, group=1,text = paste0('Ano: ', a, '\n', 'Valor total: ', round(sin,2) ))) +
        geom_line(size=1,colour="orange") + geom_point(colour="orange",size=2) +
        labs(x="Ano", y="Quantidade de beneficiários") +
        #scale_x_date(labels = date_format("%aaaa/%mm")) +
        scale_x_continuous(breaks = seq(2000,2021,1) )+
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"))
    }
    
    if(input$escolha == "Proporção"){
      p <- data6_prev_prop() %>% 
        ggplot(aes(x=a, y=sin, group=1,text = paste0('Ano: ', a, '\n', 'Valor total: ', round(sin,2) ))) +
        geom_line(size=1,colour="orange") + geom_point(colour="orange",size=2) +
        labs(x="Ano", y="Benefício (reais)/ nº de beneficiários") +
        #scale_x_date(labels = date_format("%aaaa/%mm")) +
        scale_x_continuous(breaks = seq(2000,2021,1) )+
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"))
    }
    
    
    ggplotly(p , tooltip = "text")
  })
  
  #resgate
  
  data62_prev <- reactive({
    
    a <- unique(data_filtro_prev()$ano)
    sin <- c()
    
    for (i in a){
      df <- data_filtro_prev() %>% filter(ano == i)
      s <- sum(df$prev_resgates)
      sin <- c(sin, s)
    }
    
    
    data <- data.frame(a,sin)
    data
  })
  
  data62_prev_num <- reactive({
    
    a <- unique(data_filtro_prev()$ano)
    sin <- c()
    
    for (i in a){
      df <- data_filtro_prev() %>% filter(ano == i)
      s <- sum(df$prev_qtd_resgatantes)
      sin <- c(sin, s)
    }
    
    
    data <- data.frame(a,sin)
    data
  })
  
  data62_prev_prop <- reactive({
    
    a <- unique(data_filtro_prev()$ano)
    sin <- c()
    
    for (i in a){
      df <- data_filtro_prev() %>% filter(ano == i)
      s <- sum(df$prev_resgates)/sum(df$prev_qtd_resgatantes)
      sin <- c(sin, s)
    }
    
    
    data <- data.frame(a,sin)
    data
  })
  
  output$grafico_6_2<-renderPlotly({
    
    p <- data62_prev() %>% 
      ggplot(aes(x=a, y=sin, group=1,text = paste0('Ano: ', a, '\n', 'Valor total: ', round(sin,2) ))) +
      geom_line(size=1,colour="orange") + geom_point(colour="orange",size=2) +
      labs(x="Ano", y="Resgates (reais)") +
      #scale_x_date(labels = date_format("%aaaa/%mm")) +
      scale_x_continuous(breaks = seq(2000,2021,1) )+
      theme_bw() +
      theme(axis.title.y=element_text(colour="black", size=12),
            axis.title.x = element_text(colour="black", size=12),
            axis.text = element_text(colour = "black", size=9.5),
            panel.border = element_blank(),
            axis.line = element_line(colour = "black"))
    
    if(input$escolha == "Nº Usuários"){
      p <- data62_prev_num() %>% 
        ggplot(aes(x=a, y=sin, group=1,text = paste0('Ano: ', a, '\n', 'Valor total: ', round(sin,2) ))) +
        geom_line(size=1,colour="orange") + geom_point(colour="orange",size=2) +
        labs(x="Ano", y="Quantidade de resgatantes") +
        #scale_x_date(labels = date_format("%aaaa/%mm")) +
        scale_x_continuous(breaks = seq(2000,2021,1) )+
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"))
    }
    
    if(input$escolha == "Proporção"){
      p <- data62_prev_prop() %>% 
        ggplot(aes(x=a, y=sin, group=1,text = paste0('Ano: ', a, '\n', 'Valor total: ', round(sin,2) ))) +
        geom_line(size=1,colour="orange") + geom_point(colour="orange",size=2) +
        labs(x="Ano", y="Resgates (reais)/ nº de resgatantes") +
        #scale_x_date(labels = date_format("%aaaa/%mm")) +
        scale_x_continuous(breaks = seq(2000,2021,1) )+
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"))
    }
    
    ggplotly(p , tooltip = "text")
  })
  
  # GRAFICOS DE BARRAS - grupos
  
  # contribuicoes
 
  data7_prev <- reactive({
    
    grupos <- unique(data_filtro_prev()$nogrupo)
    cont <- c()
    
    for (i in grupos){
      df <- data_filtro_prev() %>% filter(nogrupo == i)
      p <- sum(df$prev_contribuicao)/length(df$prev_contribuicao)
      cont <- c(cont, p)
    }
    
    data <- data.frame(grupos,cont)
    
  })
  
  data7_prev_num <- reactive({
    
    grupos <- unique(data_filtro_prev()$nogrupo)
    cont <- c()
    
    for (i in grupos){
      df <- data_filtro_prev() %>% filter(nogrupo == i)
      p <- sum(df$prev_qtd_contruibuintes)/length(df$prev_qtd_contruibuintes)
      cont <- c(cont, p)
    }
    
    data <- data.frame(grupos,cont)
    
  })
  
  data7_prev_prop <- reactive({
    
    grupos <- unique(data_filtro_prev()$nogrupo)
    cont <- c()
    
    for (i in grupos){
      df <- data_filtro_prev() %>% filter(nogrupo == i)
      p <- sum(df$prev_contribuicao)/sum(df$prev_qtd_contruibuintes)
      cont <- c(cont, p)
    }
    
    data <- data.frame(grupos,cont)
    
  })
  

  
  output$grafico_7 <- renderPlotly({
    
    p <- data7_prev() %>% 
      ggplot(aes(x=reorder(grupos, cont), y=cont,text = paste0('Grupo ', grupos, '\n', 'Número: ', round(cont,2) ))) +
      geom_bar(stat="identity", fill="orange") +
      labs(x="Grupos", y="Contribuição (reais)") +
      theme_bw() +
      theme(axis.title.y=element_text(colour="black", size=12),
            axis.title.x = element_text(colour="black", size=12),
            axis.text = element_text(colour = "black", size=9.5),
            panel.border = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(angle = 90, hjust = 1))
    
    if(input$escolha == "Nº Usuários"){
      p <- data7_prev_num() %>% 
        ggplot(aes(x=reorder(grupos, cont), y=cont,text = paste0('Grupo ', grupos, '\n', 'Número: ', round(cont,2) ))) +
        geom_bar(stat="identity", fill="orange") +
        labs(x="Grupos", y="Nº de contribuintes") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.text.x = element_text(angle = 90, hjust = 1))
    }
    if(input$escolha == "Proporção"){
      p <- data7_prev_prop() %>% 
        ggplot(aes(x=reorder(grupos, cont), y=cont,text = paste0('Grupo ', grupos, '\n', 'Número: ', round(cont,2) ))) +
        geom_bar(stat="identity", fill="orange") +
        labs(x="Grupos", y="Contribuição (reais)/Nº de contribuintes") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.text.x = element_text(angle = 90, hjust = 1))
    }
    
    ggplotly(p , tooltip = "text")
  })
  
  # beneficios
  
  data8_prev <- reactive({
    
    grupos <- unique(data_filtro_prev()$nogrupo)
    ben <- c()
    
    for (i in grupos){
      df <- data_filtro_prev() %>% filter(nogrupo == i)
      p <- sum(df$prev_beneficio)/length(df$prev_beneficio)
      ben <- c(ben, p)
    }
    
    data <- data.frame(grupos,ben)
    
  })
  
  data8_prev_num <- reactive({
    
    grupos <- unique(data_filtro_prev()$nogrupo)
    ben <- c()
    
    for (i in grupos){
      df <- data_filtro_prev() %>% filter(nogrupo == i)
      p <- sum(df$prev_qtd_beneficiarios)/length(df$prev_qtd_beneficiarios)
      ben <- c(ben, p)
    }
    
    data <- data.frame(grupos,ben)
    
  })
  
  data8_prev_prop <- reactive({
    
    grupos <- unique(data_filtro_prev()$nogrupo)
    ben <- c()
    
    for (i in grupos){
      df <- data_filtro_prev() %>% filter(nogrupo == i)
      p <- sum(df$prev_beneficio)/sum(df$prev_qtd_beneficiarios)
      ben <- c(ben, p)
    }
    
    data <- data.frame(grupos,ben)
    
  })
  
  output$grafico_8 <- renderPlotly({
    
    p <- data8_prev() %>% 
      ggplot(aes(x=reorder(grupos, ben), y=ben,text = paste0('Grupo ', grupos, '\n', 'Número: ', round(ben,2) ))) +
      geom_bar(stat="identity", fill="orange") +
      labs(x="Grupos", y="Benefício (reais)") +
      theme_bw() +
      theme(axis.title.y=element_text(colour="black", size=12),
            axis.title.x = element_text(colour="black", size=12),
            axis.text = element_text(colour = "black", size=9.5),
            panel.border = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(angle = 90, hjust = 1))
    
    if(input$escolha == "Nº Usuários"){
      p <- data8_prev_num() %>% 
        ggplot(aes(x=reorder(grupos, ben), y=ben,text = paste0('Grupo ', grupos, '\n', 'Número: ', round(ben,2) ))) +
        geom_bar(stat="identity", fill="orange") +
        labs(x="Grupos", y="Nº de beneficiários") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.text.x = element_text(angle = 90, hjust = 1))
    }
    
    if(input$escolha == "Proporção"){
      p <- data8_prev_prop() %>% 
        ggplot(aes(x=reorder(grupos, ben), y=ben,text = paste0('Grupo ', grupos, '\n', 'Número: ', round(ben,2) ))) +
        geom_bar(stat="identity", fill="orange") +
        labs(x="Grupos", y="Benefício (reais)/Nº de beneficiários") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.text.x = element_text(angle = 90, hjust = 1))
    }
    
    ggplotly(p , tooltip = "text")
  })
  
  # resgates
  
  data9_prev <- reactive({
    
    grupos <- unique(data_filtro_prev()$nogrupo)
    res <- c()
    
    for (i in grupos){
      df <- data_filtro_prev() %>% filter(nogrupo == i)
      p <- sum(df$prev_resgates)/length(df$prev_resgates)
      res <- c(res, p)
    }
    
    data <- data.frame(grupos,res)
    
  })
  
  data9_prev_num <- reactive({
    
    grupos <- unique(data_filtro_prev()$nogrupo)
    res <- c()
    
    for (i in grupos){
      df <- data_filtro_prev() %>% filter(nogrupo == i)
      p <- sum(df$prev_qtd_resgatantes)/length(df$prev_qtd_resgatantes)
      res <- c(res, p)
    }
    
    data <- data.frame(grupos,res)
    
  })
  
  data9_prev_prop <- reactive({
    
    grupos <- unique(data_filtro_prev()$nogrupo)
    res <- c()
    
    for (i in grupos){
      df <- data_filtro_prev() %>% filter(nogrupo == i)
      p <- sum(df$prev_resgates)/sum(df$prev_qtd_resgatantes)
      res <- c(res, p)
    }
    
    data <- data.frame(grupos,res)
    
  })
  
  output$grafico_9 <- renderPlotly({
   
    p <- data9_prev() %>% 
      ggplot(aes(x=reorder(grupos, res), y=res,text = paste0('Grupo ', grupos, '\n', 'Número: ', round(res,2) ))) +
      geom_bar(stat="identity", fill="orange") +
      labs(x="Grupos", y="Resgate (reais)") +
      theme_bw() +
      theme(axis.title.y=element_text(colour="black", size=12),
            axis.title.x = element_text(colour="black", size=12),
            axis.text = element_text(colour = "black", size=9.5),
            panel.border = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(angle = 90, hjust = 1))
    
    if(input$escolha == "Nº Usuários"){
      p <- data9_prev_num() %>% 
        ggplot(aes(x=reorder(grupos, res), y=res,text = paste0('Grupo ', grupos, '\n', 'Número: ', round(res,2) ))) +
        geom_bar(stat="identity", fill="orange") +
        labs(x="Grupos", y="Nº de resgatantes") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.text.x = element_text(angle = 90, hjust = 1))
    }
    if(input$escolha == "Proporção"){
      p <- data9_prev_prop() %>% 
        ggplot(aes(x=reorder(grupos, res), y=res,text = paste0('Grupo ', grupos, '\n', 'Número: ', round(res,2) ))) +
        geom_bar(stat="identity", fill="orange") +
        labs(x="Grupos", y="Resgate (reais)/Nº de resgatantes") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.text.x = element_text(angle = 90, hjust = 1))
    }
    
    ggplotly(p , tooltip = "text")
  })
  
})

shinyApp(ui, server)
    

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

# Dados -------------------------------------------------------------------

setwd("C:\\Users\\helen\\Desktop\\Documentos\\estatistica\\semestres\\6 semestre\\lab2\\app_1")

dados_prev <- read.csv("DADOS_PREV.csv",sep = ";")

dados_prev$nogrupo <- gsub(" ", "", levels(dados_prev$nogrupo), fixed = TRUE)

dados_seg <- read.csv("DADOS_SEG.csv",sep = ";")

dados_seg$nogrupo <- gsub(" ", "", levels(dados_seg$nogrupo), fixed = TRUE)

dados_seg$granome <- gsub(" ", "", levels(dados_seg$granome), fixed = TRUE)

dados_seg$premio <- dados_seg$premio/100000

dados_seg$sinistro <- dados_seg$sinistro/100000


# especificacoes

cor_graficos <- "#00688B"

# App ---------------------------------------------------------------------

sidebar<-dashboardSidebar(
  sidebarMenu(id="teste",
              menuItem("Página Inicial", tabName = "geral", icon = icon("balance-scale-left")),
              menuItem("Seguros", tabName = "seg", icon = icon("donate")),
              menuItem("Previdência", tabName = "prev", icon = icon("comments-dollar")),
              menuItem("Capitalização", tabName = "cap", icon = icon("money-bill-alt"))
  ),
  

  
  conditionalPanel(
    condition = "input.teste == 'geral'",
    selectInput("filtro_pag_inicial", "Filtro:", 
                choices = c("Seguros","Previdência","Capitalização"),
                selected = "Seguros"),
    
    selectInput('local', 'Escolha a(s) localidade(s):', 
                choices = c("Local1","Local2","Local3","Todas"),
                selected = "Todas",
                multiple = TRUE
    )
  ),
  
  conditionalPanel(
    condition = "input.teste == 'prev'",
    selectInput('grupo_p', 'Escolha o(s) grupo(s):', 
                choices = c(unique(dados_prev$nogrupo)),
                multiple = TRUE
    ),
    
    selectInput('escolha', 'Escolha quais dados deseja visualizar:', 
                choices = c("Números absolutos","Usuários","Proporção"),
                multiple = FALSE, selected = "Números absolutos"
    ),
    
    sliderTextInput("data_prev", "Período:", 
                    choices = sort(unique(dados_prev$ano)),
                    selected = c((min(dados_prev$ano)), (max(dados_prev$ano)))
    )
    
  ),
  
  conditionalPanel(
    condition = "input.teste == 'seg'",
    selectInput('grupo', 'Escolha a(s) organização(ões):', 
                choices = c(unique(dados_seg$nogrupo)),
                multiple = TRUE
    ),
    
    
    sliderTextInput("data_seg", "Período:", 
                    choices = sort(unique(dados_seg$anos)),
                    selected = c((min(dados_seg$anos)), (max(dados_seg$anos)))
    ),
    
    selectInput('tipo', 'Tipo de seguro:', 
                choices = c(unique(dados_seg$granome)),
                multiple = TRUE
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
              column(width = 3,
                     
                     
                     infoBoxOutput("info_1", width = 12),
                     infoBoxOutput("info_2", width = 12),
                     infoBoxOutput("info_3", width = 12)
                    
              )
          
              #box(width = 4,
              #    title = "Formação de professor",
              #    plotlyOutput("professores_ativ2"))
              # MAPA DO BRASIL
            )            
    ),
    
    
    # Seguros --------------------------------------------------
    
    tabItem(tabName = "seg",
            
            fluidRow(
              
              column(width = 8,
                     
                     infoBoxOutput("pre_total", width = 4),
                     infoBoxOutput("sin_total", width = 4)
              ),
              
              
              # grafico de barras
              
              tabBox(
                title = "Organizações (médias anuais)",
                id = "tabset0", width = 12,
                tabPanel("Prêmio", plotlyOutput("grafico_1")),
                tabPanel("Sinistro", plotlyOutput("grafico_2"))
                
              ),
              
              # grafico de linhas
              
              tabBox(
                title = "Série histórica",
                id = "tabset1", width = 12,
                tabPanel("Prêmio", plotlyOutput("grafico_3")),
                tabPanel("Sinistro", plotlyOutput("grafico_4"))
              )
              
            )             
    ),
    
    # Previdencia -------------------------------------------------------------
    
    tabItem(tabName = "prev",
            fluidRow(
              
              # grafico de colunas
              
              tabBox(
                title = "Valores",
                id = "tabset2", width = 12,
                tabPanel("Contribuições", plotlyOutput("grafico_5", height = "800px")),
                tabPanel("Benefícios", plotlyOutput("grafico_6", height = "800px")),
                tabPanel("Resgates", plotlyOutput("grafico_6_2", height = "800px"))
                
              ),
              
              
              tabBox(
                title = "Comparação entre as entidades",
                id = "tabset3", width = 12,
                tabPanel("Contribuições", plotlyOutput("grafico_7")),
                tabPanel("Benefícios", plotlyOutput("grafico_8")),
                tabPanel("Resgates", plotlyOutput("grafico_9"))
              )
            )
    ),
    
    # Capitalizacao -------------------------------------------------------------
    
    tabItem(tabName = "cap",
            
            fluidRow(
              
              # grafico de barras
              
              tabBox(
                title = "Comparação entre as empresas",
                id = "tabset4", width = 12,
                tabPanel("Geral", plotlyOutput("grafico_10", height = "665px")),
                tabPanel("Receitas e resgates", plotlyOutput("grafico_11", height = "665px")),
                tabPanel("Sorteios", plotlyOutput("grafico_12", height = "665px"))
                
              ),
              
              # grafico de linhas
              
              tabBox(
                title = "Série histórica",
                id = "tabset5", width = 12,
                tabPanel("Receitas e resgates", plotlyOutput("grafico_13")),
                tabPanel("Sorteios", plotlyOutput("grafico_14"))
              )
              
            )             
    )
  )
)

ui<-dashboardPage(sidebar = sidebar,header = header,body= body)

server <- function(input, output) {
  
  # Pagina Inicial -------------------------------------------------------------------------------
  
  
  output$info_1 <- renderInfoBox({
    infoBox(
      "Informação 1", 15 ,
      color = "purple", fill = TRUE,icon=icon("user-friends")
    )
  })
  
  output$info_2 <- renderInfoBox({
    infoBox(
      "Informação 2", 16 ,
      color = "purple", fill = TRUE,icon=icon("user-friends")
    )
  })
  
  output$info_3 <- renderInfoBox({
    infoBox(
      "Informação 3", 17 ,
      color = "purple", fill = TRUE,icon=icon("user-friends")
    )
  })
  
  
  # mapa
  
#  output$formandos_prop <-renderPlotly({
#    p <- dados%>%
#     # mapa
#    ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
#  })
  
  
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
      "Prêmio total (em milhões)", round(sum(data_filtro_seg()$premio)/1000000,4) ,
      color = "purple", fill = TRUE,icon=icon("file-invoice-dollar")
    )
  })
  
  output$sin_total <- renderInfoBox({
    infoBox(
      "Sinistro total (em milhões)", round(sum(data_filtro_seg()$sinistro)/1000000,4) ,
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
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"))
    }
   
    
    ggplotly(p , tooltip = "text")
  })
  
  
  # Previdencia -------------------------------------------------------------
  
  #output$grafico_5 <- renderPlotly({
  
  #    p <- serv_filtrado() %>% 
  #      filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>% 
  #      mutate(disciplina = fct_reorder(disciplina, mencao, function(.x) mean(.x %in% c("SR", "II", "MI")))) %>% 
  #      ggplot() + 
  #      geom_bar(aes(x = fct_rev(disciplina), fill = fct_rev(mencao), text = paste0("Número: ",..count..))) + 
  #      labs(x="", y="Número de alunos", fill = "") +
  #      scale_fill_brewer(palette = "RdBu", direction = -1) + 
  #      coord_flip()
  #  
  #  ggplotly(p , tooltip = "text")
  #})
  
  # Capitalizacao -------------------------------------------------------------
  
  #output$grafico_10 <- renderPlotly({
  
  #    p <- serv_filtrado() %>% 
  #      filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>% 
  #      mutate(disciplina = fct_reorder(disciplina, mencao, function(.x) mean(.x %in% c("SR", "II", "MI")))) %>% 
  #      ggplot() + 
  #      geom_bar(aes(x = fct_rev(disciplina), fill = fct_rev(mencao), text = paste0("Número: ",..count..))) + 
  #      labs(x="", y="Número de alunos", fill = "") +
  #      scale_fill_brewer(palette = "RdBu", direction = -1) + 
  #      coord_flip()
  #  
  #  ggplotly(p , tooltip = "text")
  #})
  
}

shinyApp(ui, server)

    

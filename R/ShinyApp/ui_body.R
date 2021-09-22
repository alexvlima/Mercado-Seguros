body <-
  
  dashboardBody(
  
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
              
              
              # Gráfico de barras
              
              tabBox(
                title = "Organizações (médias anuais)",
                id = "tabset0", width = 12,
                tabPanel("Prêmio (em reais)", plotlyOutput("grafico_1")),
                tabPanel("Sinistro (em reais)", plotlyOutput("grafico_2"))
                
              ),
              
              # Gráfico de linhas
              
              tabBox(
                title = "Série histórica",
                id = "tabset1", width = 12,
                tabPanel("Prêmio (em reais)", plotlyOutput("grafico_3")),
                tabPanel("Sinistro (em reais)", plotlyOutput("grafico_4"))
              )
              
            )             
    ),
    
    # Previdência -------------------------------------------------------------
    
    tabItem(tabName = "prev",
            fluidRow(
              
              column(width = 12,
                     
                     infoBoxOutput("cont_total", width = 4),
                     infoBoxOutput("ben_total", width = 4),
                     infoBoxOutput("res_total", width = 4),
              ),
              
              # Gráfico de colunas
              
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

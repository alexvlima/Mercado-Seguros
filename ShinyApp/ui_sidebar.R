
sidebar <- 
  dashboardSidebar(
    sidebarMenu(id="teste",
                menuItem("Página Inicial", tabName = "geral", icon = icon("balance-scale-left")),
                menuItem("Seguros", tabName = "seg", icon = icon("donate")),
                menuItem("Previdência", tabName = "prev", icon = icon("comments-dollar"))
    ),
    
    conditionalPanel(
      condition = "input.teste == 'seg'",
      selectInput('grupo', 'Escolha a(s) organização(ões):', 
                  choices = c(unique(base_seguros$nogrupo)),
                  multiple = TRUE
      ),
      
      selectInput('tipo', 'Grupo do Ramo:', 
                  choices = c(unique(base_seguros$gr_ramo)),
                  multiple = TRUE
      ),
      
      sliderTextInput("data_seg", "Período:", 
                      choices = sort(unique(base_seguros$ano)),
                      selected = c((min(base_seguros$ano)), (max(base_seguros$ano)))
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


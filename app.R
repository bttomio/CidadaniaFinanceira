# Carregar as bibliotecas necessárias
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(readxl)
library(shinydashboard)

CT <- read_xlsx("CT.xlsx")
VAR_PROD <- read_xlsx("VAR_PROD.xlsx")

# Definir a interface do usuário
ui <- navbarPage(
  title = "Cidadania Financeira [FURB]",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")  # Link the CSS file
  ),
  
  # Página inicial (Capa) sem seleção de página
  tabPanel("Início",
           fluidPage(
             titlePanel("Bem-vindo(a)!"),
             
             mainPanel(
               h4("Esta página divulga os dados coletados pelo projeto de extensão Cidadania Financeira da ",
                  a("Universidade de Blumenau (FURB).", href = "https://www.furb.br")),
               p("Escolha uma das opções acima para acessar as páginas de dados filtrados."),
               tags$div(tags$img(src = "logo.png", class = "logo")),
               
               # Caixa para o último valor da cesta básica de Blumenau
               #uiOutput("caixa_cesta_blumenau")
             )
           )),
  
  # Página de "Cesta Básica - Variação Mensal"
  tabPanel("Cesta Básica",
           fluidPage(
             # Texto destacado
             tags$div(style = "font-size: 20px; font-weight: bold; color: #1a1a1a; margin-bottom: 20px;", 
                      "Para visualizar um gráfico, selecione: Cidade."),
             
             titlePanel("Cesta Básica"),
             
             sidebarLayout(
               sidebarPanel(
                 selectInput("cidade", "Selecione a Cidade", choices = c("Todos", unique(CT$Cidade)), selected = "Todos"),
                 selectInput("mes", "Selecione o Mês", choices = c("Todos", "Janeiro", "Fevereiro", "Março", "Abril", "Maio", 
                                                                   "Junho", "Julho", "Agosto", "Setembro", "Outubro", 
                                                                   "Novembro", "Dezembro"), selected = "Todos"),
                 selectInput("ano", "Selecione o Ano", choices = c("Todos", unique(CT$Ano)), selected = "Todos")
               ),
               mainPanel(
                 box(
                   title = "Tabela de Dados",
                   status = "primary",
                   solidHeader = TRUE,
                   width = 12,
                   DTOutput("tabela_cesta")
                 ),
                 
                 br(),
                 
                 # Gráfico da variação da Cesta Básica
                 conditionalPanel(
                   condition = "input.cidade != 'Todos'",
                   box(
                     title = "Gráfico de Variação da Cesta Básica",
                     status = "success",
                     solidHeader = TRUE,
                     width = 12,
                     plotOutput("grafico_cesta")
                   )
                 )
               )
             )
           )),
  
  # Página de "Dados dos Produtos"
  tabPanel("Produtos da Cesta",
           fluidPage(
             # Texto destacado
             tags$div(style = "font-size: 20px; font-weight: bold; color: #1a1a1a; margin-bottom: 20px;", 
                      "Para visualizar um gráfico, selecione: Produto e Cidade."),
             
             titlePanel("Produtos da Cesta"),
             
             sidebarLayout(
               sidebarPanel(
                 selectInput("produto", "Selecione o Produto", choices = c("Todos", unique(VAR_PROD$Produto)), selected = "Todos"),
                 selectInput("cidade_produto", "Selecione a Cidade", choices = c("Todos", unique(VAR_PROD$Cidade)), selected = "Todos"),
                 selectInput("mes_produto", "Selecione o Mês", choices = c("Todos", "Janeiro", "Fevereiro", "Março", "Abril", "Maio", 
                                                                           "Junho", "Julho", "Agosto", "Setembro", "Outubro", 
                                                                           "Novembro", "Dezembro"), selected = "Todos"),
                 selectInput("ano_produto", "Selecione o Ano", choices = c("Todos", unique(VAR_PROD$Ano)), selected = "Todos")
               ),
               mainPanel(
                 box(
                   title = "Tabela de Dados dos Produtos da Cesta",
                   status = "primary",
                   solidHeader = TRUE,
                   width = 12,
                   DTOutput("tabela_produtos")
                 ),
                 
                 br(),
                 
                 # Gráfico da variação dos produtos
                 conditionalPanel(
                   condition = "input.produto != 'Todos' && input.cidade_produto != 'Todos'",
                   box(
                     title = "Gráfico de Variação dos Produtos",
                     status = "success",
                     solidHeader = TRUE,
                     width = 12,
                     plotOutput("grafico_produtos")
                   )
                 )
               )
             )
           )),
  # Página de "Variação Anual"
  tabPanel("Variação Anual",
           fluidPage(
             titlePanel("Variação Anual da Cesta Básica"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("cidade_anual", "Selecione a Cidade", choices = c("Todos", unique(CT$Cidade)), selected = "Todos"),
                 selectInput("ano_anual", "Selecione o Ano", choices = c("Todos", unique(CT$Ano)), selected = "Todos")
               ),
               mainPanel(
                 box(
                   title = "Tabela de Variação Anual",
                   status = "primary",
                   solidHeader = TRUE,
                   width = 12,
                   DTOutput("tabela_variacao_anual")
                 ),
                 br(),
                 box(
                   title = "Gráfico de Variação Anual",
                   status = "success",
                   solidHeader = TRUE,
                   width = 12,
                   plotOutput("grafico_variacao_anual")
                 )
               )
             )
           ))
)

# Definir a lógica do servidor
server <- function(input, output, session) {
  
  # Converter a coluna 'Período' para o formato Date, se necessário
  CT$Período <- as.Date(CT$Período)
  
  # Filtrar o último valor da cesta básica de Blumenau
  ultimo_valor_cesta <- reactive({
    dados <- CT %>%
      filter(Cidade == "Blumenau") %>%  # Filtrar por Blumenau
      arrange(desc(Período)) %>%        # Ordenar por período (do mais recente para o mais antigo)
      slice(1)                          # Pegar o primeiro valor (último período)
    
    # Verificar se há dados e se as colunas necessárias existem
    if (nrow(dados) == 0 || all(is.na(dados$Cesta)) || all(is.na(dados$`Variação (%)`))) {
      return(NULL)  # Retorna NULL se não houver dados válidos
    } else {
      return(dados)
    }
  })
  
  # Renderizar a caixa na página inicial
  output$caixa_cesta_blumenau <- renderUI({
    dados <- ultimo_valor_cesta()
    
    # Verificar se há dados
    if (is.null(dados)) {
      return(tags$div("Nenhum dado disponível para Blumenau.", class = "caixa-cesta"))
    }
    
    # Extrair o valor e a variação
    valor <- dados$Cesta
    variacao <- dados$`Variação (%)`
    
    # Verificar se os valores são NA
    if (is.na(valor) || is.na(variacao)) {
      return(tags$div("Dados incompletos para Blumenau.", class = "caixa-cesta"))
    }
    
    # Determinar a classe CSS com base na variação
    classe_cor <- ifelse(variacao >= 0, "caixa-positiva", "caixa-negativa")
    
    # Criar o conteúdo da caixa
    tags$div(
      class = paste("caixa-cesta", classe_cor),
      paste("Último valor da Cesta Básica em Blumenau: R$", round(valor, 2)),
      br(),
      paste("Variação: ", round(variacao, 2), "%")
    )
  })
  
  # Garantir que a coluna 'Mês' seja tratada como um fator ordenado cronologicamente
  CT$Mês <- factor(CT$Mês, levels = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", 
                                      "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"), ordered = TRUE)
  
  VAR_PROD$Mês <- factor(VAR_PROD$Mês, levels = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", 
                                                  "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"), ordered = TRUE)
  
  # Filtrar dados da Cesta Básica com base nas seleções do usuário
  dados_filtrados <- reactive({
    CT %>%
      filter(
        (input$cidade == "Todos" | Cidade == input$cidade),
        (input$mes == "Todos" | Mês == input$mes),
        (input$ano == "Todos" | Ano == input$ano)
      )
  })
  
  # Filtrar dados dos Produtos com base nas seleções do usuário
  dados_produtos_filtrados <- reactive({
    VAR_PROD %>%
      filter(
        (input$produto == "Todos" | Produto == input$produto),
        (input$cidade_produto == "Todos" | Cidade == input$cidade_produto),
        (input$mes_produto == "Todos" | Mês == input$mes_produto),
        (input$ano_produto == "Todos" | Ano == input$ano_produto)
      )
  })
  
  # Renderizar a tabela de Cesta Básica
  output$tabela_cesta <- renderDT({
    datatable(
      dados_filtrados(),  # Use the full data
      options = list(
        scrollY = "300px",  # Define the height of the scrollable area
        scrollCollapse = TRUE,  # Collapse the table if there are few rows
        paging = FALSE,  # Disable pagination
        order = list(list(0, 'asc')),  # Ensure the first column ('Mês') is ordered ascending
        columnDefs = list(
          list(targets = which(names(dados_filtrados()) == "Período"), visible = FALSE)  # Hide the 'Período' column
        )
      )
    ) %>%
      formatStyle(
        'Variação (%)',  # Style the 'Variação (%)' column
        color = styleInterval(0, c('red', 'black'))  # Color negative numbers red, positive black
      ) %>%
      formatRound(
        c("Cesta", "Variação (%)"),  # Format 'Cesta' and 'Variação (%)' columns
        digits = 2  # Round to 2 decimal places
      )
  })
  
  # Renderizar a tabela de Dados dos Produtos
  output$tabela_produtos <- renderDT({
    datatable(
      dados_produtos_filtrados(),  # Use the full data
      options = list(
        scrollY = "300px",  # Define the height of the scrollable area
        scrollCollapse = TRUE,  # Collapse the table if there are few rows
        paging = FALSE,  # Disable pagination
        order = list(list(0, 'asc')),  # Ensure the first column ('Mês') is ordered ascending
        columnDefs = list(
          list(targets = which(names(dados_produtos_filtrados()) == "Período"), visible = FALSE)  # Hide the 'Período' column
        )
      )
    ) %>%
      formatStyle(
        'Variação (%)',  # Style the 'Variação (%)' column
        color = styleInterval(0, c('red', 'black'))  # Color negative numbers red, positive black
      ) %>%
      formatRound(
        c("Média (produto)", "Variação (%)"),  # Format 'Média (produto)' and 'Variação (%)' columns
        digits = 2  # Round to 2 decimal places
      )
  })
  
  library(ggrepel)
  library(scales)
  
  # Renderizar o gráfico de Variação Mensal da Cesta Básica
  output$grafico_cesta <- renderPlot({
    dados <- dados_filtrados()
    
    # Verifica se os dados estão vazios antes de tentar criar o gráfico
    req(nrow(dados) > 0)
    
    # Caso a coluna 'Período' ainda não esteja no formato Date, convertemos ela
    dados$Período <- as.Date(dados$Período)
    
    # Adicionar uma coluna de cor para os rótulos (preto para positivo, vermelho para negativo)
    dados$label_color <- ifelse(dados$`Variação (%)` >= 0, "black", "red")
    
    ggplot(dados, aes(x = Período, y = `Variação (%)`, group = Cidade, color = Cidade)) +
      geom_line(colour = "blue") +
      geom_point(colour = "blue") +
      geom_text_repel(aes(label = paste0(round(`Variação (%)`, 2), "%"), color = label_color),
                      size = 4,
                      nudge_y = ifelse(dados$`Variação (%)` >= 0, 0.2, -0.2),
                      max.overlaps = 20,
                      box.padding = 0.35,
                      point.padding = 0.5,
                      segment.size = 0.2,
                      direction = "both",
                      force = 1,
                      show.legend = FALSE,
                      color = dados$label_color) +
      geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 1) +
      labs(title = paste("Variação Percentual da Cesta Básica em", input$cidade), 
           x = "Período", 
           y = "Variação (%)") +
      scale_y_continuous(labels = label_percent(scale = 1)) +
      scale_x_date(labels = scales::date_format("%b/%Y", locale = "pt_BR"), breaks = scales::date_breaks("1 month")) +  # Format the x-axis to show Month/Year
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        legend.position = "none",
        panel.border = element_rect(color = "black", size = 1)
      )
  })
  
  # Renderizar o gráfico de Variação Mensal dos Produtos
  output$grafico_produtos <- renderPlot({
    dados_produtos <- dados_produtos_filtrados()
    
    # Verifica se os dados dos produtos estão vazios antes de tentar criar o gráfico
    req(nrow(dados_produtos) > 0)
    
    # Caso a coluna 'Período' ainda não esteja no formato Date, convertemos ela
    dados_produtos$Período <- as.Date(dados_produtos$Período)
    
    # Adicionar uma coluna de cor para os rótulos (preto para positivo, vermelho para negativo)
    dados_produtos$label_color <- ifelse(dados_produtos$`Variação (%)` >= 0, "black", "red")
    
    ggplot(dados_produtos, aes(x = Período, y = `Variação (%)`, group = Produto, color = Produto)) +
      geom_line(colour = "blue") +
      geom_point(colour = "blue") +
      geom_text_repel(aes(label = paste0(round(`Variação (%)`, 2), "%"), color = label_color),
                      size = 4,
                      nudge_y = ifelse(dados_produtos$`Variação (%)` >= 0, 0.2, -0.2),
                      max.overlaps = 20,
                      box.padding = 0.35,
                      point.padding = 0.5,
                      segment.size = 0.2,
                      direction = "both",
                      force = 1,
                      show.legend = FALSE,
                      color = dados_produtos$label_color) +
      geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 1) +
      labs(title = paste("Variação do Produto:", input$produto), x = "Período", y = "Variação (%)") +
      scale_y_continuous(labels = label_percent(scale = 1)) +
      scale_x_date(labels = scales::date_format("%b/%Y", locale = "pt_BR"), breaks = scales::date_breaks("1 month")) +  # Format the x-axis to show Month/Year
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        legend.position = "none",
        panel.border = element_rect(color = "black", size = 1)
      )
  })
  # Filtrar dados da Variação Anual
  dados_variacao_anual <- reactive({
    CT %>%
      filter(
        (input$cidade_anual == "Todos" | Cidade == input$cidade_anual),
        (input$ano_anual == "Todos" | Ano == input$ano_anual)
      ) %>%
      group_by(Ano, Cidade) %>%
      summarise(Variação_Anual = mean(`Variação (%)`, na.rm = TRUE))
  })
  
  # Renderizar a tabela de Variação Anual
  output$tabela_variacao_anual <- renderDT({
    datatable(
      dados_variacao_anual(),
      options = list(scrollY = "300px", scrollCollapse = TRUE, paging = FALSE)
    ) %>%
      formatRound("Variação_Anual", digits = 2)
  })
  
  # Renderizar o gráfico de Variação Anual
  output$grafico_variacao_anual <- renderPlot({
    dados <- dados_variacao_anual()
    req(nrow(dados) > 0)
    ggplot(dados, aes(x = Ano, y = Variação_Anual, fill = Cidade)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = round(Variação_Anual, 2)), vjust = -0.5, size = 5) +
      labs(title = "Variação Anual da Cesta Básica", x = "Ano", y = "Variação (%)") +
      theme_minimal()
  })
}

# Executar o aplicativo Shiny
shinyApp(ui = ui, server = server)


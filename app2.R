# Carregar bibliotecas
library(shiny)
library(shinyWidgets)
library(DT)
library(dplyr)
library(ggplot2)
library(readxl)
library(plotly)
library(bslib)
library(ggrepel)
library(scales)

# Ler os dados
CT <- read_xlsx("CT.xlsx")
VAR_PROD <- read_xlsx("VAR_PROD.xlsx")

# Ordenar os meses corretamente
meses_ordem <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", 
                 "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")

CT$Mês <- factor(CT$Mês, levels = meses_ordem, ordered = TRUE)
VAR_PROD$Mês <- factor(VAR_PROD$Mês, levels = meses_ordem, ordered = TRUE)

# Interface moderna com bslib
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#0073B7",
    base_font = font_google("Inter")
  ),
  
  navbarPage(
    title = div(icon("chart-line"), "Cidadania Financeira [FURB]"),
    
    tabPanel("Início",
             fluidPage(
               titlePanel("Bem-vindo(a)!"),
               h4("Esta página divulga os dados coletados pelo projeto de extensão Cidadania Financeira da ", 
                  a("Universidade Regional de Blumenau (FURB)", href = "https://www.furb.br", target = "_blank")),
               p("Os dados referem-se aos preços de produtos da cesta básica nos municípios de Blumenau e Gaspar, 
                 coletados semanalmente por estudantes da disciplina de Economia Brasileira. 
                 As coletas são realizadas nos sites dos mercados Angeloni, Bistek e Komprão."),
               p("Os dados são atualizados automaticamente sempre que os arquivos forem substituídos."),
               p(uiOutput("ultima_atualizacao")),
               img(src = "logo.png", height = "150px")
             )
    ),
    
    tabPanel("Cesta Básica",
             fluidPage(
               h3("Cesta Básica - Variação Mensal"),
               pickerInput("cidade", "Selecione a Cidade", choices = c("Todos", unique(CT$Cidade)), selected = "Todos"),
               pickerInput("mes", "Selecione o Mês", choices = c("Todos", meses_ordem), selected = "Todos"),
               pickerInput("ano", "Selecione o Ano", choices = c("Todos", unique(CT$Ano)), selected = "Todos"),
               br(),
               h4("Tabela de Dados"),
               DTOutput("tabela_cesta"),
               br(),
               conditionalPanel(
                 condition = "input.cidade != 'Todos'",
                 h4("Gráfico Interativo de Variação da Cesta"),
                 plotlyOutput("grafico_cesta")
               )
             )
    ),
    
    tabPanel("Produtos da Cesta",
             fluidPage(
               h3("Produtos da Cesta"),
               pickerInput("produto", "Selecione o Produto", choices = c("Todos", unique(VAR_PROD$Produto)), selected = "Todos"),
               pickerInput("cidade_produto", "Selecione a Cidade", choices = c("Todos", unique(VAR_PROD$Cidade)), selected = "Todos"),
               pickerInput("mes_produto", "Selecione o Mês", choices = c("Todos", meses_ordem), selected = "Todos"),
               pickerInput("ano_produto", "Selecione o Ano", choices = c("Todos", unique(VAR_PROD$Ano)), selected = "Todos"),
               br(),
               h4("Tabela de Dados dos Produtos"),
               DTOutput("tabela_produtos"),
               br(),
               conditionalPanel(
                 condition = "input.produto != 'Todos' && input.cidade_produto != 'Todos'",
                 h4("Gráfico Interativo do Produto"),
                 plotlyOutput("grafico_produtos")
               )
             )
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  # Última atualização
  output$ultima_atualizacao <- renderUI({
    strong(paste("Última atualização:", format(Sys.Date(), "%d/%m/%Y")))
  })
  
  # Filtragens
  dados_filtrados <- reactive({
    CT %>%
      filter(
        (input$cidade == "Todos" | Cidade == input$cidade),
        (input$mes == "Todos" | Mês == input$mes),
        (input$ano == "Todos" | Ano == input$ano)
      )
  })
  
  dados_produtos_filtrados <- reactive({
    VAR_PROD %>%
      filter(
        (input$produto == "Todos" | Produto == input$produto),
        (input$cidade_produto == "Todos" | Cidade == input$cidade_produto),
        (input$mes_produto == "Todos" | Mês == input$mes_produto),
        (input$ano_produto == "Todos" | Ano == input$ano_produto)
      )
  })
  
  # Tabela da cesta com filtros no topo
  output$tabela_cesta <- renderDT({
    datatable(
      dados_filtrados(),
      filter = "top",
      options = list(scrollY = "300px", paging = FALSE),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Variação (%)',
        color = styleInterval(0, c("red", "black"))
      ) %>%
      formatRound(c("Cesta", "Variação (%)"), digits = 2)
  })
  
  # Tabela dos produtos com filtros no topo
  output$tabela_produtos <- renderDT({
    datatable(
      dados_produtos_filtrados(),
      filter = "top",
      options = list(scrollY = "300px", paging = FALSE),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Variação (%)',
        color = styleInterval(0, c("red", "black"))
      ) %>%
      formatRound(c("Média (produto)", "Variação (%)"), digits = 2)
  })
  
  # Gráfico interativo da cesta básica
  output$grafico_cesta <- renderPlotly({
    dados <- dados_filtrados()
    req(nrow(dados) > 0)
    
    dados$Período <- as.Date(dados$Período)
    
    p <- ggplot(dados, aes(x = Período, y = `Variação (%)`, text = paste0("Valor: ", round(`Variação (%)`, 2), "%"))) +
      geom_line(color = "#0073B7") +
      geom_point(color = "#0073B7") +
      geom_hline(yintercept = 0, color = "black") +
      labs(title = paste("Variação da Cesta em", input$cidade), x = "", y = "Variação (%)") +
      scale_y_continuous(labels = label_percent(scale = 1)) +
      scale_x_date(labels = date_format("%b/%Y", locale = "pt_BR")) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Gráfico interativo dos produtos
  output$grafico_produtos <- renderPlotly({
    dados <- dados_produtos_filtrados()
    req(nrow(dados) > 0)
    
    dados$Período <- as.Date(dados$Período)
    
    p <- ggplot(dados, aes(x = Período, y = `Variação (%)`, text = paste0("Valor: ", round(`Variação (%)`, 2), "%"))) +
      geom_line(color = "#28a745") +
      geom_point(color = "#28a745") +
      geom_hline(yintercept = 0, color = "black") +
      labs(title = paste("Variação do Produto:", input$produto), x = "", y = "Variação (%)") +
      scale_y_continuous(labels = label_percent(scale = 1)) +
      scale_x_date(labels = date_format("%b/%Y", locale = "pt_BR")) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
}

# Rodar o app
shinyApp(ui = ui, server = server)

# Carregar as bibliotecas necessárias
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(readxl)
library(shinydashboard)
library(googlesheets4)
library(ggrepel)
library(scales)# Carregar o pacote lubridate, se necessário
library(lubridate)

# Configurar o locale para português do Brasil
Sys.setlocale("LC_TIME", "pt_BR.UTF-8")

# Função para ler e incrementar o contador de visitas com Google Sheets
get_visit_count <- function(sheet_id = "1ABQ98Le37xrH4i9HzptJvYfDtegomDTitpGtKm0H9Fs") {
  # Authenticate with service account from environment variable
  gs4_auth(path = Sys.getenv("GOOGLE_SHEETS_AUTH"))
  
  # Read current count
  sheet_data <- read_sheet(sheet_id, sheet = 1)
  if (nrow(sheet_data) == 0 || is.na(sheet_data$Count[1])) {
    count <- 1
  } else {
    count <- sheet_data$Count[1] + 1
  }
  
  # Update the sheet (overwrite the count in A2)
  range_write(sheet_id, data = data.frame(Count = count), sheet = 1, range = "A2", col_names = FALSE)
  
  return(count)
}

# Carregar os dados
CT <- read_xlsx("CT.xlsx")
VAR_PROD <- read_xlsx("VAR_PROD.xlsx")

# Definir a interface do usuário
ui <- navbarPage(
  title = div(
    tags$a(
      href = "https://bttomio.shinyapps.io/cidadaniafinanceira/",  # Link para a página inicial (ajuste conforme a URL da sua aplicação)
      "Cidadania Financeira [FURB]",
      style = "text-decoration: none; color: inherit;"  # Estilo para parecer com o texto original
    )
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),  # Link the CSS file
    tags$style(HTML("
      .youtube-container {
        position: relative;
        padding-bottom: 56.25%; /* Proporção 16:9 */
        height: 0;
        overflow: hidden;
        max-width: 100%;
        margin-bottom: 20px;
      }
      .youtube-container iframe {
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
      }
      .media-link {
        font-size: 18px;
        color: #007bff;
        text-decoration: none;
      }
      .media-link:hover {
        text-decoration: underline;
      }
      .intro-text {
        font-size: 16px;
        color: #34495e;
        margin-bottom: 20px;
      }
      .intro-text h3 {
        font-size: 24px;
        font-weight: bold;
        color: #2c3e50;
        margin-bottom: 10px;
      }
      .intro-text ul {
        margin-left: 20px;
        margin-bottom: 20px;
      }
    "))
  ),
  
  # Página inicial (Capa)
  tabPanel("Início",
           fluidPage(
             tags$head(
               tags$style(HTML("
        .inicio-container {
          text-align: center;
          padding: 40px 20px;
        }
        .inicio-logo {
          max-width: 200px;
          margin-top: 20px;
        }
        .inicio-title {
          font-size: 32px;
          font-weight: bold;
          margin-bottom: 10px;
          color: #2c3e50;
        }
        .inicio-text {
          font-size: 18px;
          margin-bottom: 20px;
          color: #34495e;
        }
        .info-box {
          display: inline-block;
          background-color: #f5f5f5;
          border-left: 5px solid #007bff;
          padding: 15px 20px;
          font-size: 16px;
          color: #2c3e50;
          border-radius: 8px;
        }
        .visit-counter {
          display: inline-block;
          background-color: #e9f7ef;
          border-left: 5px solid #28a745;
          padding: 15px 20px;
          font-size: 16px;
          color: #2c3e50;
          border-radius: 8px;
          margin-top: 10px;
        }
      "))
             ),
             
             div(class = "inicio-container",
                 div(class = "inicio-title", "Bem-vindo(a)! Agradecemos muito sua visita!"),
                 br(),
                 br(),
                 div(class = "inicio-text",
                     HTML("Esta plataforma divulga os dados coletados pelo projeto de extensão <strong>Cidadania Financeira</strong> da 
        <a href='https://www.furb.br' target='_blank'>Universidade Regional de Blumenau (FURB)</a>.")
                 ),
                 div(class = "inicio-text",
                     "Escolha uma das opções no menu acima para visualizar os dados da cesta básica e dos produtos analisados."),
                 br(),
                 div(class = "inicio-text",
                     "Entre em contato conosco: ",
                     tags$a(href = "mailto:bttomio@furb.br?subject=Contato&body=Olá! Tudo bem? Por gentileza, escreva sua mensagem aqui...", "bttomio@furb.br"),
                     " | ",
                     tags$a(href = "mailto:financas@furb.br?subject=Contato&body=Olá! Tudo bem? Por gentileza, escreva sua mensagem aqui...", "financas@furb.br")),
                 div(class = "inicio-text",
                     "Estamos sempre à disposição!"),
                 br(),
                 div(class = "info-box", "Última atualização: 25/09/2025"),
                 br(),
                 div(class = "visit-counter", textOutput("visit_count")),
                 br(),
                 img(src = "logo.jpg", class = "inicio-logo")
             )
           )
  ),
  
  # Página de "Cesta Básica - Variação Mensal"
  tabPanel("Cesta Básica",
           fluidPage(
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
  
  # Página de "Produtos da Cesta"
  tabPanel("Produtos da Cesta",
           fluidPage(
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
  
  # Página de "Relatórios"
  tabPanel("Relatórios",
           fluidPage(
             titlePanel("Relatórios da Cesta Básica de Blumenau"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("relatorio_mes_ano", "Selecione o Mês/Ano", 
                             choices = c("Selecione", "Agosto/2025"), 
                             selected = "Selecione")
               ),
               mainPanel(
                 conditionalPanel(
                   condition = "input.relatorio_mes_ano == 'Agosto/2025'",
                   div(class = "intro-text",
                       HTML("
                       <h3>Cesta Básica de Blumenau apresenta queda em agosto de 2025</h3>
                       <p>O Indicador da Cesta Básica de Blumenau, divulgado mensalmente pelo projeto Cidadania Financeira da FURB, registrou um valor de R$ 669,94 (ver Gráfico 1). Constata-se uma redução de 1,57% no custo total dos alimentos essenciais em agosto de 2025, em comparação com o mês anterior (ver Gráfico 2). Essa variação reflete o comportamento dos preços de 13 produtos que compõem a cesta básica, revelando oscilações significativas tanto de alta quanto de baixa.</p>
                       <p>Conforme Gráfico 3, entre os itens que apresentaram aumento de preço, destacam-se:</p>
                       <ul>
                         <li>Batata, com alta expressiva de 17,99%, sendo o produto com maior aumento no mês.</li>
                         <li>Banana, que subiu 6,97%, seguida pelo pão francês (1,55%), farinha de trigo (1,19%) e açúcar refinado (0,22%).</li>
                       </ul>
                       <p>Por outro lado, diversos produtos registraram queda nos preços, contribuindo para o recuo geral do indicador:</p>
                       <ul>
                         <li>O café em pó teve a maior redução, com queda de 7,09%.</li>
                         <li>A carne caiu 4,99%, seguida pela manteiga (4,17%), leite (3,26%), feijão preto (3,14%), arroz tipo 1 (2,01%), óleo de soja (1,12%) e tomate (1,07%).</li>
                       </ul>
                       <p>Essas variações refletem fatores sazonais, logísticos e de mercado que influenciam diretamente o custo de vida da população local.</p>
                       <p>Para mais informações sobre o indicador e outros dados econômicos regionais, acesse <a href='https://furb.br/cidadaniafinanceira' target='_blank'>furb.br/cidadaniafinanceira</a>.</p>
                       <p>Contato: <a href='mailto:bttomio@furb.br?subject=Contato&body=Olá! Tudo bem? Por gentileza, escreva sua mensagem aqui...'>Prof. Dr. Bruno Thiago Tomio – bttomio@furb.br</a></p>
                     ")
                   ),
                   box(
                     title = "Gráfico 1",
                     status = "success",
                     solidHeader = TRUE,
                     width = 12,
                     plotOutput("grafico_valor_cesta_blumenau")
                   ),
                   box(
                     title = "Gráfico 2",
                     status = "success",
                     solidHeader = TRUE,
                     width = 12,
                     plotOutput("grafico_variacao_cesta_blumenau")
                   ),
                   box(
                     title = "Gráfico 3",
                     status = "success",
                     solidHeader = TRUE,
                     width = 12,
                     plotOutput("grafico_produtos_blumenau")
                   )
                 )
               )
             )
           )
  ),
  
  # Página de "Metodologia da Cesta"
  tabPanel(
    "Metodologia da Cesta",
    fluidPage(
      h3("Metodologia da Cesta Básica"),
      p("Ela é baseada na Cesta Básica de Alimentos do DIEESE (Departamento Intersindical de Estatística e Estudos Socioeconômicos), pesquisada mensalmente em 18 capitais brasileiras. É uma cesta de alimentos composta por 13 produtos alimentícios em quantidades suficientes para garantir, durante um mês, o sustento e bem-estar de uma pessoa adulta."),
      h4("Composição da Cesta Básica - Região 3, que inclui Santa Catarina"),
      tableOutput("regiao3_table")
    )
  ),
  
  # Página de "Mídia"
  tabPanel(
    "Mídia",
    fluidPage(
      div(class = "inicio-container",
          div(class = "inicio-title", "Mídia e Divulgação"),
          div(class = "inicio-text",
              "Confira divulgações do projeto Cidadania Financeira:"),
          br(),
          div(class = "inicio-text",
              tags$a(
                href = "https://youtube.com/playlist?list=PLAL8vVk6Z3KDgc7DBFHEa8ddIM5dTgBh-&si=0QSsK_77HA874AxZ",
                target = "_blank",
                class = "media-link",
                "Colunas no Boletim de Economia da FURB FM (107,1)"
              )
          ),
          div(class = "inicio-text",
              tags$a(
                href = "https://globoplay.globo.com/v/13715431/",
                target = "_blank",
                class = "media-link",
                "Participação ao vivo no Jornal do Almoço (Blumenau) - NSC TV [GloboPlay]"
              )
          ),
          br(),
          img(src = "logo.jpg", class = "inicio-logo")
      )
    )
  ),
  
  # Página de "Equipe"
  tabPanel(
    "Equipe",
    fluidPage(
      div(class = "inicio-container",
          div(class = "inicio-title", "Equipe do Projeto"),
          div(class = "inicio-text",
              HTML("<strong>Prof. Dr. Bruno Thiago Tomio</strong><br>Coordenador/Criador do projeto<br>
                   <a href='https://bttomio.github.io' target='_blank'>Página pessoal</a>")),
          div(class = "inicio-text",
              HTML("<strong>Stefanie Romualdo Schulze</strong><br>Bolsista do projeto")),
          br(),
          img(src = "logo.jpg", class = "inicio-logo")
      )
    )
  )
)

# Definir a lógica do servidor
server <- function(input, output, session) {
  
  # Inicializar o contador de visitas
  visit_count <- reactiveVal(get_visit_count())
  
  # Renderizar o contador de visitas
  output$visit_count <- renderText({
    paste("Total de Visitas:", visit_count())
  })
  
  # Tabela com os itens da cesta
  output$regiao3_table <- renderTable({
    data.frame(
      Produto = c("Açúcar", "Arroz", "Batata", "Café em pó", "Carne bovina", "Farinha", "Feijão", "Fruta (banana)", "Leite", "Manteiga", "Óleo de soja", "Pão francês", "Tomate"),
      Quantidade = c("3,0 kg", "3,0 kg", "6,0 kg", "600 g", "6,6 kg", "1,5 kg", "4,5 kg", "90 unidades", "7,5 l", "750 g", "900 g", "6,0 kg", "9,0 kg")
    )
  })
  
  # Converter a coluna 'Período' para o formato Date
  CT$Período <- as.Date(CT$Período)
  VAR_PROD$Período <- as.Date(VAR_PROD$Período)
  
  # Filtrar o último valor da cesta básica de Blumenau
  ultimo_valor_cesta <- reactive({
    dados <- CT %>%
      filter(Cidade == "Blumenau") %>%
      arrange(desc(Período)) %>%
      slice(1)
    
    # Verificar se há dados e se as colunas necessárias existem
    if (nrow(dados) == 0 || all(is.na(dados$Cesta)) || all(is.na(dados$`Variação (%)`))) {
      return(NULL)
    } else {
      return(dados)
    }
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
      dados_filtrados(),
      options = list(
        scrollY = "300px",
        scrollCollapse = TRUE,
        paging = FALSE,
        order = list(list(0, 'asc')),
        columnDefs = list(
          list(targets = which(names(dados_filtrados()) == "Período"), visible = FALSE)
        )
      )
    ) %>%
      formatStyle(
        'Variação (%)',
        color = styleInterval(0, c('red', 'black'))
      ) %>%
      formatRound(
        c("Cesta", "Variação (%)"),
        digits = 2
      )
  })
  
  # Renderizar a tabela de Dados dos Produtos
  output$tabela_produtos <- renderDT({
    datatable(
      dados_produtos_filtrados(),
      options = list(
        scrollY = "300px",
        scrollCollapse = TRUE,
        paging = FALSE,
        order = list(list(0, 'asc')),
        columnDefs = list(
          list(targets = which(names(dados_produtos_filtrados()) == "Período"), visible = FALSE)
        )
      )
    ) %>%
      formatStyle(
        'Variação (%)',
        color = styleInterval(0, c('red', 'black'))
      ) %>%
      formatRound(
        c("Média (produto)", "Variação (%)"),
        digits = 2
      )
  })
  
  # Renderizar o gráfico de Variação Mensal da Cesta Básica
  output$grafico_cesta <- renderPlot({
    dados <- dados_filtrados()
    
    req(nrow(dados) > 0)
    
    dados$Período <- as.Date(dados$Período)
    dados$label_color <- ifelse(dados$`Variação (%)` >= 0, "black", "red")
    
    ggplot(dados, aes(x = Período, y = `Variação (%)`, group = Cidade, color = Cidade)) +
      geom_line(colour = "blue") +
      geom_point(colour = "blue") +
      geom_text_repel(aes(label = paste0(round(`Variação (%)`, 2), "%"), color = label_color),
                      size = 12/3,  # Ajustado para tamanho 12
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
           x = NULL, 
           y = "Variação (%)") +
      scale_y_continuous(labels = label_percent(scale = 1)) +
      scale_x_date(labels = scales::date_format("%b/%Y", locale = "pt_BR"), breaks = scales::date_breaks("1 month")) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 12, face = "bold"),
        legend.position = "none",
        panel.border = element_rect(color = "black", size = 1)
      )
  })
  
  # Renderizar o gráfico de Variação Mensal dos Produtos
  output$grafico_produtos <- renderPlot({
    dados_produtos <- dados_produtos_filtrados()
    
    req(nrow(dados_produtos) > 0)
    
    dados_produtos$Período <- as.Date(dados_produtos$Período)
    dados_produtos$label_color <- ifelse(dados_produtos$`Variação (%)` >= 0, "black", "red")
    
    ggplot(dados_produtos, aes(x = Período, y = `Variação (%)`, group = Produto, color = Produto)) +
      geom_line(colour = "blue") +
      geom_point(colour = "blue") +
      geom_text_repel(aes(label = paste0(round(`Variação (%)`, 2), "%"), color = label_color),
                      size = 12/3,  # Ajustado para tamanho 12
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
      scale_x_date(labels = scales::date_format("%b/%Y", locale = "pt_BR"), breaks = scales::date_breaks("1 month")) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 12, face = "bold"),
        legend.position = "none",
        panel.border = element_rect(color = "black", size = 1)
      )
  })
  
  # Renderizar o gráfico de Valor da Cesta Básica (Últimos 13 Meses)
  output$grafico_valor_cesta_blumenau <- renderPlot({
    dados <- CT %>%
      filter(Cidade == "Blumenau") %>%
      arrange(desc(Período)) %>%
      slice_head(n = 13)
    
    req(nrow(dados) > 0)
    
    ggplot(dados, aes(x = Período, y = Cesta)) +
      geom_bar(stat = "identity", fill = "#1f77b4") +
      geom_text(aes(label = scales::number_format(big.mark = ".", decimal.mark = ",", accuracy = 0.01)(Cesta)),
                vjust = -0.5, size = 12/3) +
      scale_y_continuous(
        labels = scales::number_format(prefix = "R$ ", big.mark = ".", decimal.mark = ","),
        expand = expansion(mult = c(0, 0.1)) # Add 10% extra space at the top
      ) +
      scale_x_date(
        labels = scales::date_format("%b/%Y", locale = "pt_BR"),
        breaks = scales::date_breaks("1 month"),
        limits = c(min(dados$Período) - 15, max(dados$Período) + 15), # Add padding to date range
        expand = expansion(add = c(0.5, 0.5)) # Add padding to avoid clipping
      ) +
      labs(
        title = "Valor da Cesta Básica em Blumenau (Últimos 13 Meses)",
        x = NULL,
        y = NULL,
        caption = paste("Fonte: Projeto de Extensão Cidadania Financeira da Universidade de Blumenau (FURB).",
                        "\n Mais detalhes em furb.br/cidadaniafinanceira.")
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 12, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 12),
        panel.border = element_rect(color = "black", size = 1)
      )
  })
  
  # Renderizar o gráfico de Variação Percentual da Cesta Básica (Últimos 13 Meses)
  output$grafico_variacao_cesta_blumenau <- renderPlot({
    dados <- CT %>%
      filter(Cidade == "Blumenau") %>%
      arrange(desc(Período)) %>%
      slice_head(n = 13)
    
    req(nrow(dados) > 0)
    
    dados$label_color <- ifelse(dados$`Variação (%)` >= 0, "#0000CC", "#FF0000")
    
    ggplot(dados, aes(x = Período, y = `Variação (%)`, group = 1)) +
      geom_line(color = "black", size = 1) +
      geom_point(color = "black") +
      geom_text_repel(aes(label = paste0(round(`Variação (%)`, 2), "%"), color = label_color),
                      size = 12/3,
                      nudge_y = ifelse(dados$`Variação (%)` >= 0, 0.2, -0.2),
                      max.overlaps = 20,
                      box.padding = 0.35,
                      point.padding = 0.5,
                      segment.size = 0.2,
                      direction = "both",
                      force = 1,
                      show.legend = FALSE) +
      geom_hline(yintercept = 0, color = "darkgrey", linetype = "solid", size = 1) +
      scale_color_identity() +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      scale_x_date(labels = scales::date_format("%b/%Y", locale = "pt_BR"), 
                   breaks = scales::date_breaks("1 month")) +
      labs(title = "Variação Percentual da Cesta Básica em Blumenau (Últimos 13 Meses)",
           x = NULL,
           y = NULL,
           caption = paste("Fonte: Projeto de Extensão Cidadania Financeira da Universidade de Blumenau (FURB).",
                           "\n Mais detalhes em furb.br/cidadaniafinanceira.")) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 12, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 12),
        panel.border = element_rect(color = "black", size = 1)
      )
  })
  
  # Renderizar o gráfico de Variação Percentual dos Produtos (Último Mês)
  output$grafico_produtos_blumenau <- renderPlot({
    dados <- VAR_PROD %>%
      filter(Cidade == "Blumenau") %>%
      arrange(desc(Período)) %>%
      slice_head(n = 13) %>%
      arrange(`Variação (%)`)
    
    req(nrow(dados) > 0)
    
    dados$Produto <- factor(dados$Produto, levels = dados$Produto)
    
    limite <- max(abs(dados$`Variação (%)`), na.rm = TRUE) * 1.1
    
    ggplot(dados, aes(x = Produto, y = `Variação (%)`, fill = `Variação (%)`)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(
        labels = function(x) paste0(x, "%"), # Add % to y-axis labels
        expand = expansion(mult = c(0.1, 0.1))) + # Add 10% extra space at the top) 
      scale_fill_gradient2(low = "#FF0000", mid = "#FFFFFF", high = "#0000CC", 
                           limits = c(-limite, limite), midpoint = 0) +
      geom_text(aes(label = paste0(round(`Variação (%)`, 2), "%")), 
                vjust = ifelse(dados$`Variação (%)` >= 0, -0.5, 1.5), size = 12/3) +
      labs(title = paste("Variação de Preços da Cesta Básica de Blumenau -", 
                         format(max(dados$Período), "%b/%Y", language = "pt-BR")),
           x = NULL,
           y = NULL,
           caption = paste("Fonte: Projeto de Extensão Cidadania Financeira da Universidade de Blumenau (FURB).",
                           "\n Mais detalhes em furb.br/cidadaniafinanceira.")) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 12, face = "bold"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        panel.border = element_rect(color = "black", size = 1),
        plot.caption = element_text(hjust = 0, size = 12)
      )
  })
}

# Executar o aplicativo Shiny
shinyApp(ui = ui, server = server)
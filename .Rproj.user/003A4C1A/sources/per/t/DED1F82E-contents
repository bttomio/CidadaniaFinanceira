# Para atualizar a página use: 
## Recuperar o número de visitas na página e inserir aqui, rodando este código:
#library(DBI)
#library(RSQLite)
#con <- dbConnect(SQLite(), dbname = "visit_counter.db")
#dbExecute(con, "UPDATE visits SET count = ? WHERE id = 1", params = list(NEW_COUNT)) # Substitua NEW_COUNT pelo valor atual
#dbDisconnect(con)

# Atualiza o app para a página:
## rsconnect::deployApp()

# Carregar as bibliotecas necessárias
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(readxl)
library(shinydashboard)
library(DBI)
library(RSQLite)

# Função para ler e incrementar o contador de visitas com SQLite
get_visit_count <- function() {
  con <- dbConnect(SQLite(), dbname = "visit_counter.db")
  if (dbExistsTable(con, "visits")) {
    count <- dbGetQuery(con, "SELECT count FROM visits WHERE id = 1")$count
    count <- count + 1
    dbExecute(con, "UPDATE visits SET count = ? WHERE id = 1", params = list(count))
  } else {
    count <- 1
    dbExecute(con, "CREATE TABLE visits (id INTEGER PRIMARY KEY, count INTEGER)")
    dbExecute(con, "INSERT INTO visits (id, count) VALUES (1, ?)", params = list(count))
  }
  dbDisconnect(con)
  return(count)
}

CT <- read_xlsx("CT.xlsx")
VAR_PROD <- read_xlsx("VAR_PROD.xlsx")

# Definir a interface do usuário
ui <- navbarPage(
  title = "Cidadania Financeira [FURB]",
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
                     " ou ",
                     tags$a(href = "mailto:financas@furb.br?subject=Contato&body=Olá! Tudo bem? Por gentileza, escreva sua mensagem aqui...", "financas@furb.br"),
                     ". Estamos sempre à disposição!"
                 ),
                 br(),
                 div(class = "info-box", "Última atualização: 06/08/2025"),
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
  
  # Página de "Dados dos Produtos"
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
  
  # Nova Página de "Mídia" (antes de Equipe)
  tabPanel(
    "Mídia",
    fluidPage(
      div(class = "inicio-container",
          div(class = "inicio-title", "Mídia e Divulgação"),
          div(class = "inicio-text",
              "Confira divulgações do projeto Cidadania Financeira:"),
          br(),
          div(class = "youtube-container",
              tags$iframe(
                width = "560",
                height = "315",
                src = "https://www.youtube.com/embed/zVeoKZVWMW0?si=xGwcYA4Mr9GUZIxh",
                title = "YouTube video player",
                frameborder = "0",
                allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share",
                referrerpolicy = "strict-origin-when-cross-origin",
                allowfullscreen = TRUE
              )
          ),
          div(class = "inicio-text",
              tags$a(
                href = "https://globoplay.globo.com/v/13715431/",
                target = "_blank",
                class = "media-link",
                "Participação no Jornal ao Vivo - GloboPlay [NSC TV]"
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
      scale_x_date(labels = scales::date_format("%b/%Y", locale = "pt_BR"), breaks = scales::date_breaks("1 month")) +
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
      scale_x_date(labels = scales::date_format("%b/%Y", locale = "pt_BR"), breaks = scales::date_breaks("1 month")) +
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
}

# Executar o aplicativo Shiny
shinyApp(ui = ui, server = server)
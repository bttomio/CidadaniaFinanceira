# Carregar as bibliotecas necessárias
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(readxl)
library(shinydashboard)
library(ggrepel)
library(scales)# Carregar o pacote lubridate, se necessário
library(lubridate)
library(bslib) # Tema visual moderno (cores, fontes, componentes Bootstrap 5)

# ---------------------------------------------------------------------------
# Paleta e tipografia da identidade visual "Cidadania Financeira"
# ---------------------------------------------------------------------------
COR_PRIMARIA   <- "#1B3A4B"  # azul-petróleo profundo (navbar, títulos)
COR_SECUNDARIA <- "#2D6E7E"  # teal (links, linhas de gráfico, hover)
COR_DESTAQUE   <- "#E8A23D"  # âmbar/dourado (CTAs, destaques)
COR_POSITIVA   <- "#3B7A57"  # verde (queda de preço / variação favorável)
COR_NEGATIVA   <- "#C1502E"  # terracota (alta de preço / variação desfavorável)
COR_FUNDO      <- "#F7F5F0"  # fundo neutro levemente quente

# Tema Bootstrap 5 customizado
meu_tema <- bs_theme(
  version = 5,
  bg = COR_FUNDO,
  fg = "#1A1A1A",
  primary = COR_PRIMARIA,
  secondary = COR_SECUNDARIA,
  success = COR_POSITIVA,
  danger = COR_NEGATIVA,
  warning = COR_DESTAQUE,
  base_font = font_google("Inter"),
  heading_font = font_google("Source Serif 4"),
  code_font = font_google("IBM Plex Mono"),
  "navbar-bg" = COR_PRIMARIA,
  "border-radius" = "0.6rem"
)

# Configurar o locale para português do Brasil
Sys.setlocale("LC_TIME", "pt_BR.UTF-8")

# Carregar os dados
CT <- read_xlsx("CT.xlsx")
VAR_PROD <- read_xlsx("VAR_PROD.xlsx")
PRECOS  <- read_xlsx("PRECOS.xlsx")

# Tabela de referência: embalagem, unidade e quantidade padrão de cada produto
TABELA_REFERENCIA <- tibble::tribble(
  ~Produto,           ~Embalagem, ~Unidade, ~Quantidade,
  "Arroz tipo 1",     "Pacote",   "Kg",     1.000,
  "Açúcar Refinado",  "Pacote",   "Kg",     1.000,
  "Café em pó",       "Pacote",   "Kg",     0.500,
  "Farinha de Trigo", "Pacote",   "Kg",     1.000,
  "Feijão Preto",     "Pacote",   "Kg",     1.000,
  "Manteiga",         "Pote",     "Kg",     0.500,
  "Óleo de Soja",     "Garrafa",  "L",      0.900,
  "Carne",            "Granel",   "Kg",     1.000,
  "Pão Francês",      "Granel",   "Kg",     1.000,
  "Batata",           "Granel",   "Kg",     1.000,
  "Tomate",           "Granel",   "Kg",     1.000,
  "Leite",            "Caixa",    "L",      1.000,
  "Banana",           "Granel",   "Kg",     1.000
)

# Definir a interface do usuário
ui <- navbarPage(
  title = div(
    tags$a(
      href = "https://bttomio.shinyapps.io/cidadaniafinanceira/",  # Link para a página inicial (ajuste conforme a URL da sua aplicação)
      "Cidadania Financeira [FURB]",
      style = "text-decoration: none; color: inherit; font-family: 'Source Serif 4', serif; font-weight: 600; letter-spacing: 0.2px;"
    )
  ),
  theme = meu_tema,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),  # Link the CSS file
    tags$style(HTML(paste0("
      /* ---------- Tipografia ---------- */
      h1, h2, h3, h4, h5, .inicio-title {
        font-family: 'Source Serif 4', serif;
      }
      body, p, label, .selectize-input, table {
        font-family: 'Inter', sans-serif;
      }
      .valor-mono {
        font-family: 'IBM Plex Mono', monospace;
        font-weight: 600;
      }

      /* ---------- Navbar ---------- */
      .navbar {
        background-color: ", COR_PRIMARIA, " !important;
        box-shadow: 0 2px 8px rgba(0,0,0,0.12);
      }
      .navbar-default .navbar-nav > li > a,
      .navbar-default .navbar-brand {
        color: #F2F2F0 !important;
        font-weight: 500;
      }
      .navbar-default .navbar-nav > li > a:hover {
        color: ", COR_DESTAQUE, " !important;
      }
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:hover {
        color: #FFFFFF !important;
        background-color: transparent !important;
        border-bottom: 3px solid ", COR_DESTAQUE, ";
      }
      
      /* ---------- Alinhamento do título no navbar ---------- */
      .navbar .container-fluid {
        display: flex;
        align-items: center;
      }
      .navbar-brand {
        display: flex;
        align-items: center;
        padding-top: 0 !important;
        padding-bottom: 0 !important;
      }
      .navbar-nav {
        align-items: center;
      }


      /* ---------- Cards (substituindo o visual padrão do shinydashboard) ---------- */
      .box {
        border-radius: 10px !important;
        border-top: none !important;
        box-shadow: 0 2px 10px rgba(27,58,75,0.08) !important;
        overflow: hidden;
      }
      .box-header {
        background-color: #FFFFFF !important;
      }
      .box-title {
        font-family: 'Source Serif 4', serif;
        font-weight: 600;
        color: ", COR_PRIMARIA, " !important;
      }
      .box.box-solid.box-primary > .box-header {
        background-color: ", COR_PRIMARIA, " !important;
        color: #FFFFFF !important;
      }
      .box.box-solid.box-primary > .box-header .box-title { color: #FFFFFF !important; }
      .box.box-solid.box-success > .box-header {
        background-color: ", COR_SECUNDARIA, " !important;
        color: #FFFFFF !important;
      }
      .box.box-solid.box-success > .box-header .box-title { color: #FFFFFF !important; }

      /* ---------- Sidebar de filtros ---------- */
      .well, .sidebar-panel, .form-group {
        background-color: #FFFFFF;
      }
      .well {
        border-radius: 10px;
        border: 1px solid #E5E1D8;
        box-shadow: 0 1px 4px rgba(0,0,0,0.04);
      }

      /* ---------- Abas internas (tabsetPanel) ---------- */
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
        color: ", COR_PRIMARIA, " !important;
        font-weight: 600;
        border-bottom: 3px solid ", COR_DESTAQUE, ";
      }

      /* ---------- Cartão de destaque (etiqueta de preço) ---------- */
      .destaque-cesta {
        display: inline-block;
        background-color: #FFFFFF;
        border: 1.5px solid ", COR_PRIMARIA, ";
        border-radius: 12px;
        padding: 18px 28px;
        box-shadow: 0 4px 14px rgba(27,58,75,0.10);
        text-align: left;
        margin-bottom: 10px;
      }
      .destaque-cesta .rotulo {
        font-size: 13px;
        text-transform: uppercase;
        letter-spacing: 1px;
        color: #6b7280;
        margin-bottom: 4px;
      }
      .destaque-cesta .valor {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 30px;
        font-weight: 700;
        color: ", COR_PRIMARIA, ";
      }
      .destaque-cesta .variacao-positiva { color: ", COR_NEGATIVA, "; font-weight: 600; }
      .destaque-cesta .variacao-negativa { color: ", COR_POSITIVA, "; font-weight: 600; }

      /* ---------- Elementos diversos ---------- */
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
        color: ", COR_SECUNDARIA, ";
        text-decoration: none;
        font-weight: 500;
      }
      .media-link:hover {
        text-decoration: underline;
        color: ", COR_PRIMARIA, ";
      }
      .intro-text {
        font-size: 16px;
        color: #34495e;
        margin-bottom: 20px;
      }
      .intro-text h3 {
        font-size: 24px;
        font-weight: bold;
        color: ", COR_PRIMARIA, ";
        margin-bottom: 10px;
      }
      .intro-text ul {
        margin-left: 20px;
        margin-bottom: 20px;
      }
      /* ---------- Fundo branco (Início e Equipe) ---------- */
      .fundo-branco {
        background-color: #FFFFFF !important;
        min-height: calc(100vh - 50px);
        padding: 20px 0;
      }
    ")))
  ),
  
  # Página inicial (Capa)
  tabPanel("Início",
           div(class = "fundo-branco",
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
                 uiOutput("destaque_cesta_ui"),
                 br(),
                 div(class = "info-box", paste("Última atualização:", format(Sys.Date(), "%d/%m/%Y"))),
                 br(),
                 img(src = "logo.jpg", class = "inicio-logo")
             )
           )
           )
  ),
  
  # Página "Cesta Básica" - agora com sub-abas: Cesta Básica, Produtos da Cesta, Preços da Cesta e Metodologia da Cesta
  tabPanel("Cesta Básica",
           fluidPage(
             tabsetPanel(
               id = "cesta_subtabs",
               
               # Sub-aba: Cesta Básica (variação mensal)
               tabPanel("Cesta Básica",
                        fluidPage(
                          tags$div(style = "font-size: 20px; font-weight: bold; color: #1a1a1a; margin: 20px 0;", 
                                   "Para visualizar um gráfico, selecione: Cidade."),
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
                        )
               ),
               
               # Sub-aba: Produtos da Cesta
               tabPanel("Produtos da Cesta",
                        fluidPage(
                          tags$div(style = "font-size: 20px; font-weight: bold; color: #1a1a1a; margin: 20px 0;", 
                                   "Para visualizar um gráfico, selecione: Produto e Cidade."),
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
                        )
               ),
               
               # Sub-aba: Preços da Cesta
               tabPanel("Preços da Cesta",
                        fluidPage(
                          tags$div(style = "font-size: 20px; font-weight: bold; color: #1a1a1a; margin: 20px 0;",
                                   "Para visualizar um gráfico, selecione: Produto e Cidade."),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("preco_produto", "Selecione o Produto",
                                          choices = c("Todos", unique(PRECOS$Produto)), selected = "Todos"),
                              selectInput("preco_cidade", "Selecione a Cidade",
                                          choices = c("Todos", unique(PRECOS$Cidade)), selected = "Todos"),
                              selectInput("preco_mes", "Selecione o Mês",
                                          choices = c("Todos", "Janeiro", "Fevereiro", "Março", "Abril", "Maio",
                                                      "Junho", "Julho", "Agosto", "Setembro", "Outubro",
                                                      "Novembro", "Dezembro"), selected = "Todos"),
                              selectInput("preco_ano", "Selecione o Ano",
                                          choices = c("Todos", unique(PRECOS$Ano)), selected = "Todos")
                            ),
                            mainPanel(
                              box(
                                title = "Tabela de Preços dos Produtos da Cesta",
                                status = "primary",
                                solidHeader = TRUE,
                                width = 12,
                                DTOutput("tabela_precos")
                              ),
                              br(),
                              conditionalPanel(
                                condition = "input.preco_produto != 'Todos' && input.preco_cidade != 'Todos'",
                                box(
                                  title = "Gráfico de Preço Médio e Variação Mensal",
                                  status = "success",
                                  solidHeader = TRUE,
                                  width = 12,
                                  plotOutput("grafico_precos")
                                )
                              )
                            )
                          )
                        )
               ),
               
               # Sub-aba: Metodologia da Cesta
               tabPanel("Metodologia da Cesta",
                        fluidPage(
                          br(),
                          p("Ela é baseada na Cesta Básica de Alimentos do DIEESE (Departamento Intersindical de Estatística e Estudos Socioeconômicos), pesquisada mensalmente em 18 capitais brasileiras. É uma cesta de alimentos composta por 13 produtos alimentícios em quantidades suficientes para garantir, durante um mês, o sustento e bem-estar de uma pessoa adulta."),
                          h4("Composição da Cesta Básica - Região 3, que inclui Santa Catarina"),
                          tableOutput("regiao3_table")
                        )
               )
             )
           )
  ),
  
  # Página de "Mídia"
  tabPanel(
    "Mídia",
    div(class = "fundo-branco",
        fluidPage(
          div(class = "inicio-container",
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
    )
  ),  
  # Página de "Equipe"
  tabPanel(
    "Equipe",
    div(class = "fundo-branco",
    fluidPage(
      div(class = "inicio-container",
          div(class = "inicio-text",
              HTML("<strong>Prof. Dr. Bruno Thiago Tomio</strong><br>Coordenador/Criador do projeto<br>")),
          
          div(class = "inicio-text",
              "Entre em contato conosco: ",
              tags$a(href = "mailto:bttomio@furb.br?subject=Contato&body=Olá! Tudo bem? Por gentileza, escreva sua mensagem aqui...", "bttomio@furb.br")),
          div(class = "inicio-text",
              "Estamos sempre à disposição!"),
          br(),
          div(class = "inicio-text",
              tags$a(href = "https://www.furb.br", target = "_blank",
                     img(src = "logo.jpg", class = "inicio-logo"))
          )
      )
    )
    )
  )
)

# Definir a lógica do servidor
server <- function(input, output, session) {
  
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
  PRECOS$Período <- as.Date(PRECOS$Período)
  
  # Filtrar o último valor da cesta básica de Blumenau
  ultimo_valor_cesta <- reactive({
    if (nrow(CT) == 0) return(NULL)
    
    ultimo_periodo <- max(CT$Período, na.rm = TRUE)
    
    dados <- CT %>%
      filter(Período == ultimo_periodo,
             !is.na(Cesta),
             !is.na(`Variação (%)`))
    
    if (nrow(dados) == 0) NULL else dados
  })
  
  # Cartão de destaque na página inicial com o valor mais recente da cesta básica (Blumenau)
  output$destaque_cesta_ui <- renderUI({
    dados <- ultimo_valor_cesta()
    if (is.null(dados)) return(NULL)
    
    cards <- lapply(seq_len(nrow(dados)), function(i) {
      linha <- dados[i, ]
      variacao <- linha$`Variação (%)`
      classe_variacao <- if (variacao >= 0) "variacao-positiva" else "variacao-negativa"
      sinal <- if (variacao >= 0) "+" else ""
      
      div(class = "destaque-cesta",
          div(class = "rotulo", paste("Cesta básica em", linha$Cidade, "—", format(linha$Período, "%B/%Y"))),
          span(class = "valor", paste0("R$ ", scales::number(linha$Cesta, decimal.mark = ",", big.mark = ".", accuracy = 0.01))),
          span(class = paste("valor-mono", classe_variacao), style = "margin-left: 12px; font-size: 18px;",
               paste0(sinal, scales::number(variacao, decimal.mark = ",", accuracy = 0.01), "% no mês"))
      )
    })
    
    div(style = "display: flex; flex-wrap: wrap; gap: 16px; justify-content: center;",
        cards)
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
  
  # Filtrar dados de Preços com base nas seleções do usuário
  dados_precos_filtrados <- reactive({
    PRECOS %>%
      filter(
        (input$preco_produto == "Todos" | Produto == input$preco_produto),
        (input$preco_cidade  == "Todos" | Cidade  == input$preco_cidade),
        (input$preco_mes     == "Todos" | Mês     == input$preco_mes),
        (input$preco_ano     == "Todos" | Ano     == input$preco_ano)
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
        color = styleInterval(0, c(COR_POSITIVA, COR_NEGATIVA))
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
        color = styleInterval(0, c(COR_POSITIVA, COR_NEGATIVA))
      ) %>%
      formatRound(
        c("Média (produto)", "Variação (%)"),
        digits = 2
      )
  })
  
  # Renderizar a tabela de Preços
  output$tabela_precos <- renderDT({
    df <- dados_precos_filtrados() %>%
      left_join(TABELA_REFERENCIA, by = "Produto") %>%
      select(Cidade, Ano, Mês, Produto, Embalagem, Unidade, Quantidade,
             `Média (produto)`, `Variação (%)`)
    datatable(
      df,
      options = list(
        scrollY = "300px",
        scrollCollapse = TRUE,
        paging = FALSE,
        order = list(list(0, 'asc'))
      )
    ) %>%
      formatStyle(
        'Variação (%)',
        color = styleInterval(0, c(COR_POSITIVA, COR_NEGATIVA))
      ) %>%
      formatRound(
        c("Média (produto)", "Variação (%)"),
        digits = 2
      )
  })
  
  # Renderizar o gráfico de Preço Médio e Variação Mensal
  output$grafico_precos <- renderPlot({
    dados_precos <- PRECOS %>%
      filter(
        Produto == input$preco_produto,
        Cidade  == input$preco_cidade
      ) %>%
      arrange(Período) %>%
      mutate(
        label_color = ifelse(`Variação (%)` >= 0, COR_NEGATIVA, COR_POSITIVA),
        xend = dplyr::lead(Período),
        yend = dplyr::lead(`Média (produto)`)
      )
    
    req(nrow(dados_precos) > 0)
    
    ggplot(dados_precos, aes(x = Período, y = `Média (produto)`, group = Produto)) +
      geom_segment(aes(xend = xend, yend = yend, color = lead(label_color)), linewidth = 0.9, na.rm = TRUE) +
      geom_point(aes(color = label_color), size = 2) +
      geom_text_repel(aes(label = paste0("R$ ", round(`Média (produto)`, 2))),
                      size = 12/3,
                      nudge_y = ifelse(dados_precos$`Variação (%)` >= 0, 0.2, -0.2),
                      max.overlaps = 20,
                      box.padding = 0.35,
                      point.padding = 0.5,
                      segment.size = 0.2,
                      direction = "both",
                      force = 1,
                      show.legend = FALSE,
                      color = dados_precos$label_color) +
      labs(title = paste("Preço Médio —", input$preco_produto, "em", input$preco_cidade),
           x = "Período", y = "Preço Médio (R$)") +
      scale_x_date(labels = scales::date_format("%b/%Y", locale = "pt_BR"), breaks = scales::date_breaks("1 month")) +
      scale_color_identity() +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 12, face = "bold"),
        legend.position = "none",
        panel.border = element_rect(color = COR_PRIMARIA, linewidth = 0.6)
      )
  })
  
  # Renderizar o gráfico de Variação Mensal da Cesta Básica
  output$grafico_cesta <- renderPlot({
    dados <- dados_filtrados()
    
    req(nrow(dados) > 0)
    
    dados <- dados %>%
      mutate(Período = as.Date(Período)) %>%
      group_by(Cidade) %>%
      arrange(Período) %>%
      mutate(
        label_color = ifelse(`Variação (%)` >= 0, COR_NEGATIVA, COR_POSITIVA),
        xend = dplyr::lead(Período),
        yend = dplyr::lead(`Variação (%)`)
      ) %>%
      ungroup()
    
    ggplot(dados, aes(x = Período, y = `Variação (%)`, group = Cidade)) +
      geom_segment(aes(xend = xend, yend = yend, color = lead(label_color)), linewidth = 0.9, na.rm = TRUE) +
      geom_point(aes(color = label_color), size = 2) +
      geom_text_repel(aes(label = paste0(round(`Variação (%)`, 2), "%")),
                      size = 12/3,
                      nudge_y = ifelse(dados$`Variação (%)` >= 0, 0.2, -0.2),
                      max.overlaps = 20,
                      box.padding = 0.35,
                      point.padding = 0.5,
                      segment.size = 0.2,
                      direction = "both",
                      force = 1,
                      show.legend = FALSE,
                      color = dados$label_color) +
      geom_hline(yintercept = 0, color = "#9CA3AF", linetype = "dashed", linewidth = 0.7) +
      labs(title = paste("Variação Percentual da Cesta Básica em", input$cidade), 
           x = NULL, 
           y = "Variação (%)") +
      scale_y_continuous(labels = label_percent(scale = 1)) +
      scale_x_date(labels = scales::date_format("%b/%Y", locale = "pt_BR"), breaks = scales::date_breaks("1 month")) +
      scale_color_identity() +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 12, face = "bold"),
        legend.position = "none",
        panel.border = element_rect(color = COR_PRIMARIA, linewidth = 0.6)
      )
  })
  
  # Renderizar o gráfico de Variação Mensal dos Produtos
  output$grafico_produtos <- renderPlot({
    dados_produtos <- dados_produtos_filtrados()
    
    req(nrow(dados_produtos) > 0)
    
    dados_produtos <- dados_produtos %>%
      mutate(Período = as.Date(Período)) %>%
      group_by(Produto) %>%
      arrange(Período) %>%
      mutate(
        label_color = ifelse(`Variação (%)` >= 0, COR_NEGATIVA, COR_POSITIVA),
        xend = dplyr::lead(Período),
        yend = dplyr::lead(`Variação (%)`)
      ) %>%
      ungroup()
    
    ggplot(dados_produtos, aes(x = Período, y = `Variação (%)`, group = Produto)) +
      geom_segment(aes(xend = xend, yend = yend, color = lead(label_color)), linewidth = 0.9, na.rm = TRUE) +
      geom_point(aes(color = label_color), size = 2) +
      geom_text_repel(aes(label = paste0(round(`Variação (%)`, 2), "%")),
                      size = 12/3,
                      nudge_y = ifelse(dados_produtos$`Variação (%)` >= 0, 0.2, -0.2),
                      max.overlaps = 20,
                      box.padding = 0.35,
                      point.padding = 0.5,
                      segment.size = 0.2,
                      direction = "both",
                      force = 1,
                      show.legend = FALSE,
                      color = dados_produtos$label_color) +
      geom_hline(yintercept = 0, color = "#9CA3AF", linetype = "dashed", linewidth = 0.7) +
      labs(title = paste("Variação do Produto:", input$produto), x = "Período", y = "Variação (%)") +
      scale_y_continuous(labels = label_percent(scale = 1)) +
      scale_x_date(labels = scales::date_format("%b/%Y", locale = "pt_BR"), breaks = scales::date_breaks("1 month")) +
      scale_color_identity() +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 12, face = "bold"),
        legend.position = "none",
        panel.border = element_rect(color = COR_PRIMARIA, linewidth = 0.6)
      )
  })
}

# Executar o aplicativo Shiny
shinyApp(ui = ui, server = server)
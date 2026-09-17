# ============================================================
# Cidadania Financeira — Coleta e consolidação da Cesta Básica
# ============================================================
# Este script lê as planilhas de coleta de preços (uma subpasta por
# cidade dentro de "data/", com um arquivo .xlsx por mercado/mês),
# calcula o valor mensal da cesta básica, a variação percentual de
# cada produto e o preço médio de cada item, e salva os resultados
# consolidados em .xlsx e .rds — prontos para alimentar o painel
# Shiny (app.R).
#
# COMO ADICIONAR UMA NOVA CIDADE
# -------------------------------
#   1) Crie a pasta data/<NomeDaPasta>/ com as planilhas .xlsx da
#      cidade, seguindo o mesmo padrão das demais (ver README).
#   2) Adicione uma linha ao vetor `cidades` logo abaixo, no formato
#      NomeDaPasta = "Nome de Exibição".
#   3) Rode o script novamente. Nenhuma outra alteração é necessária
#      — o pipeline (leitura, cálculo da cesta, variações, preços)
#      é o mesmo para todas as cidades.
# ============================================================

library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(purrr)
library(writexl)

# ------------------------------------------------------------
# 1. Configuração geral
# ------------------------------------------------------------

# Pasta onde estão as subpastas de cada cidade
DATA_DIR <- "data"

# Nome da subpasta em data/ (esquerda) -> nome de exibição usado nas
# tabelas e no painel (direita). Adicione/edite cidades aqui.
cidades <- c(
  Blumenau     = "Blumenau",
  Gaspar       = "Gaspar",
  Brusque      = "Brusque",
  Bombinhas    = "Bombinhas",
  Indaial      = "Indaial",
  Jaragua      = "Jaraguá do Sul",
  Massaranduba = "Massaranduba",
  Navegantes   = "Navegantes",
  Timbo        = "Timbó",
  Balneario    = "Balneário Camboriú",
  Pomerode     = "Pomerode"
)

month_order <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
                 "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")

# Data limite: não gera meses "futuros" na expansão do calendário
data_atual <- Sys.Date()

# Quantidade padrão (por mês, para 1 pessoa adulta) de cada produto da
# cesta básica — metodologia DIEESE, Região 3 (Santa Catarina)
quantidade_data <- tibble::tibble(
  Produto = c("Arroz tipo 1", "Açúcar Refinado", "Café em pó", "Farinha de Trigo", "Feijão Preto",
              "Manteiga", "Óleo de Soja", "Carne", "Pão Francês", "Batata", "Tomate", "Leite", "Banana"),
  Quantidade = c(3, 3, 0.6, 1.5, 4.5, 0.75, 0.9, 6.6, 6, 6, 9, 7.5, 1.2)
)

# ------------------------------------------------------------
# 2. Funções auxiliares
# ------------------------------------------------------------

# Expande um grid com todos os meses (até `data_atual`) para uma
# cidade, opcionalmente cruzando também com uma lista de produtos.
criar_todos_meses <- function(anos, cidade, produtos = NULL) {
  grid <- if (is.null(produtos)) {
    expand.grid(Ano = anos, Mês = month_order, Cidade = cidade, stringsAsFactors = FALSE)
  } else {
    expand.grid(Ano = anos, Mês = month_order, Cidade = cidade, Produto = produtos,
                stringsAsFactors = FALSE)
  }
  
  grid %>%
    mutate(Mês_num = match(Mês, month_order),
           Data = as.Date(paste(Ano, Mês_num, "01", sep = "-"))) %>%
    filter(Data <= data_atual) %>%
    select(-Data, -Mês_num)
}

# Constrói a coluna `Período` (primeiro dia do mês) a partir de Ano e Mês
# (nome em português). Usa match() em vez de lubridate::dmy(), pois dmy()
# depende do locale do sistema para reconhecer nomes de mês em português —
# o que falha silenciosamente (gerando NA) em máquinas sem o locale pt_BR
# instalado (comum em servidores Linux "limpos", Windows sem idioma
# português, ou runners de CI). match() funciona sempre, independente do
# sistema operacional ou do locale configurado.
construir_periodo <- function(ano, mes) {
  as.Date(paste(ano, match(as.character(mes), month_order), "01", sep = "-"))
}

# Remove acentos comuns do português (sem depender de iconv, cujo
# comportamento varia entre sistemas operacionais).
remover_acentos <- function(x) {
  padroes <- c("Á" = "A", "Â" = "A", "Ã" = "A", "À" = "A", "Ä" = "A",
               "É" = "E", "Ê" = "E", "È" = "E", "Ë" = "E",
               "Í" = "I", "Î" = "I", "Ì" = "I", "Ï" = "I",
               "Ó" = "O", "Ô" = "O", "Õ" = "O", "Ò" = "O", "Ö" = "O",
               "Ú" = "U", "Û" = "U", "Ù" = "U", "Ü" = "U",
               "Ç" = "C")
  for (de in names(padroes)) x <- gsub(de, padroes[[de]], x, fixed = TRUE)
  x
}

# Versão de month_order em maiúsculas e sem acento, para comparação
MONTH_ORDER_NORM <- remover_acentos(toupper(month_order))

# Converte diferentes formas de escrever o mês (nome completo em
# qualquer capitalização/acentuação, número com ou sem zero à
# esquerda, ex.: "SETEMBRO", "setembro", "Setembro", "9", "09") para
# o nome padronizado usado em `month_order` (ex.: "Setembro"). Retorna
# NA e avisa se não reconhecer o valor, em vez de falhar silenciosamente.
normalizar_mes <- function(x) {
  x <- trimws(x)
  
  # Formato numérico: "9", "09", "9.0" etc.
  if (grepl("^[0-9]{1,2}(\\.0+)?$", x)) {
    num <- as.integer(round(as.numeric(x)))
    if (!is.na(num) && num >= 1 && num <= 12) return(month_order[num])
    warning(sprintf("Número de mês inválido: '%s'.", x))
    return(NA_character_)
  }
  
  # Formato texto: ignora maiúsculas/minúsculas e acentos
  idx <- match(remover_acentos(toupper(x)), MONTH_ORDER_NORM)
  if (is.na(idx)) {
    warning(sprintf("Mês não reconhecido: '%s'. Verifique o cabeçalho da planilha.", x))
    return(NA_character_)
  }
  month_order[idx]
}

# Extrai Mês/Ano (do cabeçalho da 5ª coluna) e o nome do mercado (do
# cabeçalho da 2ª coluna) de uma planilha bruta, e organiza as colunas.
processar_planilha <- function(tbl, cidade) {
  col_mes_ano <- names(tbl)[5]   # ex.: "Setembro / 2026", "SETEMBRO/2026", "09/2026"
  col_mercado <- names(tbl)[2]   # nome do mercado pesquisado
  
  partes <- str_split(col_mes_ano, "/")[[1]]
  
  novas_colunas <- tibble(
    `Mês`   = rep(normalizar_mes(partes[1]), nrow(tbl)),
    Ano     = rep(trimws(partes[2]), nrow(tbl)),
    Cidade  = rep(cidade, nrow(tbl)),
    Mercado = rep(col_mercado, nrow(tbl))
  )
  
  bind_cols(tbl, novas_colunas) %>%
    select(-c(2:8)) %>%
    rename(
      Produto = MERCADO,
      `Preço médio` = ...9
    ) %>%
    mutate_at(vars(2), as.numeric) %>%
    filter(!is.na(`Preço médio`)) %>%
    mutate(
      `Preço médio` = round(`Preço médio`, 2),
      Ano = trimws(Ano)
    )
}

# Lê todas as planilhas .xlsx de uma cidade e as empilha em um único
# data frame. Retorna NULL (com aviso) se a pasta não existir ou
# estiver vazia, para que o pipeline possa seguir para as demais cidades.
ler_planilhas_cidade <- function(pasta, cidade, data_dir = DATA_DIR) {
  caminho <- file.path(data_dir, pasta)
  
  if (!dir.exists(caminho)) {
    warning(sprintf("Pasta '%s' não encontrada — cidade '%s' ignorada.", caminho, cidade))
    return(NULL)
  }
  
  file_paths <- list.files(path = caminho, pattern = "\\.xlsx$", full.names = TRUE)
  
  if (length(file_paths) == 0) {
    warning(sprintf("Nenhuma planilha .xlsx em '%s' — cidade '%s' ignorada.", caminho, cidade))
    return(NULL)
  }
  
  map_dfr(file_paths, ~ processar_planilha(read_excel(.x), cidade))
}

# Preço médio de cada produto no mês, excluindo o menor/maior valor
# entre mercados quando a diferença relativa entre eles for grande
# (> 30%), para reduzir o efeito de outliers pontuais de coleta.
calcular_media_produto <- function(df_cidade) {
  df_cidade %>%
    mutate(Ano = trimws(Ano)) %>%
    group_by(Cidade, Ano, Mês, Produto) %>%
    mutate(
      n_obs     = n(),
      min_value = min(`Preço médio`),
      max_value = max(`Preço médio`),
      diff_rel  = ifelse(max_value == 0, 0, (max_value - min_value) / max_value),
      rank_asc  = rank(`Preço médio`,  ties.method = "first"),
      rank_desc = rank(-`Preço médio`, ties.method = "first")
    ) %>%
    filter(n_obs < 3 | diff_rel <= 0.3 | (rank_asc != 1 & rank_desc != 1)) %>%
    select(-n_obs, -min_value, -max_value, -diff_rel, -rank_asc, -rank_desc) %>%
    summarise(
      `Média (produto)` = mean(`Preço médio`),
      Quantidade = first(Quantidade),
      .groups = "drop"
    ) %>%
    mutate(Total = `Média (produto)` * Quantidade)
}

# Soma o total (já tratado) de cada produto = valor da cesta no mês
calcular_cesta <- function(med_prod) {
  med_prod %>%
    group_by(Cidade, Ano, Mês) %>%
    summarise(Cesta = sum(Total, na.rm = TRUE), .groups = "drop")
}

# Preenche os meses sem coleta (para o calendário ficar completo) e
# calcula a variação percentual mensal da cesta.
expandir_cesta <- function(cesta) {
  anos   <- unique(trimws(cesta$Ano))
  cidade <- unique(cesta$Cidade)
  todos_meses <- criar_todos_meses(anos, cidade)
  
  full_join(cesta, todos_meses, by = c("Ano", "Mês", "Cidade")) %>%
    distinct(Ano, Mês, Cidade, .keep_all = TRUE) %>%
    mutate(Mês = factor(Mês, levels = month_order)) %>%
    arrange(Ano, Mês) %>%
    mutate(
      `Variação (%)` = round((Cesta - lag(Cesta)) / lag(Cesta) * 100, 2),
      Cesta = round(Cesta, 2),
      `Período` = construir_periodo(Ano, Mês)
    )
}

# Variação percentual de cada produto, com TODOS os meses do calendário
# preenchidos (os meses sem coleta ficam como NA — útil para os gráficos).
calcular_var_prod <- function(med_prod) {
  var_prod <- med_prod %>%
    select(Cidade, Ano, Mês, Produto, `Média (produto)` = Total)
  
  anos     <- unique(trimws(var_prod$Ano))
  cidade   <- unique(var_prod$Cidade)
  produtos <- unique(var_prod$Produto)
  todos_meses <- criar_todos_meses(anos, cidade, produtos)
  
  todos_meses %>%
    left_join(var_prod, by = c("Ano", "Mês", "Cidade", "Produto")) %>%
    arrange(Produto, Ano, Mês) %>%
    group_by(Produto) %>%
    mutate(`Variação (%)` = round((`Média (produto)` - lag(`Média (produto)`)) / lag(`Média (produto)`) * 100, 2)) %>%
    ungroup() %>%
    arrange(Ano, Mês) %>%
    mutate(`Período` = construir_periodo(Ano, Mês))
}

# Preço médio e variação de cada produto, apenas nos meses em que
# houve coleta (sem preencher os meses faltantes) — usado na aba "Preços".
calcular_precos <- function(med_prod) {
  med_prod %>%
    select(Cidade, Ano, Mês, Produto, `Média (produto)`) %>%
    arrange(Produto, Ano, Mês) %>%
    group_by(Produto) %>%
    mutate(`Variação (%)` = round((`Média (produto)` - lag(`Média (produto)`)) / lag(`Média (produto)`) * 100, 2)) %>%
    ungroup() %>%
    arrange(Ano, Mês) %>%
    mutate(`Período` = construir_periodo(Ano, Mês))
}

# Executa o pipeline completo (leitura -> cesta -> variações -> preços)
# para uma única cidade. Retorna NULL se não houver dados para ela.
processar_cidade <- function(pasta, cidade, data_dir = DATA_DIR) {
  message(sprintf("Processando %s...", cidade))
  
  df_bruto <- ler_planilhas_cidade(pasta, cidade, data_dir)
  if (is.null(df_bruto)) return(NULL)
  
  df_bruto <- df_bruto %>%
    mutate(
      Mercado = str_to_title(Mercado),
      Mercado = gsub("Komprao", "Komprão", Mercado),
      Produto = gsub("Açucar Refinado", "Açúcar Refinado", Produto)
    )
  
  df_cidade <- df_bruto %>%
    left_join(quantidade_data, by = "Produto") %>%
    mutate(
      Total = `Preço médio` * Quantidade,
      Mês   = factor(Mês, levels = month_order)
    ) %>%
    arrange(Ano, Mês)
  
  med_prod <- calcular_media_produto(df_cidade)
  
  list(
    CT       = expandir_cesta(calcular_cesta(med_prod)),
    VAR_PROD = calcular_var_prod(med_prod),
    PRECOS   = calcular_precos(med_prod)
  )
}

# ------------------------------------------------------------
# 3. Processamento de todas as cidades
# ------------------------------------------------------------

resultados <- imap(cidades, ~ processar_cidade(pasta = .y, cidade = .x))
resultados <- compact(resultados)  # remove cidades sem planilhas (NULL)

if (length(resultados) == 0) {
  stop("Nenhuma cidade pôde ser processada. Verifique se a pasta '", DATA_DIR,
       "' contém as subpastas com as planilhas de cada cidade.")
}

CT       <- map_dfr(resultados, "CT")
VAR_PROD <- map_dfr(resultados, "VAR_PROD")
PRECOS   <- map_dfr(resultados, "PRECOS")

# ------------------------------------------------------------
# 4. Exportação dos resultados
# ------------------------------------------------------------

write_xlsx(CT,       path = "CT.xlsx")
write_xlsx(VAR_PROD, path = "VAR_PROD.xlsx")
write_xlsx(PRECOS,   path = "PRECOS.xlsx")

# .rds é mais rápido de ler e preserva os tipos das colunas — é o
# formato consumido pelo app.R
saveRDS(CT,       "CT.rds")
saveRDS(VAR_PROD, "VAR_PROD.rds")
saveRDS(PRECOS,   "PRECOS.rds")

message("Concluído! Cidades processadas: ", paste(names(resultados), collapse = ", "))
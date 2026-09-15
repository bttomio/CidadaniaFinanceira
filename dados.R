library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(purrr)
library(lubridate)

#### BLUMENAU #####

# Define a função para criar a tabela de todos os meses
criar_todos_meses <- function(anos, meses, data_atual) {
  meses_num <- 1:12
  names(meses_num) <- meses
  
  expand.grid(Ano = anos, Mês = meses, Cidade = unique(Blumenau_CT$Cidade)) %>%
    mutate(Mês_num = meses_num[Mês],  
           Data = as.Date(paste(Ano, Mês_num, "01", sep = "-"))) %>%  
    filter(Data <= data_atual) %>%  
    select(-Data, -Mês_num)  
}

# 1. Definir as variáveis
data_atual <- Sys.Date()
month_order <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", 
                 "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
meses <- month_order

# 2. Lê os arquivos de Excel
file_paths <- list.files(full.names = TRUE, path = "data/Blumenau", pattern='*.xlsx')
file_name <- basename(file_paths)
file_name_parts <- strsplit(file_name, " - ")

# Função para processar os dados
process_tibble <- function(tibble) {
  col_name <- names(tibble)[5]
  col_name_2 <- names(tibble)[2]
  
  # Dividir o caminho em partes
  parts <- str_split(col_name, "/")[[1]]
  
  # Criar a tabela com colunas adicionais, removendo espaços do Ano
  new_df <- tibble(
    `Mês` = rep(parts[1], nrow(tibble)),
    Ano = rep(trimws(parts[2]), nrow(tibble)),  # Remove espaços do Ano
    Cidade = rep('Blumenau', nrow(tibble)),
    Mercado = rep(col_name_2, nrow(tibble))
  )
  
  # Combinar as novas colunas
  processed_tibble <- bind_cols(tibble, new_df) %>%
    select(-c(2:8)) %>%
    rename(
      Produto = MERCADO,
      `Preço médio` = ...9
    ) %>%
    mutate_at(vars(2), as.numeric) %>%
    filter(!is.na(`Preço médio`)) %>%
    mutate(`Preço médio` = round(`Preço médio`, 2)) %>%
    mutate(Ano = trimws(Ano))  # Garante que Ano esteja sem espaços
  
  return(processed_tibble)
}

# 3. Processamento dos dados
data_list <- list()
for (i in seq_along(file_paths)) {
  # Extrair informações do nome do arquivo
  month_year <- file_name_parts[[i]][1]
  name_with_extension <- file_name_parts[[i]][2]
  name <- gsub(".xlsx", "", name_with_extension)
  
  # Extrair mês e ano, removendo espaços
  month_year_parts <- strsplit(trimws(month_year), " ")[[1]]
  month <- month_year_parts[1]
  year <- trimws(month_year_parts[2])  # Remove espaços do ano
  
  # Nome da lista
  list_name <- paste(name, month, year, sep = ".")
  
  # Ler o arquivo Excel
  data <- read_excel(file_paths[i])
  
  # Adicionar à lista de dados
  data_list[[list_name]] <- data
}

# Processar os dados
processed_list <- map(data_list, process_tibble)
df_Blumenau <- do.call(rbind.data.frame, processed_list)

# 4. Limpeza e transformação final
df_Blumenau <- df_Blumenau %>%
  mutate(Mercado = str_to_title(Mercado)) %>%
  mutate(Mercado = gsub("Komprao", "Komprão", Mercado)) %>%
  mutate(Produto = gsub("Açucar Refinado", "Açúcar Refinado", Produto))

# 5. Quantidade de produtos
quantidade_data <- tibble::tibble(
  Produto = c("Arroz tipo 1", "Açúcar Refinado", "Café em pó", "Farinha de Trigo", "Feijão Preto", 
              "Manteiga", "Óleo de Soja", "Carne", "Pão Francês", "Batata", "Tomate", "Leite", "Banana"),
  Quantidade = c(3, 3, 0.6, 1.5, 4.5, 0.75, 0.9, 6.6, 6, 6, 9, 7.5, 1.2)
)

# 6. Juntar com as quantidades e calcular o total
df_Blumenau_CT <- df_Blumenau %>%
  left_join(quantidade_data, by = "Produto") %>%
  mutate(Total = `Preço médio` * Quantidade) %>% 
  mutate(Mês = factor(Mês, levels = month_order)) %>%  
  arrange(Ano, Mês)

# 7. Calcular o preço médio de cada produto (excluindo o menor e maior valor entre os mercados)
Blumenau_MED_PROD <- df_Blumenau_CT %>%
  mutate(Ano = trimws(Ano)) %>%  # Remove espaços antes do cálculo
  group_by(Cidade, Ano, Mês, Produto) %>%
  mutate(
    n_obs = n(),
    min_value = min(`Preço médio`),
    max_value = max(`Preço médio`),
    diff_rel = ifelse(max_value == 0, 0, (max_value - min_value) / max_value),
    rank_asc  = rank(`Preço médio`,  ties.method = "first"),
    rank_desc = rank(-`Preço médio`, ties.method = "first")) %>%
  filter(
    n_obs < 3 | diff_rel <= 0.3 | (rank_asc != 1 & rank_desc != 1)) %>%
  select(-n_obs, -min_value, -max_value, -diff_rel, -rank_asc, -rank_desc) %>%
  summarise(
    `Média (produto)` = mean(`Preço médio`),
    Quantidade = first(Quantidade),
    .groups = 'drop'
  ) %>%
  mutate(Total = `Média (produto)` * Quantidade)

# 7b. Calcular a cesta da cidade somando o total (já tratado) de cada produto
Blumenau_CT <- Blumenau_MED_PROD %>%
  group_by(Cidade, Ano, Mês) %>%
  summarise(Cesta = sum(Total, na.rm = TRUE), .groups = 'drop')

# Redefinir 'anos' com base nos dados processados
anos <- unique(trimws(Blumenau_CT$Ano))

# 8. Expansão para incluir todos os meses
todos_meses <- criar_todos_meses(anos, meses, data_atual)

Blumenau_CT <- full_join(Blumenau_CT, todos_meses, by = c("Ano", "Mês", "Cidade")) %>%
  distinct(Ano, Mês, Cidade, .keep_all = TRUE) %>%
  mutate(Mês = factor(Mês, levels = month_order)) %>%
  arrange(Ano, Mês) %>%
  mutate(
    `Variação (%)` = (Cesta - lag(Cesta)) / lag(Cesta) * 100
  ) %>% 
  mutate(Cesta = round(Cesta, 2)) %>% 
  mutate(`Variação (%)` = round(`Variação (%)`, 2)) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

# 9. Variação percentual de cada produto (reaproveitando a média já tratada no passo 7)
Blumenau_VAR_PROD <- Blumenau_MED_PROD %>%
  select(Cidade, Ano, Mês, Produto, `Média (produto)` = Total)

# 10. Expansão para incluir todos os meses para o VAR_PROD
meses_num <- 1:12
todos_meses_VAR_PROD <- expand.grid(Ano = anos, 
                                    Mês = meses, 
                                    Cidade = unique(Blumenau_VAR_PROD$Cidade),
                                    Produto = unique(Blumenau_VAR_PROD$Produto)) %>%
  mutate(Mês_num = meses_num[Mês],  
         Data = as.Date(paste(Ano, Mês_num, "01", sep = "-"))) %>%  
  filter(Data <= data_atual) %>%  
  select(-Data, -Mês_num)  

Blumenau_VAR_PROD <- todos_meses_VAR_PROD %>%
  left_join(Blumenau_VAR_PROD, by = c("Ano", "Mês", "Cidade", "Produto")) %>% 
  arrange(Produto, Ano, Mês) %>% 
  group_by(Produto) %>%
  mutate(
    `Variação (%)` = round((`Média (produto)` - lag(`Média (produto)`)) / lag(`Média (produto)`) * 100, 2)
  ) %>%
  ungroup() %>% 
  arrange(Ano, Mês) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

# 11. Cálculo de média de preço de cada item (reaproveitando o valor já tratado no passo 7)
df_Blumenau_MED <- Blumenau_MED_PROD %>%
  select(Cidade, Ano, Mês, Produto, `Média (produto)`) %>%
  arrange(Produto, Ano, Mês) %>% 
  group_by(Produto) %>%
  mutate(
    `Variação (%)` = round((`Média (produto)` - lag(`Média (produto)`)) / lag(`Média (produto)`) * 100, 2)
  ) %>%
  ungroup() %>% 
  arrange(Ano, Mês) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

#### GASPAR #####

# Define a função para criar a tabela de todos os meses
criar_todos_meses <- function(anos, meses, data_atual) {
  meses_num <- 1:12
  names(meses_num) <- meses
  
  expand.grid(Ano = anos, Mês = meses, Cidade = unique(Gaspar_CT$Cidade)) %>%
    mutate(Mês_num = meses_num[Mês],  
           Data = as.Date(paste(Ano, Mês_num, "01", sep = "-"))) %>%  
    filter(Data <= data_atual) %>%  
    select(-Data, -Mês_num)  
}

# 1. Definir as variáveis
data_atual <- Sys.Date()
meses <- month_order

# 2. Lê os arquivos de Excel
file_paths <- list.files(full.names = TRUE, path = "data/Gaspar", pattern='*.xlsx')
file_name <- basename(file_paths)
file_name_parts <- strsplit(file_name, " - ")

# Função para processar os dados
process_tibble <- function(tibble) {
  col_name <- names(tibble)[5]
  col_name_2 <- names(tibble)[2]
  
  # Dividir o caminho em partes
  parts <- str_split(col_name, "/")[[1]]
  
  # Criar a tabela com colunas adicionais, removendo espaços do Ano
  new_df <- tibble(
    `Mês` = rep(parts[1], nrow(tibble)),
    Ano = rep(trimws(parts[2]), nrow(tibble)),  # Remove espaços do Ano
    Cidade = rep('Gaspar', nrow(tibble)),
    Mercado = rep(col_name_2, nrow(tibble))
  )
  
  # Combinar as novas colunas
  processed_tibble <- bind_cols(tibble, new_df) %>%
    select(-c(2:8)) %>%
    rename(
      Produto = MERCADO,
      `Preço médio` = ...9
    ) %>%
    mutate_at(vars(2), as.numeric) %>%
    filter(!is.na(`Preço médio`)) %>%
    mutate(`Preço médio` = round(`Preço médio`, 2)) %>%
    mutate(Ano = trimws(Ano))  # Garante que Ano esteja sem espaços
  
  return(processed_tibble)
}

# 3. Processamento dos dados
data_list <- list()
for (i in seq_along(file_paths)) {
  # Extrair informações do nome do arquivo
  month_year <- file_name_parts[[i]][1]
  name_with_extension <- file_name_parts[[i]][2]
  name <- gsub(".xlsx", "", name_with_extension)
  
  # Extrair mês e ano, removendo espaços
  month_year_parts <- strsplit(trimws(month_year), " ")[[1]]
  month <- month_year_parts[1]
  year <- trimws(month_year_parts[2])  # Remove espaços do ano
  
  # Nome da lista
  list_name <- paste(name, month, year, sep = ".")
  
  # Ler o arquivo Excel
  data <- read_excel(file_paths[i])
  
  # Adicionar à lista de dados
  data_list[[list_name]] <- data
}

# Processar os dados
processed_list <- map(data_list, process_tibble)
df_Gaspar <- do.call(rbind.data.frame, processed_list)

# 4. Limpeza e transformação final
df_Gaspar <- df_Gaspar %>%
  mutate(Mercado = str_to_title(Mercado)) %>%
  mutate(Mercado = gsub("Komprao", "Komprão", Mercado)) %>%
  mutate(Produto = gsub("Açucar Refinado", "Açúcar Refinado", Produto))

# 5. Quantidade de produtos
quantidade_data <- tibble::tibble(
  Produto = c("Arroz tipo 1", "Açúcar Refinado", "Café em pó", "Farinha de Trigo", "Feijão Preto", 
              "Manteiga", "Óleo de Soja", "Carne", "Pão Francês", "Batata", "Tomate", "Leite", "Banana"),
  Quantidade = c(3, 3, 0.6, 1.5, 4.5, 0.75, 0.9, 6.6, 6, 6, 9, 7.5, 1.2)
)

# 6. Juntar com as quantidades e calcular o total
df_Gaspar_CT <- df_Gaspar %>%
  left_join(quantidade_data, by = "Produto") %>%
  mutate(Total = `Preço médio` * Quantidade) %>% 
  mutate(Mês = factor(Mês, levels = month_order)) %>%  
  arrange(Ano, Mês)

# 7. Calcular o preço médio de cada produto (excluindo o menor e maior valor entre os mercados)
Gaspar_MED_PROD <- df_Gaspar_CT %>%
  mutate(Ano = trimws(Ano)) %>%  # Remove espaços antes do cálculo
  group_by(Cidade, Ano, Mês, Produto) %>%
  mutate(
    n_obs = n(),
    min_value = min(`Preço médio`),
    max_value = max(`Preço médio`),
    diff_rel = ifelse(max_value == 0, 0, (max_value - min_value) / max_value),
    rank_asc  = rank(`Preço médio`,  ties.method = "first"),
    rank_desc = rank(-`Preço médio`, ties.method = "first")) %>%
  filter(
    n_obs < 3 | diff_rel <= 0.3 | (rank_asc != 1 & rank_desc != 1)) %>%
  select(-n_obs, -min_value, -max_value, -diff_rel, -rank_asc, -rank_desc) %>%
  summarise(
    `Média (produto)` = mean(`Preço médio`),
    Quantidade = first(Quantidade),
    .groups = 'drop'
  ) %>%
  mutate(Total = `Média (produto)` * Quantidade)

# 7b. Calcular a cesta da cidade somando o total (já tratado) de cada produto
Gaspar_CT <- Gaspar_MED_PROD %>%
  group_by(Cidade, Ano, Mês) %>%
  summarise(Cesta = sum(Total, na.rm = TRUE), .groups = 'drop')

# Redefinir 'anos' com base nos dados processados
anos <- unique(trimws(Gaspar_CT$Ano))

# 8. Expansão para incluir todos os meses
todos_meses <- criar_todos_meses(anos, meses, data_atual)

Gaspar_CT <- full_join(Gaspar_CT, todos_meses, by = c("Ano", "Mês", "Cidade")) %>%
  distinct(Ano, Mês, Cidade, .keep_all = TRUE) %>%
  mutate(Mês = factor(Mês, levels = month_order)) %>%
  arrange(Ano, Mês) %>%
  mutate(
    `Variação (%)` = (Cesta - lag(Cesta)) / lag(Cesta) * 100
  ) %>% 
  mutate(Cesta = round(Cesta, 2)) %>% 
  mutate(`Variação (%)` = round(`Variação (%)`, 2)) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

# 9. Variação percentual de cada produto (reaproveitando a média já tratada no passo 7)
Gaspar_VAR_PROD <- Gaspar_MED_PROD %>%
  select(Cidade, Ano, Mês, Produto, `Média (produto)` = Total)

# 10. Expansão para incluir todos os meses para o VAR_PROD
meses_num <- 1:12
todos_meses_VAR_PROD <- expand.grid(Ano = anos, 
                                    Mês = meses, 
                                    Cidade = unique(Gaspar_VAR_PROD$Cidade),
                                    Produto = unique(Gaspar_VAR_PROD$Produto)) %>%
  mutate(Mês_num = meses_num[Mês],  
         Data = as.Date(paste(Ano, Mês_num, "01", sep = "-"))) %>%  
  filter(Data <= data_atual) %>%  
  select(-Data, -Mês_num)  

Gaspar_VAR_PROD <- todos_meses_VAR_PROD %>%
  left_join(Gaspar_VAR_PROD, by = c("Ano", "Mês", "Cidade", "Produto")) %>% 
  arrange(Produto, Ano, Mês) %>% 
  group_by(Produto) %>%
  mutate(
    `Variação (%)` = round((`Média (produto)` - lag(`Média (produto)`)) / lag(`Média (produto)`) * 100, 2)
  ) %>%
  ungroup() %>% 
  arrange(Ano, Mês) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

# 11. Cálculo de média de preço de cada item (reaproveitando o valor já tratado no passo 7)
df_Gaspar_MED <- Gaspar_MED_PROD %>%
  select(Cidade, Ano, Mês, Produto, `Média (produto)`) %>%
  arrange(Produto, Ano, Mês) %>% 
  group_by(Produto) %>%
  mutate(
    `Variação (%)` = round((`Média (produto)` - lag(`Média (produto)`)) / lag(`Média (produto)`) * 100, 2)
  ) %>%
  ungroup() %>% 
  arrange(Ano, Mês) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

#### BRUSQUE #####

# Define a função para criar a tabela de todos os meses
criar_todos_meses <- function(anos, meses, data_atual) {
  meses_num <- 1:12
  names(meses_num) <- meses
  
  expand.grid(Ano = anos, Mês = meses, Cidade = unique(Brusque_CT$Cidade)) %>%
    mutate(Mês_num = meses_num[Mês],  
           Data = as.Date(paste(Ano, Mês_num, "01", sep = "-"))) %>%  
    filter(Data <= data_atual) %>%  
    select(-Data, -Mês_num)  
}

# 1. Definir as variáveis
data_atual <- Sys.Date()
meses <- month_order

# 2. Lê os arquivos de Excel
file_paths <- list.files(full.names = TRUE, path = "data/Brusque", pattern='*.xlsx')
file_name <- basename(file_paths)
file_name_parts <- strsplit(file_name, " - ")

# Função para processar os dados
process_tibble <- function(tibble) {
  col_name <- names(tibble)[5]
  col_name_2 <- names(tibble)[2]
  
  # Dividir o caminho em partes
  parts <- str_split(col_name, "/")[[1]]
  
  # Criar a tabela com colunas adicionais, removendo espaços do Ano
  new_df <- tibble(
    `Mês` = rep(parts[1], nrow(tibble)),
    Ano = rep(trimws(parts[2]), nrow(tibble)),  # Remove espaços do Ano
    Cidade = rep('Brusque', nrow(tibble)),
    Mercado = rep(col_name_2, nrow(tibble))
  )
  
  # Combinar as novas colunas
  processed_tibble <- bind_cols(tibble, new_df) %>%
    select(-c(2:8)) %>%
    rename(
      Produto = MERCADO,
      `Preço médio` = ...9
    ) %>%
    mutate_at(vars(2), as.numeric) %>%
    filter(!is.na(`Preço médio`)) %>%
    mutate(`Preço médio` = round(`Preço médio`, 2)) %>%
    mutate(Ano = trimws(Ano))  # Garante que Ano esteja sem espaços
  
  return(processed_tibble)
}

# 3. Processamento dos dados
data_list <- list()
for (i in seq_along(file_paths)) {
  # Extrair informações do nome do arquivo
  month_year <- file_name_parts[[i]][1]
  name_with_extension <- file_name_parts[[i]][2]
  name <- gsub(".xlsx", "", name_with_extension)
  
  # Extrair mês e ano, removendo espaços
  month_year_parts <- strsplit(trimws(month_year), " ")[[1]]
  month <- month_year_parts[1]
  year <- trimws(month_year_parts[2])  # Remove espaços do ano
  
  # Nome da lista
  list_name <- paste(name, month, year, sep = ".")
  
  # Ler o arquivo Excel
  data <- read_excel(file_paths[i])
  
  # Adicionar à lista de dados
  data_list[[list_name]] <- data
}

# Processar os dados
processed_list <- map(data_list, process_tibble)
df_Brusque <- do.call(rbind.data.frame, processed_list)

# 4. Limpeza e transformação final
df_Brusque <- df_Brusque %>%
  mutate(Mercado = str_to_title(Mercado)) %>%
  mutate(Mercado = gsub("Komprao", "Komprão", Mercado)) %>%
  mutate(Produto = gsub("Açucar Refinado", "Açúcar Refinado", Produto))

# 5. Quantidade de produtos
quantidade_data <- tibble::tibble(
  Produto = c("Arroz tipo 1", "Açúcar Refinado", "Café em pó", "Farinha de Trigo", "Feijão Preto", 
              "Manteiga", "Óleo de Soja", "Carne", "Pão Francês", "Batata", "Tomate", "Leite", "Banana"),
  Quantidade = c(3, 3, 0.6, 1.5, 4.5, 0.75, 0.9, 6.6, 6, 6, 9, 7.5, 1.2)
)

# 6. Juntar com as quantidades e calcular o total
df_Brusque_CT <- df_Brusque %>%
  left_join(quantidade_data, by = "Produto") %>%
  mutate(Total = `Preço médio` * Quantidade) %>% 
  mutate(Mês = factor(Mês, levels = month_order)) %>%  
  arrange(Ano, Mês)

# 7. Calcular o preço médio de cada produto (excluindo o menor e maior valor entre os mercados)
Brusque_MED_PROD <- df_Brusque_CT %>%
  mutate(Ano = trimws(Ano)) %>%  # Remove espaços antes do cálculo
  group_by(Cidade, Ano, Mês, Produto) %>%
  mutate(
    n_obs = n(),
    min_value = min(`Preço médio`),
    max_value = max(`Preço médio`),
    diff_rel = ifelse(max_value == 0, 0, (max_value - min_value) / max_value),
    rank_asc  = rank(`Preço médio`,  ties.method = "first"),
    rank_desc = rank(-`Preço médio`, ties.method = "first")) %>%
  filter(
    n_obs < 3 | diff_rel <= 0.3 | (rank_asc != 1 & rank_desc != 1)) %>%
  select(-n_obs, -min_value, -max_value, -diff_rel, -rank_asc, -rank_desc) %>%
  summarise(
    `Média (produto)` = mean(`Preço médio`),
    Quantidade = first(Quantidade),
    .groups = 'drop'
  ) %>%
  mutate(Total = `Média (produto)` * Quantidade)

# 7b. Calcular a cesta da cidade somando o total (já tratado) de cada produto
Brusque_CT <- Brusque_MED_PROD %>%
  group_by(Cidade, Ano, Mês) %>%
  summarise(Cesta = sum(Total, na.rm = TRUE), .groups = 'drop')

# Redefinir 'anos' com base nos dados processados
anos <- unique(trimws(Brusque_CT$Ano))

# 8. Expansão para incluir todos os meses
todos_meses <- criar_todos_meses(anos, meses, data_atual)

Brusque_CT <- full_join(Brusque_CT, todos_meses, by = c("Ano", "Mês", "Cidade")) %>%
  distinct(Ano, Mês, Cidade, .keep_all = TRUE) %>%
  mutate(Mês = factor(Mês, levels = month_order)) %>%
  arrange(Ano, Mês) %>%
  mutate(
    `Variação (%)` = (Cesta - lag(Cesta)) / lag(Cesta) * 100
  ) %>% 
  mutate(Cesta = round(Cesta, 2)) %>% 
  mutate(`Variação (%)` = round(`Variação (%)`, 2)) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

# 9. Variação percentual de cada produto (reaproveitando a média já tratada no passo 7)
Brusque_VAR_PROD <- Brusque_MED_PROD %>%
  select(Cidade, Ano, Mês, Produto, `Média (produto)` = Total)

# 10. Expansão para incluir todos os meses para o VAR_PROD
meses_num <- 1:12
todos_meses_VAR_PROD <- expand.grid(Ano = anos, 
                                    Mês = meses, 
                                    Cidade = unique(Brusque_VAR_PROD$Cidade),
                                    Produto = unique(Brusque_VAR_PROD$Produto)) %>%
  mutate(Mês_num = meses_num[Mês],  
         Data = as.Date(paste(Ano, Mês_num, "01", sep = "-"))) %>%  
  filter(Data <= data_atual) %>%  
  select(-Data, -Mês_num)  

Brusque_VAR_PROD <- todos_meses_VAR_PROD %>%
  left_join(Brusque_VAR_PROD, by = c("Ano", "Mês", "Cidade", "Produto")) %>% 
  arrange(Produto, Ano, Mês) %>% 
  group_by(Produto) %>%
  mutate(
    `Variação (%)` = round((`Média (produto)` - lag(`Média (produto)`)) / lag(`Média (produto)`) * 100, 2)
  ) %>%
  ungroup() %>% 
  arrange(Ano, Mês) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

# 11. Cálculo de média de preço de cada item (reaproveitando o valor já tratado no passo 7)
df_Brusque_MED <- Brusque_MED_PROD %>%
  select(Cidade, Ano, Mês, Produto, `Média (produto)`) %>%
  arrange(Produto, Ano, Mês) %>% 
  group_by(Produto) %>%
  mutate(
    `Variação (%)` = round((`Média (produto)` - lag(`Média (produto)`)) / lag(`Média (produto)`) * 100, 2)
  ) %>%
  ungroup() %>% 
  arrange(Ano, Mês) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

#### BOMBINHAS #####

# Define a função para criar a tabela de todos os meses
criar_todos_meses <- function(anos, meses, data_atual) {
  meses_num <- 1:12
  names(meses_num) <- meses
  
  expand.grid(Ano = anos, Mês = meses, Cidade = unique(Bombinhas_CT$Cidade)) %>%
    mutate(Mês_num = meses_num[Mês],  
           Data = as.Date(paste(Ano, Mês_num, "01", sep = "-"))) %>%  
    filter(Data <= data_atual) %>%  
    select(-Data, -Mês_num)  
}

# 1. Definir as variáveis
data_atual <- Sys.Date()
meses <- month_order

# 2. Lê os arquivos de Excel
file_paths <- list.files(full.names = TRUE, path = "data/Bombinhas", pattern='*.xlsx')
file_name <- basename(file_paths)
file_name_parts <- strsplit(file_name, " - ")

# Função para processar os dados
process_tibble <- function(tibble) {
  col_name <- names(tibble)[5]
  col_name_2 <- names(tibble)[2]
  
  # Dividir o caminho em partes
  parts <- str_split(col_name, "/")[[1]]
  
  # Criar a tabela com colunas adicionais, removendo espaços do Ano
  new_df <- tibble(
    `Mês` = rep(parts[1], nrow(tibble)),
    Ano = rep(trimws(parts[2]), nrow(tibble)),  # Remove espaços do Ano
    Cidade = rep('Bombinhas', nrow(tibble)),
    Mercado = rep(col_name_2, nrow(tibble))
  )
  
  # Combinar as novas colunas
  processed_tibble <- bind_cols(tibble, new_df) %>%
    select(-c(2:8)) %>%
    rename(
      Produto = MERCADO,
      `Preço médio` = ...9
    ) %>%
    mutate_at(vars(2), as.numeric) %>%
    filter(!is.na(`Preço médio`)) %>%
    mutate(`Preço médio` = round(`Preço médio`, 2)) %>%
    mutate(Ano = trimws(Ano))  # Garante que Ano esteja sem espaços
  
  return(processed_tibble)
}

# 3. Processamento dos dados
data_list <- list()
for (i in seq_along(file_paths)) {
  # Extrair informações do nome do arquivo
  month_year <- file_name_parts[[i]][1]
  name_with_extension <- file_name_parts[[i]][2]
  name <- gsub(".xlsx", "", name_with_extension)
  
  # Extrair mês e ano, removendo espaços
  month_year_parts <- strsplit(trimws(month_year), " ")[[1]]
  month <- month_year_parts[1]
  year <- trimws(month_year_parts[2])  # Remove espaços do ano
  
  # Nome da lista
  list_name <- paste(name, month, year, sep = ".")
  
  # Ler o arquivo Excel
  data <- read_excel(file_paths[i])
  
  # Adicionar à lista de dados
  data_list[[list_name]] <- data
}

# Processar os dados
processed_list <- map(data_list, process_tibble)
df_Bombinhas <- do.call(rbind.data.frame, processed_list)

# 4. Limpeza e transformação final
df_Bombinhas <- df_Bombinhas %>%
  mutate(Mercado = str_to_title(Mercado)) %>%
  mutate(Mercado = gsub("Komprao", "Komprão", Mercado)) %>%
  mutate(Produto = gsub("Açucar Refinado", "Açúcar Refinado", Produto))

# 5. Quantidade de produtos
quantidade_data <- tibble::tibble(
  Produto = c("Arroz tipo 1", "Açúcar Refinado", "Café em pó", "Farinha de Trigo", "Feijão Preto", 
              "Manteiga", "Óleo de Soja", "Carne", "Pão Francês", "Batata", "Tomate", "Leite", "Banana"),
  Quantidade = c(3, 3, 0.6, 1.5, 4.5, 0.75, 0.9, 6.6, 6, 6, 9, 7.5, 1.2)
)

# 6. Juntar com as quantidades e calcular o total
df_Bombinhas_CT <- df_Bombinhas %>%
  left_join(quantidade_data, by = "Produto") %>%
  mutate(Total = `Preço médio` * Quantidade) %>% 
  mutate(Mês = factor(Mês, levels = month_order)) %>%  
  arrange(Ano, Mês)

# 7. Calcular o preço médio de cada produto (excluindo o menor e maior valor entre os mercados)
Bombinhas_MED_PROD <- df_Bombinhas_CT %>%
  mutate(Ano = trimws(Ano)) %>%  # Remove espaços antes do cálculo
  group_by(Cidade, Ano, Mês, Produto) %>%
  mutate(
    n_obs = n(),
    min_value = min(`Preço médio`),
    max_value = max(`Preço médio`),
    diff_rel = ifelse(max_value == 0, 0, (max_value - min_value) / max_value),
    rank_asc  = rank(`Preço médio`,  ties.method = "first"),
    rank_desc = rank(-`Preço médio`, ties.method = "first")) %>%
  filter(
    n_obs < 3 | diff_rel <= 0.3 | (rank_asc != 1 & rank_desc != 1)) %>%
  select(-n_obs, -min_value, -max_value, -diff_rel, -rank_asc, -rank_desc) %>%
  summarise(
    `Média (produto)` = mean(`Preço médio`),
    Quantidade = first(Quantidade),
    .groups = 'drop'
  ) %>%
  mutate(Total = `Média (produto)` * Quantidade)

# 7b. Calcular a cesta da cidade somando o total (já tratado) de cada produto
Bombinhas_CT <- Bombinhas_MED_PROD %>%
  group_by(Cidade, Ano, Mês) %>%
  summarise(Cesta = sum(Total, na.rm = TRUE), .groups = 'drop')

# Redefinir 'anos' com base nos dados processados
anos <- unique(trimws(Bombinhas_CT$Ano))

# 8. Expansão para incluir todos os meses
todos_meses <- criar_todos_meses(anos, meses, data_atual)

Bombinhas_CT <- full_join(Bombinhas_CT, todos_meses, by = c("Ano", "Mês", "Cidade")) %>%
  distinct(Ano, Mês, Cidade, .keep_all = TRUE) %>%
  mutate(Mês = factor(Mês, levels = month_order)) %>%
  arrange(Ano, Mês) %>%
  mutate(
    `Variação (%)` = (Cesta - lag(Cesta)) / lag(Cesta) * 100
  ) %>% 
  mutate(Cesta = round(Cesta, 2)) %>% 
  mutate(`Variação (%)` = round(`Variação (%)`, 2)) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

# 9. Variação percentual de cada produto (reaproveitando a média já tratada no passo 7)
Bombinhas_VAR_PROD <- Bombinhas_MED_PROD %>%
  select(Cidade, Ano, Mês, Produto, `Média (produto)` = Total)

# 10. Expansão para incluir todos os meses para o VAR_PROD
meses_num <- 1:12
todos_meses_VAR_PROD <- expand.grid(Ano = anos, 
                                    Mês = meses, 
                                    Cidade = unique(Bombinhas_VAR_PROD$Cidade),
                                    Produto = unique(Bombinhas_VAR_PROD$Produto)) %>%
  mutate(Mês_num = meses_num[Mês],  
         Data = as.Date(paste(Ano, Mês_num, "01", sep = "-"))) %>%  
  filter(Data <= data_atual) %>%  
  select(-Data, -Mês_num)  

Bombinhas_VAR_PROD <- todos_meses_VAR_PROD %>%
  left_join(Bombinhas_VAR_PROD, by = c("Ano", "Mês", "Cidade", "Produto")) %>% 
  arrange(Produto, Ano, Mês) %>% 
  group_by(Produto) %>%
  mutate(
    `Variação (%)` = round((`Média (produto)` - lag(`Média (produto)`)) / lag(`Média (produto)`) * 100, 2)
  ) %>%
  ungroup() %>% 
  arrange(Ano, Mês) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

# 11. Cálculo de média de preço de cada item (reaproveitando o valor já tratado no passo 7)
df_Bombinhas_MED <- Bombinhas_MED_PROD %>%
  select(Cidade, Ano, Mês, Produto, `Média (produto)`) %>%
  arrange(Produto, Ano, Mês) %>% 
  group_by(Produto) %>%
  mutate(
    `Variação (%)` = round((`Média (produto)` - lag(`Média (produto)`)) / lag(`Média (produto)`) * 100, 2)
  ) %>%
  ungroup() %>% 
  arrange(Ano, Mês) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

#### INDAIAL #####

# Define a função para criar a tabela de todos os meses
criar_todos_meses <- function(anos, meses, data_atual) {
  meses_num <- 1:12
  names(meses_num) <- meses
  
  expand.grid(Ano = anos, Mês = meses, Cidade = unique(Indaial_CT$Cidade)) %>%
    mutate(Mês_num = meses_num[Mês],  
           Data = as.Date(paste(Ano, Mês_num, "01", sep = "-"))) %>%  
    filter(Data <= data_atual) %>%  
    select(-Data, -Mês_num)  
}

# 1. Definir as variáveis
data_atual <- Sys.Date()
meses <- month_order

# 2. Lê os arquivos de Excel
file_paths <- list.files(full.names = TRUE, path = "data/Indaial", pattern='*.xlsx')
file_name <- basename(file_paths)
file_name_parts <- strsplit(file_name, " - ")

# Função para processar os dados
process_tibble <- function(tibble) {
  col_name <- names(tibble)[5]
  col_name_2 <- names(tibble)[2]
  
  # Dividir o caminho em partes
  parts <- str_split(col_name, "/")[[1]]
  
  # Criar a tabela com colunas adicionais, removendo espaços do Ano
  new_df <- tibble(
    `Mês` = rep(parts[1], nrow(tibble)),
    Ano = rep(trimws(parts[2]), nrow(tibble)),  # Remove espaços do Ano
    Cidade = rep('Indaial', nrow(tibble)),
    Mercado = rep(col_name_2, nrow(tibble))
  )
  
  # Combinar as novas colunas
  processed_tibble <- bind_cols(tibble, new_df) %>%
    select(-c(2:8)) %>%
    rename(
      Produto = MERCADO,
      `Preço médio` = ...9
    ) %>%
    mutate_at(vars(2), as.numeric) %>%
    filter(!is.na(`Preço médio`)) %>%
    mutate(`Preço médio` = round(`Preço médio`, 2)) %>%
    mutate(Ano = trimws(Ano))  # Garante que Ano esteja sem espaços
  
  return(processed_tibble)
}

# 3. Processamento dos dados
data_list <- list()
for (i in seq_along(file_paths)) {
  # Extrair informações do nome do arquivo
  month_year <- file_name_parts[[i]][1]
  name_with_extension <- file_name_parts[[i]][2]
  name <- gsub(".xlsx", "", name_with_extension)
  
  # Extrair mês e ano, removendo espaços
  month_year_parts <- strsplit(trimws(month_year), " ")[[1]]
  month <- month_year_parts[1]
  year <- trimws(month_year_parts[2])  # Remove espaços do ano
  
  # Nome da lista
  list_name <- paste(name, month, year, sep = ".")
  
  # Ler o arquivo Excel
  data <- read_excel(file_paths[i])
  
  # Adicionar à lista de dados
  data_list[[list_name]] <- data
}

# Processar os dados
processed_list <- map(data_list, process_tibble)
df_Indaial <- do.call(rbind.data.frame, processed_list)

# 4. Limpeza e transformação final
df_Indaial <- df_Indaial %>%
  mutate(Mercado = str_to_title(Mercado)) %>%
  mutate(Mercado = gsub("Komprao", "Komprão", Mercado)) %>%
  mutate(Produto = gsub("Açucar Refinado", "Açúcar Refinado", Produto))

# 5. Quantidade de produtos
quantidade_data <- tibble::tibble(
  Produto = c("Arroz tipo 1", "Açúcar Refinado", "Café em pó", "Farinha de Trigo", "Feijão Preto", 
              "Manteiga", "Óleo de Soja", "Carne", "Pão Francês", "Batata", "Tomate", "Leite", "Banana"),
  Quantidade = c(3, 3, 0.6, 1.5, 4.5, 0.75, 0.9, 6.6, 6, 6, 9, 7.5, 1.2)
)

# 6. Juntar com as quantidades e calcular o total
df_Indaial_CT <- df_Indaial %>%
  left_join(quantidade_data, by = "Produto") %>%
  mutate(Total = `Preço médio` * Quantidade) %>% 
  mutate(Mês = factor(Mês, levels = month_order)) %>%  
  arrange(Ano, Mês)

# 7. Calcular o preço médio de cada produto (excluindo o menor e maior valor entre os mercados)
Indaial_MED_PROD <- df_Indaial_CT %>%
  mutate(Ano = trimws(Ano)) %>%  # Remove espaços antes do cálculo
  group_by(Cidade, Ano, Mês, Produto) %>%
  mutate(
    n_obs = n(),
    min_value = min(`Preço médio`),
    max_value = max(`Preço médio`),
    diff_rel = ifelse(max_value == 0, 0, (max_value - min_value) / max_value),
    rank_asc  = rank(`Preço médio`,  ties.method = "first"),
    rank_desc = rank(-`Preço médio`, ties.method = "first")) %>%
  filter(
    n_obs < 3 | diff_rel <= 0.3 | (rank_asc != 1 & rank_desc != 1)) %>%
  select(-n_obs, -min_value, -max_value, -diff_rel, -rank_asc, -rank_desc) %>%
  summarise(
    `Média (produto)` = mean(`Preço médio`),
    Quantidade = first(Quantidade),
    .groups = 'drop'
  ) %>%
  mutate(Total = `Média (produto)` * Quantidade)

# 7b. Calcular a cesta da cidade somando o total (já tratado) de cada produto
Indaial_CT <- Indaial_MED_PROD %>%
  group_by(Cidade, Ano, Mês) %>%
  summarise(Cesta = sum(Total, na.rm = TRUE), .groups = 'drop')

# Redefinir 'anos' com base nos dados processados
anos <- unique(trimws(Indaial_CT$Ano))

# 8. Expansão para incluir todos os meses
todos_meses <- criar_todos_meses(anos, meses, data_atual)

Indaial_CT <- full_join(Indaial_CT, todos_meses, by = c("Ano", "Mês", "Cidade")) %>%
  distinct(Ano, Mês, Cidade, .keep_all = TRUE) %>%
  mutate(Mês = factor(Mês, levels = month_order)) %>%
  arrange(Ano, Mês) %>%
  mutate(
    `Variação (%)` = (Cesta - lag(Cesta)) / lag(Cesta) * 100
  ) %>% 
  mutate(Cesta = round(Cesta, 2)) %>% 
  mutate(`Variação (%)` = round(`Variação (%)`, 2)) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

# 9. Variação percentual de cada produto (reaproveitando a média já tratada no passo 7)
Indaial_VAR_PROD <- Indaial_MED_PROD %>%
  select(Cidade, Ano, Mês, Produto, `Média (produto)` = Total)

# 10. Expansão para incluir todos os meses para o VAR_PROD
meses_num <- 1:12
todos_meses_VAR_PROD <- expand.grid(Ano = anos, 
                                    Mês = meses, 
                                    Cidade = unique(Indaial_VAR_PROD$Cidade),
                                    Produto = unique(Indaial_VAR_PROD$Produto)) %>%
  mutate(Mês_num = meses_num[Mês],  
         Data = as.Date(paste(Ano, Mês_num, "01", sep = "-"))) %>%  
  filter(Data <= data_atual) %>%  
  select(-Data, -Mês_num)  

Indaial_VAR_PROD <- todos_meses_VAR_PROD %>%
  left_join(Indaial_VAR_PROD, by = c("Ano", "Mês", "Cidade", "Produto")) %>% 
  arrange(Produto, Ano, Mês) %>% 
  group_by(Produto) %>%
  mutate(
    `Variação (%)` = round((`Média (produto)` - lag(`Média (produto)`)) / lag(`Média (produto)`) * 100, 2)
  ) %>%
  ungroup() %>% 
  arrange(Ano, Mês) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

# 11. Cálculo de média de preço de cada item (reaproveitando o valor já tratado no passo 7)
df_Indaial_MED <- Indaial_MED_PROD %>%
  select(Cidade, Ano, Mês, Produto, `Média (produto)`) %>%
  arrange(Produto, Ano, Mês) %>% 
  group_by(Produto) %>%
  mutate(
    `Variação (%)` = round((`Média (produto)` - lag(`Média (produto)`)) / lag(`Média (produto)`) * 100, 2)
  ) %>%
  ungroup() %>% 
  arrange(Ano, Mês) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

#### JARAGUÁ DO SUL #####

# Define a função para criar a tabela de todos os meses
criar_todos_meses <- function(anos, meses, data_atual) {
  meses_num <- 1:12
  names(meses_num) <- meses
  
  expand.grid(Ano = anos, Mês = meses, Cidade = unique(Jaragua_CT$Cidade)) %>%
    mutate(Mês_num = meses_num[Mês],  
           Data = as.Date(paste(Ano, Mês_num, "01", sep = "-"))) %>%  
    filter(Data <= data_atual) %>%  
    select(-Data, -Mês_num)  
}

# 1. Definir as variáveis
data_atual <- Sys.Date()
meses <- month_order

# 2. Lê os arquivos de Excel
file_paths <- list.files(full.names = TRUE, path = "data/Jaragua", pattern='*.xlsx')
file_name <- basename(file_paths)
file_name_parts <- strsplit(file_name, " - ")

# Função para processar os dados
process_tibble <- function(tibble) {
  col_name <- names(tibble)[5]
  col_name_2 <- names(tibble)[2]
  
  # Dividir o caminho em partes
  parts <- str_split(col_name, "/")[[1]]
  
  # Criar a tabela com colunas adicionais, removendo espaços do Ano
  new_df <- tibble(
    `Mês` = rep(parts[1], nrow(tibble)),
    Ano = rep(trimws(parts[2]), nrow(tibble)),  # Remove espaços do Ano
    Cidade = rep('Jaragua', nrow(tibble)),
    Mercado = rep(col_name_2, nrow(tibble))
  )
  
  # Combinar as novas colunas
  processed_tibble <- bind_cols(tibble, new_df) %>%
    select(-c(2:8)) %>%
    rename(
      Produto = MERCADO,
      `Preço médio` = ...9
    ) %>%
    mutate_at(vars(2), as.numeric) %>%
    filter(!is.na(`Preço médio`)) %>%
    mutate(`Preço médio` = round(`Preço médio`, 2)) %>%
    mutate(Ano = trimws(Ano))  # Garante que Ano esteja sem espaços
  
  return(processed_tibble)
}

# 3. Processamento dos dados
data_list <- list()
for (i in seq_along(file_paths)) {
  # Extrair informações do nome do arquivo
  month_year <- file_name_parts[[i]][1]
  name_with_extension <- file_name_parts[[i]][2]
  name <- gsub(".xlsx", "", name_with_extension)
  
  # Extrair mês e ano, removendo espaços
  month_year_parts <- strsplit(trimws(month_year), " ")[[1]]
  month <- month_year_parts[1]
  year <- trimws(month_year_parts[2])  # Remove espaços do ano
  
  # Nome da lista
  list_name <- paste(name, month, year, sep = ".")
  
  # Ler o arquivo Excel
  data <- read_excel(file_paths[i])
  
  # Adicionar à lista de dados
  data_list[[list_name]] <- data
}

# Processar os dados
processed_list <- map(data_list, process_tibble)
df_Jaragua <- do.call(rbind.data.frame, processed_list)

# 4. Limpeza e transformação final
df_Jaragua <- df_Jaragua %>%
  mutate(Mercado = str_to_title(Mercado)) %>%
  mutate(Mercado = gsub("Komprao", "Komprão", Mercado)) %>%
  mutate(Produto = gsub("Açucar Refinado", "Açúcar Refinado", Produto))

# 5. Quantidade de produtos
quantidade_data <- tibble::tibble(
  Produto = c("Arroz tipo 1", "Açúcar Refinado", "Café em pó", "Farinha de Trigo", "Feijão Preto", 
              "Manteiga", "Óleo de Soja", "Carne", "Pão Francês", "Batata", "Tomate", "Leite", "Banana"),
  Quantidade = c(3, 3, 0.6, 1.5, 4.5, 0.75, 0.9, 6.6, 6, 6, 9, 7.5, 1.2)
)

# 6. Juntar com as quantidades e calcular o total
df_Jaragua_CT <- df_Jaragua %>%
  left_join(quantidade_data, by = "Produto") %>%
  mutate(Total = `Preço médio` * Quantidade) %>% 
  mutate(Mês = factor(Mês, levels = month_order)) %>%  
  arrange(Ano, Mês)

# 7. Calcular o preço médio de cada produto (excluindo o menor e maior valor entre os mercados)
Jaragua_MED_PROD <- df_Jaragua_CT %>%
  mutate(Ano = trimws(Ano)) %>%  # Remove espaços antes do cálculo
  group_by(Cidade, Ano, Mês, Produto) %>%
  mutate(
    n_obs = n(),
    min_value = min(`Preço médio`),
    max_value = max(`Preço médio`),
    diff_rel = ifelse(max_value == 0, 0, (max_value - min_value) / max_value),
    rank_asc  = rank(`Preço médio`,  ties.method = "first"),
    rank_desc = rank(-`Preço médio`, ties.method = "first")) %>%
  filter(
    n_obs < 3 | diff_rel <= 0.3 | (rank_asc != 1 & rank_desc != 1)) %>%
  select(-n_obs, -min_value, -max_value, -diff_rel, -rank_asc, -rank_desc) %>%
  summarise(
    `Média (produto)` = mean(`Preço médio`),
    Quantidade = first(Quantidade),
    .groups = 'drop'
  ) %>%
  mutate(Total = `Média (produto)` * Quantidade)

# 7b. Calcular a cesta da cidade somando o total (já tratado) de cada produto
Jaragua_CT <- Jaragua_MED_PROD %>%
  group_by(Cidade, Ano, Mês) %>%
  summarise(Cesta = sum(Total, na.rm = TRUE), .groups = 'drop')

# Redefinir 'anos' com base nos dados processados
anos <- unique(trimws(Jaragua_CT$Ano))

# 8. Expansão para incluir todos os meses
todos_meses <- criar_todos_meses(anos, meses, data_atual)

Jaragua_CT <- full_join(Jaragua_CT, todos_meses, by = c("Ano", "Mês", "Cidade")) %>%
  distinct(Ano, Mês, Cidade, .keep_all = TRUE) %>%
  mutate(Mês = factor(Mês, levels = month_order)) %>%
  arrange(Ano, Mês) %>%
  mutate(
    `Variação (%)` = (Cesta - lag(Cesta)) / lag(Cesta) * 100
  ) %>% 
  mutate(Cesta = round(Cesta, 2)) %>% 
  mutate(`Variação (%)` = round(`Variação (%)`, 2)) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

# 9. Variação percentual de cada produto (reaproveitando a média já tratada no passo 7)
Jaragua_VAR_PROD <- Jaragua_MED_PROD %>%
  select(Cidade, Ano, Mês, Produto, `Média (produto)` = Total)

# 10. Expansão para incluir todos os meses para o VAR_PROD
meses_num <- 1:12
todos_meses_VAR_PROD <- expand.grid(Ano = anos, 
                                    Mês = meses, 
                                    Cidade = unique(Jaragua_VAR_PROD$Cidade),
                                    Produto = unique(Jaragua_VAR_PROD$Produto)) %>%
  mutate(Mês_num = meses_num[Mês],  
         Data = as.Date(paste(Ano, Mês_num, "01", sep = "-"))) %>%  
  filter(Data <= data_atual) %>%  
  select(-Data, -Mês_num)  

Jaragua_VAR_PROD <- todos_meses_VAR_PROD %>%
  left_join(Jaragua_VAR_PROD, by = c("Ano", "Mês", "Cidade", "Produto")) %>% 
  arrange(Produto, Ano, Mês) %>% 
  group_by(Produto) %>%
  mutate(
    `Variação (%)` = round((`Média (produto)` - lag(`Média (produto)`)) / lag(`Média (produto)`) * 100, 2)
  ) %>%
  ungroup() %>% 
  arrange(Ano, Mês) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

# 11. Cálculo de média de preço de cada item (reaproveitando o valor já tratado no passo 7)
df_Jaragua_MED <- Jaragua_MED_PROD %>%
  select(Cidade, Ano, Mês, Produto, `Média (produto)`) %>%
  arrange(Produto, Ano, Mês) %>% 
  group_by(Produto) %>%
  mutate(
    `Variação (%)` = round((`Média (produto)` - lag(`Média (produto)`)) / lag(`Média (produto)`) * 100, 2)
  ) %>%
  ungroup() %>% 
  arrange(Ano, Mês) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

#### MASSARANDUBA #####

# Define a função para criar a tabela de todos os meses
criar_todos_meses <- function(anos, meses, data_atual) {
  meses_num <- 1:12
  names(meses_num) <- meses
  
  expand.grid(Ano = anos, Mês = meses, Cidade = unique(Massaranduba_CT$Cidade)) %>%
    mutate(Mês_num = meses_num[Mês],  
           Data = as.Date(paste(Ano, Mês_num, "01", sep = "-"))) %>%  
    filter(Data <= data_atual) %>%  
    select(-Data, -Mês_num)  
}

# 1. Definir as variáveis
data_atual <- Sys.Date()
meses <- month_order

# 2. Lê os arquivos de Excel
file_paths <- list.files(full.names = TRUE, path = "data/Massaranduba", pattern='*.xlsx')
file_name <- basename(file_paths)
file_name_parts <- strsplit(file_name, " - ")

# Função para processar os dados
process_tibble <- function(tibble) {
  col_name <- names(tibble)[5]
  col_name_2 <- names(tibble)[2]
  
  # Dividir o caminho em partes
  parts <- str_split(col_name, "/")[[1]]
  
  # Criar a tabela com colunas adicionais, removendo espaços do Ano
  new_df <- tibble(
    `Mês` = rep(parts[1], nrow(tibble)),
    Ano = rep(trimws(parts[2]), nrow(tibble)),  # Remove espaços do Ano
    Cidade = rep('Massaranduba', nrow(tibble)),
    Mercado = rep(col_name_2, nrow(tibble))
  )
  
  # Combinar as novas colunas
  processed_tibble <- bind_cols(tibble, new_df) %>%
    select(-c(2:8)) %>%
    rename(
      Produto = MERCADO,
      `Preço médio` = ...9
    ) %>%
    mutate_at(vars(2), as.numeric) %>%
    filter(!is.na(`Preço médio`)) %>%
    mutate(`Preço médio` = round(`Preço médio`, 2)) %>%
    mutate(Ano = trimws(Ano))  # Garante que Ano esteja sem espaços
  
  return(processed_tibble)
}

# 3. Processamento dos dados
data_list <- list()
for (i in seq_along(file_paths)) {
  # Extrair informações do nome do arquivo
  month_year <- file_name_parts[[i]][1]
  name_with_extension <- file_name_parts[[i]][2]
  name <- gsub(".xlsx", "", name_with_extension)
  
  # Extrair mês e ano, removendo espaços
  month_year_parts <- strsplit(trimws(month_year), " ")[[1]]
  month <- month_year_parts[1]
  year <- trimws(month_year_parts[2])  # Remove espaços do ano
  
  # Nome da lista
  list_name <- paste(name, month, year, sep = ".")
  
  # Ler o arquivo Excel
  data <- read_excel(file_paths[i])
  
  # Adicionar à lista de dados
  data_list[[list_name]] <- data
}

# Processar os dados
processed_list <- map(data_list, process_tibble)
df_Massaranduba <- do.call(rbind.data.frame, processed_list)

# 4. Limpeza e transformação final
df_Massaranduba <- df_Massaranduba %>%
  mutate(Mercado = str_to_title(Mercado)) %>%
  mutate(Mercado = gsub("Komprao", "Komprão", Mercado)) %>%
  mutate(Produto = gsub("Açucar Refinado", "Açúcar Refinado", Produto))

# 5. Quantidade de produtos
quantidade_data <- tibble::tibble(
  Produto = c("Arroz tipo 1", "Açúcar Refinado", "Café em pó", "Farinha de Trigo", "Feijão Preto", 
              "Manteiga", "Óleo de Soja", "Carne", "Pão Francês", "Batata", "Tomate", "Leite", "Banana"),
  Quantidade = c(3, 3, 0.6, 1.5, 4.5, 0.75, 0.9, 6.6, 6, 6, 9, 7.5, 1.2)
)

# 6. Juntar com as quantidades e calcular o total
df_Massaranduba_CT <- df_Massaranduba %>%
  left_join(quantidade_data, by = "Produto") %>%
  mutate(Total = `Preço médio` * Quantidade) %>% 
  mutate(Mês = factor(Mês, levels = month_order)) %>%  
  arrange(Ano, Mês)

# 7. Calcular o preço médio de cada produto (excluindo o menor e maior valor entre os mercados)
Massaranduba_MED_PROD <- df_Massaranduba_CT %>%
  mutate(Ano = trimws(Ano)) %>%  # Remove espaços antes do cálculo
  group_by(Cidade, Ano, Mês, Produto) %>%
  mutate(
    n_obs = n(),
    min_value = min(`Preço médio`),
    max_value = max(`Preço médio`),
    diff_rel = ifelse(max_value == 0, 0, (max_value - min_value) / max_value),
    rank_asc  = rank(`Preço médio`,  ties.method = "first"),
    rank_desc = rank(-`Preço médio`, ties.method = "first")) %>%
  filter(
    n_obs < 3 | diff_rel <= 0.3 | (rank_asc != 1 & rank_desc != 1)) %>%
  select(-n_obs, -min_value, -max_value, -diff_rel, -rank_asc, -rank_desc) %>%
  summarise(
    `Média (produto)` = mean(`Preço médio`),
    Quantidade = first(Quantidade),
    .groups = 'drop'
  ) %>%
  mutate(Total = `Média (produto)` * Quantidade)

# 7b. Calcular a cesta da cidade somando o total (já tratado) de cada produto
Massaranduba_CT <- Massaranduba_MED_PROD %>%
  group_by(Cidade, Ano, Mês) %>%
  summarise(Cesta = sum(Total, na.rm = TRUE), .groups = 'drop')

# Redefinir 'anos' com base nos dados processados
anos <- unique(trimws(Massaranduba_CT$Ano))

# 8. Expansão para incluir todos os meses
todos_meses <- criar_todos_meses(anos, meses, data_atual)

Massaranduba_CT <- full_join(Massaranduba_CT, todos_meses, by = c("Ano", "Mês", "Cidade")) %>%
  distinct(Ano, Mês, Cidade, .keep_all = TRUE) %>%
  mutate(Mês = factor(Mês, levels = month_order)) %>%
  arrange(Ano, Mês) %>%
  mutate(
    `Variação (%)` = (Cesta - lag(Cesta)) / lag(Cesta) * 100
  ) %>% 
  mutate(Cesta = round(Cesta, 2)) %>% 
  mutate(`Variação (%)` = round(`Variação (%)`, 2)) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

# 9. Variação percentual de cada produto (reaproveitando a média já tratada no passo 7)
Massaranduba_VAR_PROD <- Massaranduba_MED_PROD %>%
  select(Cidade, Ano, Mês, Produto, `Média (produto)` = Total)

# 10. Expansão para incluir todos os meses para o VAR_PROD
meses_num <- 1:12
todos_meses_VAR_PROD <- expand.grid(Ano = anos, 
                                    Mês = meses, 
                                    Cidade = unique(Massaranduba_VAR_PROD$Cidade),
                                    Produto = unique(Massaranduba_VAR_PROD$Produto)) %>%
  mutate(Mês_num = meses_num[Mês],  
         Data = as.Date(paste(Ano, Mês_num, "01", sep = "-"))) %>%  
  filter(Data <= data_atual) %>%  
  select(-Data, -Mês_num)  

Massaranduba_VAR_PROD <- todos_meses_VAR_PROD %>%
  left_join(Massaranduba_VAR_PROD, by = c("Ano", "Mês", "Cidade", "Produto")) %>% 
  arrange(Produto, Ano, Mês) %>% 
  group_by(Produto) %>%
  mutate(
    `Variação (%)` = round((`Média (produto)` - lag(`Média (produto)`)) / lag(`Média (produto)`) * 100, 2)
  ) %>%
  ungroup() %>% 
  arrange(Ano, Mês) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

# 11. Cálculo de média de preço de cada item (reaproveitando o valor já tratado no passo 7)
df_Massaranduba_MED <- Massaranduba_MED_PROD %>%
  select(Cidade, Ano, Mês, Produto, `Média (produto)`) %>%
  arrange(Produto, Ano, Mês) %>% 
  group_by(Produto) %>%
  mutate(
    `Variação (%)` = round((`Média (produto)` - lag(`Média (produto)`)) / lag(`Média (produto)`) * 100, 2)
  ) %>%
  ungroup() %>% 
  arrange(Ano, Mês) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

#### NAVEGANTES #####

# Define a função para criar a tabela de todos os meses
criar_todos_meses <- function(anos, meses, data_atual) {
  meses_num <- 1:12
  names(meses_num) <- meses
  
  expand.grid(Ano = anos, Mês = meses, Cidade = unique(Navegantes_CT$Cidade)) %>%
    mutate(Mês_num = meses_num[Mês],  
           Data = as.Date(paste(Ano, Mês_num, "01", sep = "-"))) %>%  
    filter(Data <= data_atual) %>%  
    select(-Data, -Mês_num)  
}

# 1. Definir as variáveis
data_atual <- Sys.Date()
meses <- month_order

# 2. Lê os arquivos de Excel
file_paths <- list.files(full.names = TRUE, path = "data/Navegantes", pattern='*.xlsx')
file_name <- basename(file_paths)
file_name_parts <- strsplit(file_name, " - ")

# Função para processar os dados
process_tibble <- function(tibble) {
  col_name <- names(tibble)[5]
  col_name_2 <- names(tibble)[2]
  
  # Dividir o caminho em partes
  parts <- str_split(col_name, "/")[[1]]
  
  # Criar a tabela com colunas adicionais, removendo espaços do Ano
  new_df <- tibble(
    `Mês` = rep(parts[1], nrow(tibble)),
    Ano = rep(trimws(parts[2]), nrow(tibble)),  # Remove espaços do Ano
    Cidade = rep('Navegantes', nrow(tibble)),
    Mercado = rep(col_name_2, nrow(tibble))
  )
  
  # Combinar as novas colunas
  processed_tibble <- bind_cols(tibble, new_df) %>%
    select(-c(2:8)) %>%
    rename(
      Produto = MERCADO,
      `Preço médio` = ...9
    ) %>%
    mutate_at(vars(2), as.numeric) %>%
    filter(!is.na(`Preço médio`)) %>%
    mutate(`Preço médio` = round(`Preço médio`, 2)) %>%
    mutate(Ano = trimws(Ano))  # Garante que Ano esteja sem espaços
  
  return(processed_tibble)
}

# 3. Processamento dos dados
data_list <- list()
for (i in seq_along(file_paths)) {
  # Extrair informações do nome do arquivo
  month_year <- file_name_parts[[i]][1]
  name_with_extension <- file_name_parts[[i]][2]
  name <- gsub(".xlsx", "", name_with_extension)
  
  # Extrair mês e ano, removendo espaços
  month_year_parts <- strsplit(trimws(month_year), " ")[[1]]
  month <- month_year_parts[1]
  year <- trimws(month_year_parts[2])  # Remove espaços do ano
  
  # Nome da lista
  list_name <- paste(name, month, year, sep = ".")
  
  # Ler o arquivo Excel
  data <- read_excel(file_paths[i])
  
  # Adicionar à lista de dados
  data_list[[list_name]] <- data
}

# Processar os dados
processed_list <- map(data_list, process_tibble)
df_Navegantes <- do.call(rbind.data.frame, processed_list)

# 4. Limpeza e transformação final
df_Navegantes <- df_Navegantes %>%
  mutate(Mercado = str_to_title(Mercado)) %>%
  mutate(Mercado = gsub("Komprao", "Komprão", Mercado)) %>%
  mutate(Produto = gsub("Açucar Refinado", "Açúcar Refinado", Produto))

# 5. Quantidade de produtos
quantidade_data <- tibble::tibble(
  Produto = c("Arroz tipo 1", "Açúcar Refinado", "Café em pó", "Farinha de Trigo", "Feijão Preto", 
              "Manteiga", "Óleo de Soja", "Carne", "Pão Francês", "Batata", "Tomate", "Leite", "Banana"),
  Quantidade = c(3, 3, 0.6, 1.5, 4.5, 0.75, 0.9, 6.6, 6, 6, 9, 7.5, 1.2)
)

# 6. Juntar com as quantidades e calcular o total
df_Navegantes_CT <- df_Navegantes %>%
  left_join(quantidade_data, by = "Produto") %>%
  mutate(Total = `Preço médio` * Quantidade) %>% 
  mutate(Mês = factor(Mês, levels = month_order)) %>%  
  arrange(Ano, Mês)

# 7. Calcular o preço médio de cada produto (excluindo o menor e maior valor entre os mercados)
Navegantes_MED_PROD <- df_Navegantes_CT %>%
  mutate(Ano = trimws(Ano)) %>%  # Remove espaços antes do cálculo
  group_by(Cidade, Ano, Mês, Produto) %>%
  mutate(
    n_obs = n(),
    min_value = min(`Preço médio`),
    max_value = max(`Preço médio`),
    diff_rel = ifelse(max_value == 0, 0, (max_value - min_value) / max_value),
    rank_asc  = rank(`Preço médio`,  ties.method = "first"),
    rank_desc = rank(-`Preço médio`, ties.method = "first")) %>%
  filter(
    n_obs < 3 | diff_rel <= 0.3 | (rank_asc != 1 & rank_desc != 1)) %>%
  select(-n_obs, -min_value, -max_value, -diff_rel, -rank_asc, -rank_desc) %>%
  summarise(
    `Média (produto)` = mean(`Preço médio`),
    Quantidade = first(Quantidade),
    .groups = 'drop'
  ) %>%
  mutate(Total = `Média (produto)` * Quantidade)

# 7b. Calcular a cesta da cidade somando o total (já tratado) de cada produto
Navegantes_CT <- Navegantes_MED_PROD %>%
  group_by(Cidade, Ano, Mês) %>%
  summarise(Cesta = sum(Total, na.rm = TRUE), .groups = 'drop')

# Redefinir 'anos' com base nos dados processados
anos <- unique(trimws(Navegantes_CT$Ano))

# 8. Expansão para incluir todos os meses
todos_meses <- criar_todos_meses(anos, meses, data_atual)

Navegantes_CT <- full_join(Navegantes_CT, todos_meses, by = c("Ano", "Mês", "Cidade")) %>%
  distinct(Ano, Mês, Cidade, .keep_all = TRUE) %>%
  mutate(Mês = factor(Mês, levels = month_order)) %>%
  arrange(Ano, Mês) %>%
  mutate(
    `Variação (%)` = (Cesta - lag(Cesta)) / lag(Cesta) * 100
  ) %>% 
  mutate(Cesta = round(Cesta, 2)) %>% 
  mutate(`Variação (%)` = round(`Variação (%)`, 2)) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

# 9. Variação percentual de cada produto (reaproveitando a média já tratada no passo 7)
Navegantes_VAR_PROD <- Navegantes_MED_PROD %>%
  select(Cidade, Ano, Mês, Produto, `Média (produto)` = Total)

# 10. Expansão para incluir todos os meses para o VAR_PROD
meses_num <- 1:12
todos_meses_VAR_PROD <- expand.grid(Ano = anos, 
                                    Mês = meses, 
                                    Cidade = unique(Navegantes_VAR_PROD$Cidade),
                                    Produto = unique(Navegantes_VAR_PROD$Produto)) %>%
  mutate(Mês_num = meses_num[Mês],  
         Data = as.Date(paste(Ano, Mês_num, "01", sep = "-"))) %>%  
  filter(Data <= data_atual) %>%  
  select(-Data, -Mês_num)  

Navegantes_VAR_PROD <- todos_meses_VAR_PROD %>%
  left_join(Navegantes_VAR_PROD, by = c("Ano", "Mês", "Cidade", "Produto")) %>% 
  arrange(Produto, Ano, Mês) %>% 
  group_by(Produto) %>%
  mutate(
    `Variação (%)` = round((`Média (produto)` - lag(`Média (produto)`)) / lag(`Média (produto)`) * 100, 2)
  ) %>%
  ungroup() %>% 
  arrange(Ano, Mês) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

# 11. Cálculo de média de preço de cada item (reaproveitando o valor já tratado no passo 7)
df_Navegantes_MED <- Navegantes_MED_PROD %>%
  select(Cidade, Ano, Mês, Produto, `Média (produto)`) %>%
  arrange(Produto, Ano, Mês) %>% 
  group_by(Produto) %>%
  mutate(
    `Variação (%)` = round((`Média (produto)` - lag(`Média (produto)`)) / lag(`Média (produto)`) * 100, 2)
  ) %>%
  ungroup() %>% 
  arrange(Ano, Mês) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

#### TIMBÓ #####

# Define a função para criar a tabela de todos os meses
criar_todos_meses <- function(anos, meses, data_atual) {
  meses_num <- 1:12
  names(meses_num) <- meses
  
  expand.grid(Ano = anos, Mês = meses, Cidade = unique(Timbo_CT$Cidade)) %>%
    mutate(Mês_num = meses_num[Mês],  
           Data = as.Date(paste(Ano, Mês_num, "01", sep = "-"))) %>%  
    filter(Data <= data_atual) %>%  
    select(-Data, -Mês_num)  
}

# 1. Definir as variáveis
data_atual <- Sys.Date()
meses <- month_order

# 2. Lê os arquivos de Excel
file_paths <- list.files(full.names = TRUE, path = "data/Timbo", pattern='*.xlsx')
file_name <- basename(file_paths)
file_name_parts <- strsplit(file_name, " - ")

# Função para processar os dados
process_tibble <- function(tibble) {
  col_name <- names(tibble)[5]
  col_name_2 <- names(tibble)[2]
  
  # Dividir o caminho em partes
  parts <- str_split(col_name, "/")[[1]]
  
  # Criar a tabela com colunas adicionais, removendo espaços do Ano
  new_df <- tibble(
    `Mês` = rep(parts[1], nrow(tibble)),
    Ano = rep(trimws(parts[2]), nrow(tibble)),  # Remove espaços do Ano
    Cidade = rep('Timbo', nrow(tibble)),
    Mercado = rep(col_name_2, nrow(tibble))
  )
  
  # Combinar as novas colunas
  processed_tibble <- bind_cols(tibble, new_df) %>%
    select(-c(2:8)) %>%
    rename(
      Produto = MERCADO,
      `Preço médio` = ...9
    ) %>%
    mutate_at(vars(2), as.numeric) %>%
    filter(!is.na(`Preço médio`)) %>%
    mutate(`Preço médio` = round(`Preço médio`, 2)) %>%
    mutate(Ano = trimws(Ano))  # Garante que Ano esteja sem espaços
  
  return(processed_tibble)
}

# 3. Processamento dos dados
data_list <- list()
for (i in seq_along(file_paths)) {
  # Extrair informações do nome do arquivo
  month_year <- file_name_parts[[i]][1]
  name_with_extension <- file_name_parts[[i]][2]
  name <- gsub(".xlsx", "", name_with_extension)
  
  # Extrair mês e ano, removendo espaços
  month_year_parts <- strsplit(trimws(month_year), " ")[[1]]
  month <- month_year_parts[1]
  year <- trimws(month_year_parts[2])  # Remove espaços do ano
  
  # Nome da lista
  list_name <- paste(name, month, year, sep = ".")
  
  # Ler o arquivo Excel
  data <- read_excel(file_paths[i])
  
  # Adicionar à lista de dados
  data_list[[list_name]] <- data
}

# Processar os dados
processed_list <- map(data_list, process_tibble)
df_Timbo <- do.call(rbind.data.frame, processed_list)

# 4. Limpeza e transformação final
df_Timbo <- df_Timbo %>%
  mutate(Mercado = str_to_title(Mercado)) %>%
  mutate(Mercado = gsub("Komprao", "Komprão", Mercado)) %>%
  mutate(Produto = gsub("Açucar Refinado", "Açúcar Refinado", Produto))

# 5. Quantidade de produtos
quantidade_data <- tibble::tibble(
  Produto = c("Arroz tipo 1", "Açúcar Refinado", "Café em pó", "Farinha de Trigo", "Feijão Preto", 
              "Manteiga", "Óleo de Soja", "Carne", "Pão Francês", "Batata", "Tomate", "Leite", "Banana"),
  Quantidade = c(3, 3, 0.6, 1.5, 4.5, 0.75, 0.9, 6.6, 6, 6, 9, 7.5, 1.2)
)

# 6. Juntar com as quantidades e calcular o total
df_Timbo_CT <- df_Timbo %>%
  left_join(quantidade_data, by = "Produto") %>%
  mutate(Total = `Preço médio` * Quantidade) %>% 
  mutate(Mês = factor(Mês, levels = month_order)) %>%  
  arrange(Ano, Mês)

# 7. Calcular o preço médio de cada produto (excluindo o menor e maior valor entre os mercados)
Timbo_MED_PROD <- df_Timbo_CT %>%
  mutate(Ano = trimws(Ano)) %>%  # Remove espaços antes do cálculo
  group_by(Cidade, Ano, Mês, Produto) %>%
  mutate(
    n_obs = n(),
    min_value = min(`Preço médio`),
    max_value = max(`Preço médio`),
    diff_rel = ifelse(max_value == 0, 0, (max_value - min_value) / max_value),
    rank_asc  = rank(`Preço médio`,  ties.method = "first"),
    rank_desc = rank(-`Preço médio`, ties.method = "first")) %>%
  filter(
    n_obs < 3 | diff_rel <= 0.3 | (rank_asc != 1 & rank_desc != 1)) %>%
  select(-n_obs, -min_value, -max_value, -diff_rel, -rank_asc, -rank_desc) %>%
  summarise(
    `Média (produto)` = mean(`Preço médio`),
    Quantidade = first(Quantidade),
    .groups = 'drop'
  ) %>%
  mutate(Total = `Média (produto)` * Quantidade)

# 7b. Calcular a cesta da cidade somando o total (já tratado) de cada produto
Timbo_CT <- Timbo_MED_PROD %>%
  group_by(Cidade, Ano, Mês) %>%
  summarise(Cesta = sum(Total, na.rm = TRUE), .groups = 'drop')

# Redefinir 'anos' com base nos dados processados
anos <- unique(trimws(Timbo_CT$Ano))

# 8. Expansão para incluir todos os meses
todos_meses <- criar_todos_meses(anos, meses, data_atual)

Timbo_CT <- full_join(Timbo_CT, todos_meses, by = c("Ano", "Mês", "Cidade")) %>%
  distinct(Ano, Mês, Cidade, .keep_all = TRUE) %>%
  mutate(Mês = factor(Mês, levels = month_order)) %>%
  arrange(Ano, Mês) %>%
  mutate(
    `Variação (%)` = (Cesta - lag(Cesta)) / lag(Cesta) * 100
  ) %>% 
  mutate(Cesta = round(Cesta, 2)) %>% 
  mutate(`Variação (%)` = round(`Variação (%)`, 2)) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

# 9. Variação percentual de cada produto (reaproveitando a média já tratada no passo 7)
Timbo_VAR_PROD <- Timbo_MED_PROD %>%
  select(Cidade, Ano, Mês, Produto, `Média (produto)` = Total)

# 10. Expansão para incluir todos os meses para o VAR_PROD
meses_num <- 1:12
todos_meses_VAR_PROD <- expand.grid(Ano = anos, 
                                    Mês = meses, 
                                    Cidade = unique(Timbo_VAR_PROD$Cidade),
                                    Produto = unique(Timbo_VAR_PROD$Produto)) %>%
  mutate(Mês_num = meses_num[Mês],  
         Data = as.Date(paste(Ano, Mês_num, "01", sep = "-"))) %>%  
  filter(Data <= data_atual) %>%  
  select(-Data, -Mês_num)  

Timbo_VAR_PROD <- todos_meses_VAR_PROD %>%
  left_join(Timbo_VAR_PROD, by = c("Ano", "Mês", "Cidade", "Produto")) %>% 
  arrange(Produto, Ano, Mês) %>% 
  group_by(Produto) %>%
  mutate(
    `Variação (%)` = round((`Média (produto)` - lag(`Média (produto)`)) / lag(`Média (produto)`) * 100, 2)
  ) %>%
  ungroup() %>% 
  arrange(Ano, Mês) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

# 11. Cálculo de média de preço de cada item (reaproveitando o valor já tratado no passo 7)
df_Timbo_MED <- Timbo_MED_PROD %>%
  select(Cidade, Ano, Mês, Produto, `Média (produto)`) %>%
  arrange(Produto, Ano, Mês) %>% 
  group_by(Produto) %>%
  mutate(
    `Variação (%)` = round((`Média (produto)` - lag(`Média (produto)`)) / lag(`Média (produto)`) * 100, 2)
  ) %>%
  ungroup() %>% 
  arrange(Ano, Mês) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

#### BALNEÁRIO CAMBORIÚ #####

# Define a função para criar a tabela de todos os meses
criar_todos_meses <- function(anos, meses, data_atual) {
  meses_num <- 1:12
  names(meses_num) <- meses
  
  expand.grid(Ano = anos, Mês = meses, Cidade = unique(Balneario_CT$Cidade)) %>%
    mutate(Mês_num = meses_num[Mês],  
           Data = as.Date(paste(Ano, Mês_num, "01", sep = "-"))) %>%  
    filter(Data <= data_atual) %>%  
    select(-Data, -Mês_num)  
}

# 1. Definir as variáveis
data_atual <- Sys.Date()
meses <- month_order

# 2. Lê os arquivos de Excel
file_paths <- list.files(full.names = TRUE, path = "data/Balneario", pattern='*.xlsx')
file_name <- basename(file_paths)
file_name_parts <- strsplit(file_name, " - ")

# Função para processar os dados
process_tibble <- function(tibble) {
  col_name <- names(tibble)[5]
  col_name_2 <- names(tibble)[2]
  
  # Dividir o caminho em partes
  parts <- str_split(col_name, "/")[[1]]
  
  # Criar a tabela com colunas adicionais, removendo espaços do Ano
  new_df <- tibble(
    `Mês` = rep(parts[1], nrow(tibble)),
    Ano = rep(trimws(parts[2]), nrow(tibble)),  # Remove espaços do Ano
    Cidade = rep('Balneario', nrow(tibble)),
    Mercado = rep(col_name_2, nrow(tibble))
  )
  
  # Combinar as novas colunas
  processed_tibble <- bind_cols(tibble, new_df) %>%
    select(-c(2:8)) %>%
    rename(
      Produto = MERCADO,
      `Preço médio` = ...9
    ) %>%
    mutate_at(vars(2), as.numeric) %>%
    filter(!is.na(`Preço médio`)) %>%
    mutate(`Preço médio` = round(`Preço médio`, 2)) %>%
    mutate(Ano = trimws(Ano))  # Garante que Ano esteja sem espaços
  
  return(processed_tibble)
}

# 3. Processamento dos dados
data_list <- list()
for (i in seq_along(file_paths)) {
  # Extrair informações do nome do arquivo
  month_year <- file_name_parts[[i]][1]
  name_with_extension <- file_name_parts[[i]][2]
  name <- gsub(".xlsx", "", name_with_extension)
  
  # Extrair mês e ano, removendo espaços
  month_year_parts <- strsplit(trimws(month_year), " ")[[1]]
  month <- month_year_parts[1]
  year <- trimws(month_year_parts[2])  # Remove espaços do ano
  
  # Nome da lista
  list_name <- paste(name, month, year, sep = ".")
  
  # Ler o arquivo Excel
  data <- read_excel(file_paths[i])
  
  # Adicionar à lista de dados
  data_list[[list_name]] <- data
}

# Processar os dados
processed_list <- map(data_list, process_tibble)
df_Balneario <- do.call(rbind.data.frame, processed_list)

# 4. Limpeza e transformação final
df_Balneario <- df_Balneario %>%
  mutate(Mercado = str_to_title(Mercado)) %>%
  mutate(Mercado = gsub("Komprao", "Komprão", Mercado)) %>%
  mutate(Produto = gsub("Açucar Refinado", "Açúcar Refinado", Produto))

# 5. Quantidade de produtos
quantidade_data <- tibble::tibble(
  Produto = c("Arroz tipo 1", "Açúcar Refinado", "Café em pó", "Farinha de Trigo", "Feijão Preto", 
              "Manteiga", "Óleo de Soja", "Carne", "Pão Francês", "Batata", "Tomate", "Leite", "Banana"),
  Quantidade = c(3, 3, 0.6, 1.5, 4.5, 0.75, 0.9, 6.6, 6, 6, 9, 7.5, 1.2)
)

# 6. Juntar com as quantidades e calcular o total
df_Balneario_CT <- df_Balneario %>%
  left_join(quantidade_data, by = "Produto") %>%
  mutate(Total = `Preço médio` * Quantidade) %>% 
  mutate(Mês = factor(Mês, levels = month_order)) %>%  
  arrange(Ano, Mês)

# 7. Calcular o preço médio de cada produto (excluindo o menor e maior valor entre os mercados)
Balneario_MED_PROD <- df_Balneario_CT %>%
  mutate(Ano = trimws(Ano)) %>%  # Remove espaços antes do cálculo
  group_by(Cidade, Ano, Mês, Produto) %>%
  mutate(
    n_obs = n(),
    min_value = min(`Preço médio`),
    max_value = max(`Preço médio`),
    diff_rel = ifelse(max_value == 0, 0, (max_value - min_value) / max_value),
    rank_asc  = rank(`Preço médio`,  ties.method = "first"),
    rank_desc = rank(-`Preço médio`, ties.method = "first")) %>%
  filter(
    n_obs < 3 | diff_rel <= 0.3 | (rank_asc != 1 & rank_desc != 1)) %>%
  select(-n_obs, -min_value, -max_value, -diff_rel, -rank_asc, -rank_desc) %>%
  summarise(
    `Média (produto)` = mean(`Preço médio`),
    Quantidade = first(Quantidade),
    .groups = 'drop'
  ) %>%
  mutate(Total = `Média (produto)` * Quantidade)

# 7b. Calcular a cesta da cidade somando o total (já tratado) de cada produto
Balneario_CT <- Balneario_MED_PROD %>%
  group_by(Cidade, Ano, Mês) %>%
  summarise(Cesta = sum(Total, na.rm = TRUE), .groups = 'drop')

# Redefinir 'anos' com base nos dados processados
anos <- unique(trimws(Balneario_CT$Ano))

# 8. Expansão para incluir todos os meses
todos_meses <- criar_todos_meses(anos, meses, data_atual)

Balneario_CT <- full_join(Balneario_CT, todos_meses, by = c("Ano", "Mês", "Cidade")) %>%
  distinct(Ano, Mês, Cidade, .keep_all = TRUE) %>%
  mutate(Mês = factor(Mês, levels = month_order)) %>%
  arrange(Ano, Mês) %>%
  mutate(
    `Variação (%)` = (Cesta - lag(Cesta)) / lag(Cesta) * 100
  ) %>% 
  mutate(Cesta = round(Cesta, 2)) %>% 
  mutate(`Variação (%)` = round(`Variação (%)`, 2)) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

# 9. Variação percentual de cada produto (reaproveitando a média já tratada no passo 7)
Balneario_VAR_PROD <- Balneario_MED_PROD %>%
  select(Cidade, Ano, Mês, Produto, `Média (produto)` = Total)

# 10. Expansão para incluir todos os meses para o VAR_PROD
meses_num <- 1:12
todos_meses_VAR_PROD <- expand.grid(Ano = anos, 
                                    Mês = meses, 
                                    Cidade = unique(Balneario_VAR_PROD$Cidade),
                                    Produto = unique(Balneario_VAR_PROD$Produto)) %>%
  mutate(Mês_num = meses_num[Mês],  
         Data = as.Date(paste(Ano, Mês_num, "01", sep = "-"))) %>%  
  filter(Data <= data_atual) %>%  
  select(-Data, -Mês_num)  

Balneario_VAR_PROD <- todos_meses_VAR_PROD %>%
  left_join(Balneario_VAR_PROD, by = c("Ano", "Mês", "Cidade", "Produto")) %>% 
  arrange(Produto, Ano, Mês) %>% 
  group_by(Produto) %>%
  mutate(
    `Variação (%)` = round((`Média (produto)` - lag(`Média (produto)`)) / lag(`Média (produto)`) * 100, 2)
  ) %>%
  ungroup() %>% 
  arrange(Ano, Mês) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

# 11. Cálculo de média de preço de cada item (reaproveitando o valor já tratado no passo 7)
df_Balneario_MED <- Balneario_MED_PROD %>%
  select(Cidade, Ano, Mês, Produto, `Média (produto)`) %>%
  arrange(Produto, Ano, Mês) %>% 
  group_by(Produto) %>%
  mutate(
    `Variação (%)` = round((`Média (produto)` - lag(`Média (produto)`)) / lag(`Média (produto)`) * 100, 2)
  ) %>%
  ungroup() %>% 
  arrange(Ano, Mês) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

#### POMERODE #####

# Define a função para criar a tabela de todos os meses
criar_todos_meses <- function(anos, meses, data_atual) {
  meses_num <- 1:12
  names(meses_num) <- meses
  
  expand.grid(Ano = anos, Mês = meses, Cidade = unique(Pomerode_CT$Cidade)) %>%
    mutate(Mês_num = meses_num[Mês],  
           Data = as.Date(paste(Ano, Mês_num, "01", sep = "-"))) %>%  
    filter(Data <= data_atual) %>%  
    select(-Data, -Mês_num)  
}

# 1. Definir as variáveis
data_atual <- Sys.Date()
meses <- month_order

# 2. Lê os arquivos de Excel
file_paths <- list.files(full.names = TRUE, path = "data/Pomerode", pattern='*.xlsx')
file_name <- basename(file_paths)
file_name_parts <- strsplit(file_name, " - ")

# Função para processar os dados
process_tibble <- function(tibble) {
  col_name <- names(tibble)[5]
  col_name_2 <- names(tibble)[2]
  
  # Dividir o caminho em partes
  parts <- str_split(col_name, "/")[[1]]
  
  # Criar a tabela com colunas adicionais, removendo espaços do Ano
  new_df <- tibble(
    `Mês` = rep(parts[1], nrow(tibble)),
    Ano = rep(trimws(parts[2]), nrow(tibble)),  # Remove espaços do Ano
    Cidade = rep('Pomerode', nrow(tibble)),
    Mercado = rep(col_name_2, nrow(tibble))
  )
  
  # Combinar as novas colunas
  processed_tibble <- bind_cols(tibble, new_df) %>%
    select(-c(2:8)) %>%
    rename(
      Produto = MERCADO,
      `Preço médio` = ...9
    ) %>%
    mutate_at(vars(2), as.numeric) %>%
    filter(!is.na(`Preço médio`)) %>%
    mutate(`Preço médio` = round(`Preço médio`, 2)) %>%
    mutate(Ano = trimws(Ano))  # Garante que Ano esteja sem espaços
  
  return(processed_tibble)
}

# 3. Processamento dos dados
data_list <- list()
for (i in seq_along(file_paths)) {
  # Extrair informações do nome do arquivo
  month_year <- file_name_parts[[i]][1]
  name_with_extension <- file_name_parts[[i]][2]
  name <- gsub(".xlsx", "", name_with_extension)
  
  # Extrair mês e ano, removendo espaços
  month_year_parts <- strsplit(trimws(month_year), " ")[[1]]
  month <- month_year_parts[1]
  year <- trimws(month_year_parts[2])  # Remove espaços do ano
  
  # Nome da lista
  list_name <- paste(name, month, year, sep = ".")
  
  # Ler o arquivo Excel
  data <- read_excel(file_paths[i])
  
  # Adicionar à lista de dados
  data_list[[list_name]] <- data
}

# Processar os dados
processed_list <- map(data_list, process_tibble)
df_Pomerode <- do.call(rbind.data.frame, processed_list)

# 4. Limpeza e transformação final
df_Pomerode <- df_Pomerode %>%
  mutate(Mercado = str_to_title(Mercado)) %>%
  mutate(Mercado = gsub("Komprao", "Komprão", Mercado)) %>%
  mutate(Produto = gsub("Açucar Refinado", "Açúcar Refinado", Produto))

# 5. Quantidade de produtos
quantidade_data <- tibble::tibble(
  Produto = c("Arroz tipo 1", "Açúcar Refinado", "Café em pó", "Farinha de Trigo", "Feijão Preto", 
              "Manteiga", "Óleo de Soja", "Carne", "Pão Francês", "Batata", "Tomate", "Leite", "Banana"),
  Quantidade = c(3, 3, 0.6, 1.5, 4.5, 0.75, 0.9, 6.6, 6, 6, 9, 7.5, 1.2)
)

# 6. Juntar com as quantidades e calcular o total
df_Pomerode_CT <- df_Pomerode %>%
  left_join(quantidade_data, by = "Produto") %>%
  mutate(Total = `Preço médio` * Quantidade) %>% 
  mutate(Mês = factor(Mês, levels = month_order)) %>%  
  arrange(Ano, Mês)

# 7. Calcular o preço médio de cada produto (excluindo o menor e maior valor entre os mercados)
Pomerode_MED_PROD <- df_Pomerode_CT %>%
  mutate(Ano = trimws(Ano)) %>%  # Remove espaços antes do cálculo
  group_by(Cidade, Ano, Mês, Produto) %>%
  mutate(
    n_obs = n(),
    min_value = min(`Preço médio`),
    max_value = max(`Preço médio`),
    diff_rel = ifelse(max_value == 0, 0, (max_value - min_value) / max_value),
    rank_asc  = rank(`Preço médio`,  ties.method = "first"),
    rank_desc = rank(-`Preço médio`, ties.method = "first")) %>%
  filter(
    n_obs < 3 | diff_rel <= 0.3 | (rank_asc != 1 & rank_desc != 1)) %>%
  select(-n_obs, -min_value, -max_value, -diff_rel, -rank_asc, -rank_desc) %>%
  summarise(
    `Média (produto)` = mean(`Preço médio`),
    Quantidade = first(Quantidade),
    .groups = 'drop'
  ) %>%
  mutate(Total = `Média (produto)` * Quantidade)

# 7b. Calcular a cesta da cidade somando o total (já tratado) de cada produto
Pomerode_CT <- Pomerode_MED_PROD %>%
  group_by(Cidade, Ano, Mês) %>%
  summarise(Cesta = sum(Total, na.rm = TRUE), .groups = 'drop')

# Redefinir 'anos' com base nos dados processados
anos <- unique(trimws(Pomerode_CT$Ano))

# 8. Expansão para incluir todos os meses
todos_meses <- criar_todos_meses(anos, meses, data_atual)

Pomerode_CT <- full_join(Pomerode_CT, todos_meses, by = c("Ano", "Mês", "Cidade")) %>%
  distinct(Ano, Mês, Cidade, .keep_all = TRUE) %>%
  mutate(Mês = factor(Mês, levels = month_order)) %>%
  arrange(Ano, Mês) %>%
  mutate(
    `Variação (%)` = (Cesta - lag(Cesta)) / lag(Cesta) * 100
  ) %>% 
  mutate(Cesta = round(Cesta, 2)) %>% 
  mutate(`Variação (%)` = round(`Variação (%)`, 2)) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

# 9. Variação percentual de cada produto (reaproveitando a média já tratada no passo 7)
Pomerode_VAR_PROD <- Pomerode_MED_PROD %>%
  select(Cidade, Ano, Mês, Produto, `Média (produto)` = Total)

# 10. Expansão para incluir todos os meses para o VAR_PROD
meses_num <- 1:12
todos_meses_VAR_PROD <- expand.grid(Ano = anos, 
                                    Mês = meses, 
                                    Cidade = unique(Pomerode_VAR_PROD$Cidade),
                                    Produto = unique(Pomerode_VAR_PROD$Produto)) %>%
  mutate(Mês_num = meses_num[Mês],  
         Data = as.Date(paste(Ano, Mês_num, "01", sep = "-"))) %>%  
  filter(Data <= data_atual) %>%  
  select(-Data, -Mês_num)  

Pomerode_VAR_PROD <- todos_meses_VAR_PROD %>%
  left_join(Pomerode_VAR_PROD, by = c("Ano", "Mês", "Cidade", "Produto")) %>% 
  arrange(Produto, Ano, Mês) %>% 
  group_by(Produto) %>%
  mutate(
    `Variação (%)` = round((`Média (produto)` - lag(`Média (produto)`)) / lag(`Média (produto)`) * 100, 2)
  ) %>%
  ungroup() %>% 
  arrange(Ano, Mês) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

# 11. Cálculo de média de preço de cada item (reaproveitando o valor já tratado no passo 7)
df_Pomerode_MED <- Pomerode_MED_PROD %>%
  select(Cidade, Ano, Mês, Produto, `Média (produto)`) %>%
  arrange(Produto, Ano, Mês) %>% 
  group_by(Produto) %>%
  mutate(
    `Variação (%)` = round((`Média (produto)` - lag(`Média (produto)`)) / lag(`Média (produto)`) * 100, 2)
  ) %>%
  ungroup() %>% 
  arrange(Ano, Mês) %>% 
  mutate(`Período` = dmy(paste("01", Mês, Ano)))

# SAVE DATA ####

CT <- rbind(Blumenau_CT, Gaspar_CT, Brusque_CT, Bombinhas_CT,
            Indaial_CT, Jaragua_CT, Massaranduba_CT,
            Navegantes_CT, Timbo_CT, Balneario_CT, Pomerode_CT) %>%
  mutate(Cidade = str_replace(Cidade, "Jaragua", "Jaraguá do Sul")) %>%
  mutate(Cidade = str_replace(Cidade, "Timbo", "Timbó")) %>%
  mutate(Cidade = str_replace(Cidade, "Balneario", "Balneário Camboriú"))

VAR_PROD <- rbind(Blumenau_VAR_PROD, Gaspar_VAR_PROD, Brusque_VAR_PROD, Bombinhas_VAR_PROD,
                  Indaial_VAR_PROD, Jaragua_VAR_PROD, Massaranduba_VAR_PROD,
                  Navegantes_VAR_PROD, Timbo_VAR_PROD, Balneario_VAR_PROD, Pomerode_VAR_PROD) %>%
  mutate(Cidade = str_replace(Cidade, "Jaragua", "Jaraguá do Sul")) %>%
  mutate(Cidade = str_replace(Cidade, "Timbo", "Timbó")) %>%
  mutate(Cidade = str_replace(Cidade, "Balneario", "Balneário Camboriú"))

PRECOS <- rbind(df_Blumenau_MED, df_Gaspar_MED, df_Brusque_MED, df_Bombinhas_MED,
                df_Indaial_MED, df_Jaragua_MED, df_Massaranduba_MED,
                df_Navegantes_MED, df_Timbo_MED, df_Balneario_MED, df_Pomerode_MED) %>%
  mutate(Cidade = str_replace(Cidade, "Jaragua", "Jaraguá do Sul")) %>%
  mutate(Cidade = str_replace(Cidade, "Timbo", "Timbó")) %>%
  mutate(Cidade = str_replace(Cidade, "Balneario", "Balneário Camboriú"))

library(writexl)

write_xlsx(CT, path = "CT.xlsx")
write_xlsx(VAR_PROD, path = "VAR_PROD.xlsx")
write_xlsx(PRECOS, path = "PRECOS.xlsx")

# Mais eficiente que arquivo de Excel
saveRDS(CT,       "CT.rds")
saveRDS(VAR_PROD, "VAR_PROD.rds")
saveRDS(PRECOS,   "PRECOS.rds")
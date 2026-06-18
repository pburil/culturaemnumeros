#===============================================================================
# PROJETO CULTURA EM NÚMEROS - EIXO 1 (ESTADOS) - SCRIPT FINAL E CORRIGIDO
#===============================================================================
library(tidyverse)
library(arrow)
library(scales)
library(gt)
library(janitor)
library(purrr)
library(rbcb)

setwd("E:\\Pedro Buril\\Ministerio da Cultura\\Cultura em Numeros\\Eixo 1\\Orcamento\\SICONFI_ESTADUAL")

# Configuração de caminhos e paleta
paleta_cores <- c(
  "Recurso Próprio (Estadual)"      = "#1b2631", 
  "Emendas Parlamentares (Cultura)" = "#27ae60", 
  "Lei Aldir Blanc 1 (LAB 1)"       = "#8e44ad",
  "Lei Paulo Gustavo (LPG)"          = "#e67e22", 
  "PNAB (Aldir Blanc 2)"            = "#f1c40f")

codigos_emendas <- c("3101", "3110", "3111", "3120", "3121", "3130", "3140", 
                     "3201", "3202", "3210", "3211", "3220", "3221")

# 1. DEFLATOR IPCA (BASE 2024) -------------------------------------------------
ipca_mensal <- rbcb::get_series(433, start_date = "2019-01-01", end_date = "2025-12-31")

fatores_ipca <- ipca_mensal %>%
  clean_names() %>%
  rename(var_mensal = x433) %>%
  mutate(ano = as.numeric(format(date, "%Y")),
         indice_encadeado = cumprod(1 + (var_mensal / 100))) %>%
  group_by(ano) %>%
  summarise(indice_medio_ano = mean(indice_encadeado), .groups = "drop") %>%
  mutate(indice_base = indice_medio_ano[ano == 2024],
         fator_deflacao = indice_base / indice_medio_ano) %>%
  select(exercicio = ano, fator_deflacao)

# 1. PROCESSAMENTO REFINADO ----------------------------------------------------
df_msc <- read_parquet("msc_orcamentaria_estados_2019_2025_final.parquet")

df_estados_processed <- df_msc %>%
  clean_names() %>%
  mutate(across(any_of(c("fonte_recursos", "complemento_fonte", "conta_contabil", "funcao")), as.character)) %>%
  filter(natureza_conta == "C") %>%
  mutate(id_funcao = as.numeric(str_extract(funcao, "\\d+")),
    fonte_string = str_remove_all(as.character(fonte_recursos), "\\."),
    conta_limpa = str_remove_all(conta_contabil, "\\."),
    complemento_limpo = str_pad(str_remove_all(complemento_fonte, "[^0-9]"), 4, pad = "0")) %>%
  filter(str_starts(conta_limpa, "62213")) %>%
  mutate(origem = case_when(
    str_detect(fonte_string, "^1719|^2719|^1720|^2720|^719|^720") ~ "PNAB (Aldir Blanc 2)",
    str_detect(fonte_string, "^1715|^2715|^1716|^2716|^715|^716") ~ "Lei Paulo Gustavo (LPG)",
    exercicio %in% c(2020, 2021) & 
      !str_detect(fonte_string, "^19|^29") & 
      str_detect(fonte_string, "^17|^27") ~ "Lei Aldir Blanc 1 (LAB 1)",
    complemento_limpo %in% codigos_emendas ~ "Emendas Parlamentares (Cultura)",
    TRUE ~ "Recurso Próprio (Estadual)")) %>%
  filter(id_funcao == 13 | origem == "Lei Aldir Blanc 1 (LAB 1)") %>%
  left_join(fatores_ipca, by = "exercicio") %>%
  mutate(valor = abs(valor), valor_real = valor * fator_deflacao) %>%
  mutate(origem = factor(origem, levels = names(paleta_cores)))

# 2. AGREGAÇÃO -----------------------------------------------------------------
df_estados_final <- df_estados_processed %>%
  group_by(exercicio, uf, origem) %>%
  summarise(valor_nominal_final = sum(valor, na.rm = TRUE),
            valor_real_final = sum(valor_real, na.rm = TRUE), .groups = "drop")

# 3. GRÁFICOS
# Gráfico 1: Nominal
df_resumo_nominal <- df_estados_final %>%
  group_by(exercicio, origem) %>%
  summarise(valor = sum(valor_nominal_final, na.rm = TRUE), .groups = "drop")

ggplot(df_resumo_nominal, aes(x = factor(exercicio), y = valor, fill = origem)) +
  geom_col(position = position_stack(), width = 0.7, alpha = 0.9) +
  geom_text(aes(label = ifelse(valor > 0.01e9, label_number(scale = 1e-9, suffix = "b", accuracy = 0.01, decimal.mark = ",")(valor), "")), 
            position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 2.8) +
  stat_summary(fun = sum, aes(label = label_number(scale = 1e-9, suffix = " bi", accuracy = 0.01, decimal.mark = ",")(after_stat(y)), group = exercicio), 
               geom = "text", vjust = -1.2, size = 3.8, fontface = "bold") +
  scale_y_continuous(labels = label_number(prefix = "R$ ", scale = 1e-9, suffix = " bi", accuracy = 0.01, decimal.mark = ","), 
                     expand = expansion(mult = c(0, 0.4))) +
  scale_fill_manual(values = paleta_cores, na.translate = FALSE) +
  labs(title = "Evolução do Investimento Cultural Estadual por Fonte (Valores Nominais)",
       subtitle = "R$ Bilhões", x = "Ano de Execução", y = "Valor Empenhado Nominal", fill = "Origem do Recurso") +
  theme_minimal() + theme(legend.position = "bottom", plot.title = element_text(face="bold", size = 14))

# Gráfico 2: Real
df_resumo_real <- df_estados_final %>%
  group_by(exercicio, origem) %>%
  summarise(valor = sum(valor_real_final, na.rm = TRUE), .groups = "drop")

ggplot(df_resumo_real, aes(x = factor(exercicio), y = valor, fill = origem)) +
  geom_col(position = position_stack(), width = 0.7, alpha = 0.9) +
  geom_text(aes(label = ifelse(valor > 0.05e9, label_number(scale = 1e-9, suffix = "b", accuracy = 0.1, decimal.mark = ",")(valor), "")), 
            position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 2.8) +
  stat_summary(fun = sum, aes(label = label_number(scale = 1e-9, suffix = " bi", accuracy = 0.1, decimal.mark = ",")(after_stat(y)), group = exercicio), 
               geom = "text", vjust = -1.2, size = 3.8, fontface = "bold") +
  scale_y_continuous(labels = label_number(prefix = "R$ ", scale = 1e-9, suffix = " bi", accuracy = 0.1, decimal.mark = ","), 
                     expand = expansion(mult = c(0, 0.4))) +
  scale_fill_manual(values = paleta_cores, na.translate = FALSE) +
  labs(title = "Evolução do Investimento Cultural Estadual por Fonte (Valores Reais)",
       subtitle = "Valores Empenhados Corrigidos pela Inflação (Preços Médios de 2024) | R$ Bilhões",
       x = "Ano de Execução", y = "Valor Empenhado Real (R$ Bilhões)", fill = "Origem do Recurso") +
  theme_minimal() + theme(legend.position = "bottom", plot.title = element_text(face="bold", size = 14))

# Gráfico 3: Percentual
df_resumo_percentual <- df_estados_final %>%
  group_by(exercicio, origem) %>%
  summarise(valor_ano_fonte = sum(valor_real_final, na.rm = TRUE), .groups = "drop_last") %>%
  mutate(participacao = valor_ano_fonte / sum(valor_ano_fonte)) %>%
  ungroup()

ggplot(df_resumo_percentual, aes(x = factor(exercicio), y = participacao, fill = origem)) +
  geom_col(position = position_stack(), width = 0.7, alpha = 0.9) +
  geom_text(aes(label = ifelse(participacao > 0.02, label_percent(accuracy = 0.1, decimal.mark = ",")(participacao), "")), 
            position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 2.8) +
  scale_y_continuous(labels = label_percent(decimal.mark = ","), expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = paleta_cores, na.translate = FALSE) +
  labs(title = "Composição Percentual do Investimento Cultural Estadual por Fonte",
       subtitle = "Participação Relativa das Fontes sobre o Investimento Real Empenhado (Base 2024)",
       x = "Ano de Execução", y = "Percentual da Execução (%)", fill = "Origem do Recurso") +
  theme_minimal() + theme(legend.position = "bottom", plot.title = element_text(face="bold", size = 14))

#Tabela por UF
df_tabela_uf_real <- df_estados_final %>%
  group_by(uf, origem, exercicio) %>%
  summarise(valor_total = sum(valor_real_final, na.rm = TRUE), .groups = "drop") %>%
  complete(uf, origem, exercicio = 2019:2025, fill = list(valor_total = 0)) %>%
  mutate(valor_mi = valor_total / 1e6) %>%
  select(uf, origem, exercicio, valor_mi) %>%
  pivot_wider(names_from = exercicio, values_from = valor_mi) %>%
  arrange(uf, origem)

colunas_anos <- as.character(2019:2025)

tabela_uf_final <- df_tabela_uf_real %>%
  gt(groupname_col = "uf") %>%
  tab_header(title = md("**Investimento Cultural Estadual Real por UF e Fonte de Recurso**"),
             subtitle = "Valores Empenhados em **R$ Milhões** | Corrigidos pela Inflação (Preços Médios de 2024)") %>%
  fmt_currency(columns = any_of(colunas_anos), currency = "BRL", dec_mark = ",", sep_mark = ".", decimals = 1, pattern = "{x} mi") %>%
  summary_rows(groups = TRUE, columns = any_of(colunas_anos), fns = list(SUBTOTAL = "sum"), formatter = fmt_currency, currency = "BRL", dec_mark = ",", sep_mark = ".", decimals = 1, pattern = "{x} mi") %>%
  cols_label(origem = "Fonte de Financiamento") %>%
  tab_options(row_group.font.weight = "bold", row_group.background.color = "#f2f4f4", summary_row.background.color = "#eaeded", table.font.size = px(11), heading.title.font.size = px(14), heading.subtitle.font.size = px(11), stub.border.color = "#d5dbdb", table.border.top.color = "#1b2631")

tabela_uf_final

# 8. RECEITA CORRENTE LÍQUIDA E PAINEL DE ESFORÇO FISCAL -----------------------
path_base_estados <- "E:/Pedro Buril/Ministerio da Cultura/Cultura em Numeros/Eixo 1/Orcamento/SICONFI_ESTADUAL"

arquivos_rcl_est <- list.files(path_base_estados, pattern = "^RCL_.*_Estados\\.csv$", full.names = TRUE)

df_rcl_estados <- arquivos_rcl_est %>%
  map_dfr(function(arq) {
    ano_arq <- as.numeric(str_extract(basename(arq), "\\d{4}"))
    if (ano_arq >= 2019 & ano_arq <= 2024) {
      linhas_topo <- read_lines(arq, n_max = 30, locale = locale(encoding = "Latin1"))
      linha_header <- which(str_detect(linhas_topo, "Cod\\.IBGE|Institui"))
      if (length(linha_header) == 0) return(NULL)
      read_delim(arq, delim = ";", skip = linha_header - 1, 
                 locale = locale(encoding = "Latin1", decimal_mark = ","), 
                 show_col_types = FALSE) %>%
        clean_names() %>%
        filter(str_detect(tolower(conta), "receita corrente l.quida \\(iii\\)"),
               str_detect(tolower(coluna), "total \\(.ltimos 12 meses\\)")) %>%
        mutate(exercicio = ano_arq,
               valor_rcl = as.numeric(valor),
               uf_chave = str_trim(uf)) %>%
        select(exercicio, uf_chave, valor_rcl)
    }
  })

df_cultura_proprio_est <- df_estados_final %>%
  filter(origem == "Recurso Próprio (Estadual)") %>%
  group_by(exercicio, uf_chave = uf) %>%
  summarise(total_cultura_proprio = sum(valor_nominal_final, na.rm = TRUE), .groups = "drop")

df_meta_pnc_est <- df_cultura_proprio_est %>%
  inner_join(df_rcl_estados, by = c("exercicio", "uf_chave")) %>%
  rename(uf = uf_chave) %>%
  mutate(percentual_rcl = (total_cultura_proprio / valor_rcl) * 100,
         atingiu_meta = ifelse(percentual_rcl >= 1.5, 1, 0),
         regiao = case_when(
           uf %in% c("AM", "PA", "AC", "RO", "RR", "AP", "TO") ~ "Norte",
           uf %in% c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA") ~ "Nordeste",
           uf %in% c("MT", "MS", "GO", "DF") ~ "Centro-Oeste",
           uf %in% c("SP", "RJ", "MG", "ES") ~ "Sudeste",
           uf %in% c("PR", "SC", "RS") ~ "Sul"))

df_painel_estados <- df_meta_pnc_est %>%
  filter(!is.na(regiao)) %>%
  mutate(cor_texto = ifelse(percentual_rcl >= 1.5, "white", "#2c3e50"),
         uf = factor(uf, levels = rev(sort(unique(uf)))))

ggplot(df_painel_estados, aes(x = factor(exercicio), y = uf, fill = percentual_rcl)) +
  geom_tile(color = "white", size = 0.4) +
  geom_text(aes(label = label_number(suffix = "%", accuracy = 0.1, decimal.mark = ",")(percentual_rcl),
                color = cor_texto), fontface = "bold", size = 2.8) +
  scale_color_identity() +
  scale_fill_gradient2(
    low = "#fadbd8", 
    mid = "#f39c12", 
    high = "#1b2631", 
    midpoint = 1.5,
    labels = label_number(suffix = "%", decimal.mark = ","),
    expand = c(0, 0)) +
  facet_grid(regiao ~ ., scales = "free_y", space = "free_y") +
  labs(title = "Painel de Esforço Fiscal da Cultura nas Esferas Estaduais (2019-2024)",
       subtitle = "Investimento de Recursos Próprios na Função 13 como Percentual (%) da RCL | Meta PNC: 1,5%",
       x = "Ano de Execução", 
       y = "Unidade da Federação (UF)", 
       fill = "Percentual da RCL",
       caption = "Fonte: MSC/SICONFI. Análise: Cultura em Números. Ponto de virada da Meta estabelecido em 1,5%.") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13, color = "#1b2631"),
        plot.subtitle = element_text(size = 10, color = "#566573"),
        axis.text.x = element_text(face = "bold", size = 9),
        axis.text.y = element_text(face = "bold", size = 9, color = "#1b2631"),
        strip.text.y = element_text(face = "bold", size = 9, angle = 0),
        strip.background = element_rect(fill = "#f2f4f4", color = "white"),
        panel.grid = element_blank(),
        legend.position = "right",
        legend.title = element_text(face = "bold", size = 9))


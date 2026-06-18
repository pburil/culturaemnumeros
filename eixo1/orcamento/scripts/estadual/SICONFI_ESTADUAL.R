#-------------------------------------------------------------------------------
# Bibliotecas
#-------------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(scales)
library(janitor)

#-------------------------------------------------------------------------------
# Configurações de Diretório
#-------------------------------------------------------------------------------
setwd("D:\\aeae\\Ministerio da Cultura\\Cultura em Numeros\\Eixo 1\\Orcamento\\SICONFI_ESTADUAL")

#-------------------------------------------------------------------------------
# 1. BASE DE POPULAÇÃO (IBGE)
#-------------------------------------------------------------------------------
dados_populacao <- read_excel("MUNIC_FINAL.xlsx") %>%
  clean_names() %>%
  mutate(sigla_uf = as.character(sigla_uf),
         populacao = as.numeric(populacao))

pop_uf <- dados_populacao %>%
  group_by(sigla_uf) %>%
  summarise(populacao = sum(populacao, na.rm = TRUE), .groups = "drop")

#-------------------------------------------------------------------------------
# 2. PROCESSAMENTO - GASTOS EM CULTURA (SICONFI - EMPENHADO)
#-------------------------------------------------------------------------------
anos <- 2018:2025 

ler_siconfi_estadual <- function(ano){
  arquivo <- paste0(ano, " - Estadual.csv")
  
  if(!file.exists(arquivo)) return(NULL)
  
  message(paste("Processando Empenho Estadual:", ano))
  
  read_delim(arquivo,
             delim = ";",
             locale = locale(encoding = "Latin1"),
             show_col_types = FALSE) %>%
    clean_names() %>%
    filter(conta %in% c("Cultura"), 
           coluna == "DESPESAS EMPENHADAS ATÉ O BIMESTRE (b)") %>% 
    mutate(ano = ano,
           valor = gsub("\\.", "", valor),
           valor = as.numeric(gsub(",", ".", valor))) %>%
    filter(!is.na(valor), valor > 0)
}

dados_cultura_est <- map_dfr(anos, ler_siconfi_estadual)

# Agregação por UF e Ano
dados_uf_tempo <- dados_cultura_est %>%
  group_by(uf, ano) %>%
  summarise(gasto_total = sum(valor, na.rm = TRUE), .groups = "drop") %>%
  rename(sigla_uf = uf) %>%
  left_join(pop_uf, by = "sigla_uf") %>%
  mutate(gasto_pc = gasto_total / populacao)

#-------------------------------------------------------------------------------
# 3. PROCESSAMENTO - RECEITA CORRENTE LÍQUIDA (RCL)
#-------------------------------------------------------------------------------
ler_rcl_estadual <- function(ano){
  arquivo <- paste0("RCL_", ano, "_Estados.csv")
  
  if(!file.exists(arquivo)) return(NULL)
  
  read_delim(arquivo,
             delim = ";",
             locale = locale(encoding = "Latin1"),
             show_col_types = FALSE) %>%
    clean_names() %>%
    filter(conta == "RECEITA CORRENTE LÍQUIDA (III) = (I - II)",
           coluna == "TOTAL (ÚLTIMOS 12 MESES)") %>%
    mutate(ano = ano,
           valor = gsub("\\.", "", valor),
           valor = as.numeric(gsub(",", ".", valor)),
           sigla_uf = as.character(uf)) %>%
    select(sigla_uf, ano, rcl = valor)
}

rcl_estados <- map_dfr(anos, ler_rcl_estadual)

#-------------------------------------------------------------------------------
# 4. UNIÃO DE DADOS E CÁLCULO DO % RCL
#-------------------------------------------------------------------------------
dados_completos <- dados_uf_tempo %>%
  left_join(rcl_estados, by = c("sigla_uf", "ano")) %>%
  mutate(perc_rcl = (gasto_total / rcl) * 100)

#-------------------------------------------------------------------------------
# 5. VISUALIZAÇÕES
#-------------------------------------------------------------------------------

# A. Evolução do % da RCL por Estado (Empenhado)
ggplot(dados_completos, aes(x = ano, y = perc_rcl)) +
  geom_line(color = "#2980b9", size = 0.8) +
  geom_point(color = "#2980b9", size = 1.2) +
  facet_wrap(~sigla_uf, scales = "free_y") +
  scale_x_continuous(breaks = seq(2018, 2025, 2)) +
  scale_y_continuous(labels = label_number(suffix = "%", decimal.mark = ",")) +
  labs(title = "Empenho Cultural como % da Receita Corrente Líquida (RCL)",
       subtitle = "Séries estaduais: Valores Empenhados (Fase b) | 2018-2025",
       x = NULL, y = "% da RCL",
       caption = "Fonte: SICONFI. Elaboração própria.") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_text(size = 8))

# B. Barra de Empenho Total Agregado (Estados)
gasto_total_estados <- dados_uf_tempo %>%
  group_by(ano) %>%
  summarise(total_anual = sum(gasto_total, na.rm = TRUE))

ggplot(gasto_total_estados, aes(x = factor(ano), y = total_anual)) +
  geom_col(fill = "#2980b9", width = 0.7, alpha = 0.9) +
  geom_text(aes(label = label_number(scale = 1e-9, suffix = " bi", 
                                     decimal.mark = ",", accuracy = 0.01)(total_anual)),
            vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_y_continuous(labels = label_number(scale = 1e-9, suffix = " bi", 
                                           decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Evolução do Empenho Total em Cultura - Governos Estaduais",
       subtitle = "Somatório de todas as UFs (Valores Empenhados - Fase b)",
       x = NULL, y = "R$ (Bilhões)",
       caption = "Fonte: SICONFI. Elaboração própria.") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", size = 14))

#Gasto cultural escondido = Turismo (função 23) - festas populares e eventos de grande porte como turismo e não como cultura
#educação (função 12) - oficinas de artes e projetos escolares entram na função educação
#Eventuais gastos culturais registrados em outras funções (como Turismo ou Educação) não estão contemplados nesta série histórica por uma questão de padronização metodológica.

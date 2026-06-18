#===============================================================================
# SCRIPT 4: CONSOLIDAÇÃO DO INVESTIMENTO CULTURAL (VALORES NOMINAIS)
#===============================================================================
library(tidyverse)
library(scales)

# 0. CONFIGURAÇÕES -------------------------------------------------------------
tema_macro <- theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 10, face = "bold"))

paleta_entes <- c(
  "União (Gasto Direto)"         = "#2980b9",
  "Estados (Recurso Próprio)"    = "#27ae60",
  "Municípios (Recurso Próprio)" = "#8e44ad")

# 1. EXTRAÇÃO E AGREGAÇÃO DOS DADOS (CENÁRIO NOMINAL) --------------------------

# 1.1 Federal (Apenas Gasto Direto - MINC, FSA, LAB1, LPG, PNAB, CIDADANIA E TURISMO)
df_fed_macro_nom <- df_anual_ajustado %>%
  filter(!fonte %in% c("Lei Rouanet", "Incentivo (ANCINE)")) %>%
  group_by(ano = as.numeric(ano)) %>%
  summarise(valor_investimento = sum(valor_nominal, na.rm = TRUE), .groups = "drop") %>%
  mutate(ente_federativo = "União (Gasto Direto)")

# 1.2 Estadual (Recurso Próprio - Nominal)
df_est_macro_nom <- df_estados_final %>%
  filter(origem == "Recurso Próprio (Estadual)") %>%
  group_by(ano = as.numeric(exercicio)) %>%
  summarise(valor_investimento = sum(valor_nominal_final, na.rm = TRUE), .groups = "drop") %>%
  mutate(ente_federativo = "Estados (Recurso Próprio)")

# 1.3 Municipal (Recurso Próprio - Nominal)
df_mun_macro_nom <- df_municipios_final %>%
  filter(origem == "Recurso Próprio (Municipal)") %>%
  group_by(ano = as.numeric(exercicio)) %>%
  summarise(valor_investimento = sum(valor_nominal_final, na.rm = TRUE), .groups = "drop") %>%
  mutate(ente_federativo = "Municípios (Recurso Próprio)")

# 2. UNIÃO DAS BASES -----------------------------------------------------------
df_macro_nominal <- bind_rows(df_fed_macro_nom, df_est_macro_nom, df_mun_macro_nom) %>%
  filter(ano >= 2019 & ano <= 2025) %>% 
  mutate(ente_federativo = factor(ente_federativo, levels = c("União (Gasto Direto)", "Estados (Recurso Próprio)", "Municípios (Recurso Próprio)")))

# 3. GRÁFICO: VOLUME NOMINAL EMPILHADO -----------------------------------------
ggplot(df_macro_nominal, aes(x = factor(ano), y = valor_investimento, fill = ente_federativo)) +
  geom_col(position = position_stack(), width = 0.7, alpha = 0.95) +
  geom_text(aes(label = ifelse(valor_investimento > 0.5e9, 
                               label_number(scale = 1e-9, suffix = "b", accuracy = 0.1, decimal.mark = ",")(valor_investimento), "")), 
            position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 3) +
  stat_summary(fun = sum, aes(label = label_number(scale = 1e-9, suffix = " bi", accuracy = 0.1, decimal.mark = ",")(after_stat(y)), 
                              group = ano), 
               geom = "text", vjust = -0.8, size = 4, fontface = "bold", color = "#1b2631") +
  scale_y_continuous(labels = label_number(prefix = "R$ ", scale = 1e-9, suffix = " bi", decimal.mark = ","), 
                     expand = expansion(mult = c(0, 0.2))) +
  scale_fill_manual(values = paleta_entes) +
  labs(title = "Volume Nacional Consolidado (Valores Nominais)",
       subtitle = "Execução orçamentária primária (Valores correntes - sem correção inflacionária)",
       x = "Ano de Execução", y = "Valor Nominal Investido (R$ Bilhões)", fill = "Esfera de Financiamento",
       caption = "Fonte: SIOP e MSC/SICONFI. Nota: Valores correntes da época da execução.") +
  tema_macro

# 4. GRÁFICO: COMPOSIÇÃO PERCENTUAL NOMINAL ------------------------------------
df_macro_pct_nom <- df_macro_nominal %>%
  group_by(ano) %>%
  mutate(pct_investimento = valor_investimento / sum(valor_investimento, na.rm = TRUE)) %>%
  ungroup()

ggplot(df_macro_pct_nom, aes(x = factor(ano), y = valor_investimento, fill = ente_federativo)) +
  geom_col(position = "fill", width = 0.7, alpha = 0.95) +
  geom_text(aes(label = percent(pct_investimento, accuracy = 0.1, decimal.mark = ",")), 
            position = position_fill(vjust = 0.5), color = "white", fontface = "bold", size = 3.2) +
  scale_y_continuous(labels = percent_format(decimal.mark = ","), expand = c(0, 0)) +
  scale_fill_manual(values = paleta_entes) +
  labs(title = "Composição Percentual do Investimento Direto (Valores Nominais)",
       subtitle = "Peso relativo de cada esfera na execução orçamentária primária (Valores correntes)",
       x = "Ano de Execução", y = "Proporção do Investimento (%)", fill = "Esfera de Financiamento",
       caption = "Fonte: SIOP e MSC/SICONFI. Análise: Cultura em Números.") +
  tema_macro +
  theme(panel.grid.major.x = element_blank())

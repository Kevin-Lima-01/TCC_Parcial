library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(writexl)
library(ggplot2)


####
#### IPCA ####
####
# Base final
T <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Inflacao/T.xlsx")
NT <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Inflacao/NT.xlsx")
IPCA <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Inflacao/IPCA.xlsx")
infla <- NT %>%
  full_join(T, by = c("municipio", "data")) %>%
  full_join(IPCA, by = c("municipio", "data"))
infla <- infla %>% rename(VIPCA = ipca)

infla <- infla %>%
  filter(substr(data, 1, 4) >= "1996")

# Indice
infla <- infla %>%
  group_by(municipio) %>%
  mutate(
    indice_VNT = 100 * cumprod(1 + ifelse(is.na(VNT), 0, VNT)/100),
    indice_VT = 100 * cumprod(1 + ifelse(is.na(VT), 0, VT)/100),
    indice_VIPCA = 100 * cumprod(1 + ifelse(is.na(VIPCA), 0, VIPCA)/100),
    indice_VNT = ifelse(is.na(VNT) & indice_VNT == 100, NA, indice_VNT),
    indice_VT = ifelse(is.na(VT) & indice_VT == 100, NA, indice_VT),
    indice_VIPCA = ifelse(is.na(VIPCA) & indice_VIPCA == 100, NA, indice_VIPCA)
  ) %>%
  ungroup()

# Log Indice
infla <- infla %>%
  mutate(
    p_VNT = log(indice_VNT),
    p_VT = log(indice_VT),
    p_VIPCA = log(indice_VIPCA)
  )

# Anualizar
infla <- infla %>%
  arrange(municipio, data) %>%
  group_by(municipio) %>%
  mutate(
    VNT_12m = (p_VNT - lag(p_VNT, 12)) * 100,
    VT_12m = (p_VT - lag(p_VT, 12)) * 100,
    VIPCA_12m = (p_VIPCA - lag(p_VIPCA, 12)) * 100
  ) %>%
  ungroup()


# Preço Relativo de Não-Transacionáveis
# phat_N = log(P_NT) - log(P_total) — desvio em relação à média do município
# Isso captura a variação *relativa* do nível de preços NT vs. o nível geral
infla <- infla %>%
  group_by(municipio) %>%
  mutate(
    phat_N = (p_VNT - p_VIPCA) * 100
  ) %>%
  ungroup()

# Gerar a base
infla_m <- infla %>%
  select(municipio, data, VNT_12m, VT_12m, VIPCA_12m, phat_N)
write_xlsx(infla_m, "C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Inflacao/infla_m.xlsx")


####
#### Tri ####
####
# Gerer para a base trimestral
infla <- infla %>%
  mutate(
    data_date = ym(data),
    ano = year(data_date),
    trimestre = quarter(data_date)
  )

infla_t <- infla %>%
  mutate(
    data_date = ym(data),
    ano       = year(data_date),
    trimestre = quarter(data_date)
  ) %>%
  group_by(municipio, ano, trimestre) %>%
  summarise(
    VNT_12m_t  = mean(VNT_12m,  na.rm = TRUE),
    VT_12m_t   = mean(VT_12m,   na.rm = TRUE),
    VIPCA_12m_t = mean(VIPCA_12m, na.rm = TRUE),
    phat_N_t   = mean(phat_N,   na.rm = TRUE),   
    .groups = "drop"
  ) %>%
  mutate(
    data_t      = paste0(ano, "-Q", trimestre),
    across(where(is.numeric), ~ ifelse(is.nan(.x), NA_real_, .x))
  ) %>%
  select(municipio, data_t, VNT_12m_t, VT_12m_t, VIPCA_12m_t, phat_N_t)


write_xlsx(infla_t, "C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Inflacao/infla_t.xlsx")

####
#### Plot ####
####
# Base para SP Mensal
sp_mensal <- infla %>%
  filter(municipio == "São Paulo (SP)") %>%
  mutate(data_date = as.Date(paste0(data, "-01")))

# Gráfico mensal 
ggplot(sp_mensal, aes(x = data_date)) +
  geom_line(aes(y = VNT_12m, color = "Non-Tradable"), size = 0.8) +
  geom_line(aes(y = VT_12m, color = "Tradable"), size = 0.8) +
  geom_line(aes(y = VIPCA_12m, color = "IPCA Total"), size = 0.8) +
  labs(
    title = "Inflação - São Paulo (SP)",
    subtitle = "Variação acumulada 12 meses (dados mensais)",
    x = "Data",
    y = "Inflação (%)",
    color = "Tipo",
    caption = "Fonte: IPCA/IBGE"
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Base para SP Trimestral
sp_trimestral <- infla %>%
  filter(municipio == "São Paulo (SP)") %>%
  mutate(
    data_date = ym(data),
    ano = year(data_date),
    trimestre = quarter(data_date),
    mes_ref = case_when(
      trimestre == 1 ~ 2,
      trimestre == 2 ~ 5,
      trimestre == 3 ~ 8,
      trimestre == 4 ~ 11
    ),
    data_trimestre = as.Date(paste(ano, mes_ref, "15", sep = "-"))
  ) %>%
  group_by(municipio, ano, trimestre, data_trimestre) %>%
  summarise(
    VNT_12m_t = mean(VNT_12m, na.rm = TRUE),
    VT_12m_t = mean(VT_12m, na.rm = TRUE),
    VIPCA_12m_t = mean(VIPCA_12m, na.rm = TRUE),
    .groups = "drop"
  )

# Gráfico trimestral com as 3 séries
ggplot(sp_trimestral, aes(x = data_trimestre)) +
  geom_line(aes(y = VNT_12m_t, color = "Non-Tradable"), size = 0.8) +
  geom_line(aes(y = VT_12m_t, color = "Tradable"), size = 0.8) +
  geom_line(aes(y = VIPCA_12m_t, color = "IPCA Total"), size = 0.8) +
  labs(
    title = "Inflação - São Paulo (SP)",
    subtitle = "Variação acumulada 12 meses (dados trimestrais)",
    x = "Data",
    y = "Inflação (%)",
    color = "Tipo",
    caption = "Fonte: IPCA/IBGE"
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

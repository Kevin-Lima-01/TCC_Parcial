library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(ggplot2)
library(writexl)
library(PNADCperiods)
library(data.table)
library(PNADCperiods)
library(PNADcIBGE)

####
#### Desemprego Mensal ####
####
# Bases
PME_m <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Desemprego/PME_14/PME_sub14_m.xlsx")
PNAD_m <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Desemprego/PNAD_TOT/PNAD_m.xlsx")
INFO_m <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Desemprego/PNAD_INFO/PNADI_m.xlsx")
PNAD_m <- PNAD_m %>% mutate(fonte = "PNAD")
PME_m <- PME_m %>% mutate(fonte = "PME")
INFO_m <- INFO_m %>% mutate(fonte = "INFORMALIDADE")

# Junção
base_combinada <- PNAD_m %>%
  bind_rows(
    PME_m %>%
      anti_join(
        PNAD_m %>% select(municipio, data),
        by = c("municipio", "data")
      )
  ) %>%
  arrange(municipio, data)

todos_municipios <- unique(base_combinada$municipio)
datas_ordenadas <- sort(unique(base_combinada$data))
data_min <- datas_ordenadas[1]
data_max <- datas_ordenadas[length(datas_ordenadas)]
datas_completas <- seq(
  as.Date(paste0(data_min, "-01")),
  as.Date(paste0(data_max, "-01")),
  by = "month")
datas_completas <- format(datas_completas, "%Y-%m")
grade_completa <- expand.grid(
  municipio = todos_municipios,
  data = datas_completas,
  stringsAsFactors = FALSE)

DES_M <- grade_completa %>%
  left_join(base_combinada, by = c("municipio", "data"))

# Informalidade
INFO_m <- INFO_m %>%
  mutate(data = as.character(data))
DES_M <- DES_M %>%
  left_join(
    INFO_m %>% select(municipio, data, taxa_informalidade),
    by = c("municipio", "data")
  )
DES_M <- DES_M %>%
  mutate(
    desemprego_informalidade = case_when(
      fonte == "PNAD" & !is.na(taxa_desemprego) & !is.na(taxa_informalidade) ~ 
        taxa_desemprego + taxa_informalidade,
      fonte == "PME" & !is.na(taxa_desemprego) & !is.na(taxa_informalidade) ~ 
        taxa_desemprego + taxa_informalidade,
      TRUE ~ NA_real_))
    
# Grafico
sp_data <- DES_M %>%
  filter(municipio == "São Paulo (SP)") %>%
  mutate(
    data_date = as.Date(paste0(data, "-01")),
    fonte = ifelse(is.na(fonte), "Sem dado", fonte)
  )

ggplot(sp_data, aes(x = data_date, y = taxa_desemprego)) +
  geom_line(color = "steelblue", size = 0.8) +
  geom_point(aes(color = fonte), size = 1.5, alpha = 0.7) +
  scale_color_manual(values = c("PNAD" = "darkgreen", "PME" = "orange", "Sem dado" = "red")) +
  annotate("rect",
           xmin = as.Date("2020-02-01"),
           xmax = as.Date("2022-03-01"),
           ymin = -Inf, ymax = Inf,
           fill = "gray70", alpha = 0.3) +
  annotate("text",
           x = as.Date("2020-01-01"),
           y = max(sp_data$taxa_desemprego, na.rm = TRUE) * 0.9,
           label = "",
           size = 3.5, color = "gray40") +
  labs(
    title = "Taxa de Desemprego - São Paulo (SP)",
    subtitle = "Dados mensais: PME (até 2012) e PNAD (a partir de 2012)",
    x = "Data",
    y = "Taxa de Desemprego (%)",
    color = "Fonte",
    caption = "Período sombreado: pandemia (2020 Q2 - 2022 Q1) mantido como NA"
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

sp_data_2015 <- DES_M %>%
  filter(municipio == "São Paulo (SP)") %>%
  mutate(
    data_date = as.Date(paste0(data, "-01")),
    fonte = ifelse(is.na(fonte), "Sem dado", fonte)
  ) %>%
  filter(data_date >= as.Date("2015-01-01"))


ggplot(sp_data_2015, aes(x = data_date, y = taxa_informalidade)) +
  geom_line(color = "purple", size = 0.8) +
  geom_point(aes(color = fonte), size = 1.5, alpha = 0.7) +
  scale_color_manual(values = c("PNAD" = "darkgreen", "PME" = "orange", "Sem dado" = "red")) +
  annotate("rect",
           xmin = as.Date("2020-02-01"),
           xmax = as.Date("2022-03-01"),
           ymin = -Inf, ymax = Inf,
           fill = "gray70", alpha = 0.3) +
  annotate("text",
           x = as.Date("2020-01-01"),
           y = max(sp_data$taxa_informalidade, na.rm = TRUE) * 0.9,
           label = "",
           size = 3.5, color = "gray40") +
  labs(
    title = "Taxa de Informalidade - São Paulo (SP)",
    subtitle = "Trabalhadores informais como proporção da força de trabalho",
    x = "Data",
    y = "Taxa de Informalidade (%)",
    color = "Fonte",
    caption = "Dados disponíveis a partir de 2015 | Período sombreado: pandemia (sem dados)"
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, max(sp_data$taxa_informalidade, na.rm = TRUE) * 1.1)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggplot(sp_data_2015, aes(x = data_date, y = desemprego_informalidade)) +
  geom_line(color = "darkred", size = 0.8) +
  geom_point(aes(color = fonte), size = 1.5, alpha = 0.7) +
  scale_color_manual(values = c("PNAD" = "darkgreen", "PME" = "orange", "Sem dado" = "red")) +
  annotate("rect",
           xmin = as.Date("2020-02-01"),
           xmax = as.Date("2022-03-01"),
           ymin = -Inf, ymax = Inf,
           fill = "gray70", alpha = 0.3) +
  annotate("text",
           x = as.Date("2020-01-01"),
           y = max(sp_data$desemprego_informalidade, na.rm = TRUE) * 0.9,
           label = "",
           size = 3.5, color = "gray40") +
  labs(
    title = "Subutilização da Mão de Obra - São Paulo (SP)",
    subtitle = "Desemprego + Informalidade",
    x = "Data",
    y = "Taxa (%)",
    color = "Fonte",
    caption = "Soma da taxa de desemprego com a taxa de informalidade | Período sombreado: pandemia (sem dados)"
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, max(sp_data$desemprego_informalidade, na.rm = TRUE) * 1.1)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

####
#### Desemprego Trimestral ####
####
# Bases
PME_t <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Desemprego/PME_14/PME_sub14_t.xlsx")
PNAD_t <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Desemprego/PNAD_TOT/PNAD_t.xlsx")
INFO_t <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Desemprego/PNAD_INFO/PNADI_t.xlsx")

PME_t <- PME_t %>%
  mutate(
    data = gsub("T", "0", periodo_trimestre),  
    data = as.character(data)
  ) %>%
  select(-periodo_trimestre)  

PNAD_t <- PNAD_t %>% mutate(fonte = "PNAD")
PME_t <- PME_t %>% mutate(fonte = "PME")
INFO_t <- INFO_t %>% mutate(fonte = "INFORMALIDADE") 

# Junção
base_combinada <- PNAD_t %>%
  bind_rows(
    PME_t %>%
      anti_join(
        PNAD_t %>% select(municipio, data),
        by = c("municipio", "data")
      )
  ) %>%
  arrange(municipio, data)

todos_municipios <- unique(base_combinada$municipio)
datas_existentes <- sort(unique(base_combinada$data))
grade_completa <- expand.grid(
  municipio = todos_municipios,
  data = datas_existentes,
  stringsAsFactors = FALSE
)

DES_T <- grade_completa %>%
  left_join(base_combinada, by = c("municipio", "data"))

# Informalidade
DES_T <- DES_T %>%
  left_join(
    INFO_t %>% select(municipio, data, taxa_informalidade),
    by = c("municipio", "data")
  )

DES_T <- DES_T %>%
  mutate(
    desemprego_informalidade = case_when(
      fonte == "PNAD" & !is.na(taxa_desemprego) & !is.na(taxa_informalidade) ~ 
        taxa_desemprego + taxa_informalidade,
      fonte == "PME" & !is.na(taxa_desemprego) & !is.na(taxa_informalidade) ~ 
        taxa_desemprego + taxa_informalidade,
      TRUE ~ NA_real_
    ))

# Graficos
sp_data <- DES_T %>%
  filter(municipio == "São Paulo (SP)") %>%
  mutate(
    ano = as.numeric(substr(data, 1, 4)),
    trimestre_num = as.numeric(substr(data, 6, 7)),
    mes_ref = case_when(
      trimestre_num == 1 ~ 2,
      trimestre_num == 2 ~ 5,
      trimestre_num == 3 ~ 8,
      trimestre_num == 4 ~ 11
    ),
    data_date = as.Date(paste(ano, mes_ref, "15", sep = "-")),
    fonte = ifelse(is.na(fonte), "Sem dado", fonte)
  ) %>%
  arrange(data_date)

ggplot(sp_data, aes(x = data_date, y = taxa_desemprego)) +
  geom_line(color = "steelblue", size = 0.8) +
  geom_point(aes(color = fonte), size = 2, alpha = 0.7) +
  scale_color_manual(values = c("PNAD" = "darkgreen", "PME" = "orange", "Sem dado" = "red")) +
  annotate("rect",
           xmin = as.Date("2020-04-01"),
           xmax = as.Date("2022-04-01"),
           ymin = -Inf, ymax = Inf,
           fill = "gray70", alpha = 0.3) +
  annotate("text",
           x = as.Date("2021-01-01"),
           y = max(sp_data$taxa_desemprego, na.rm = TRUE) * 0.9,
           label = "",
           size = 3.5, color = "gray40") +
  labs(
    title = "Taxa de Desemprego - São Paulo (SP)",
    subtitle = "Dados trimestrais: PME (até 2012) e PNAD (a partir de 2012)",
    x = "Data",
    y = "Taxa de Desemprego (%)",
    color = "Fonte",
    caption = "Período sombreado: pandemia (2020 Q2 - 2022 Q1) mantido como NA"
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

sp_data_2015 <- sp_data %>%
  filter(ano >= 2015)

ggplot(sp_data_2015, aes(x = data_date, y = taxa_informalidade)) +
  geom_line(color = "purple", size = 0.8) +
  geom_point(aes(color = fonte), size = 2, alpha = 0.7) +
  scale_color_manual(values = c("PNAD" = "darkgreen", "PME" = "orange", "Sem dado" = "red")) +
  annotate("rect",
           xmin = as.Date("2020-04-01"),
           xmax = as.Date("2022-04-01"),
           ymin = -Inf, ymax = Inf,
           fill = "gray70", alpha = 0.3) +
  annotate("text",
           x = as.Date("2021-01-01"),
           y = max(sp_data_2015$taxa_informalidade, na.rm = TRUE) * 0.9,
           label = "",
           size = 3.5, color = "gray40") +
  labs(
    title = "Taxa de Informalidade - São Paulo (SP)",
    subtitle = "Dados trimestrais (2015-2024)",
    x = "Data",
    y = "Taxa de Informalidade (%)",
    color = "Fonte",
    caption = "Período sombreado: pandemia (2020 Q2 - 2022 Q1) mantido como NA"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, max(sp_data_2015$taxa_informalidade, na.rm = TRUE) * 1.1)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggplot(sp_data_2015, aes(x = data_date, y = desemprego_informalidade)) +
  geom_line(color = "darkred", size = 0.8) +
  geom_point(aes(color = fonte), size = 2, alpha = 0.7) +
  scale_color_manual(values = c("PNAD" = "darkgreen", "PME" = "orange", "Sem dado" = "red")) +
  annotate("rect",
           xmin = as.Date("2020-04-01"),
           xmax = as.Date("2022-04-01"),
           ymin = -Inf, ymax = Inf,
           fill = "gray70", alpha = 0.3) +
  annotate("text",
           x = as.Date("2021-01-01"),
           y = max(sp_data_2015$desemprego_informalidade, na.rm = TRUE) * 0.9,
           label = "",
           size = 3.5, color = "gray40") +
  labs(
    title = "Subutilização da Mão de Obra - São Paulo (SP)",
    subtitle = "Desemprego + Informalidade (dados trimestrais 2015-2024)",
    x = "Data",
    y = "Taxa (%)",
    color = "Fonte",
    caption = "Soma da taxa de desemprego com a taxa de informalidade | Período sombreado: pandemia (sem dados)"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, max(sp_data_2015$desemprego_informalidade, na.rm = TRUE) * 1.1)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )
###
### Exportar dados ###
###
# Exportar
DES_M <- DES_M %>%
  mutate(
    taxa_desemprego = as.numeric(as.character(taxa_desemprego)),
    taxa_informalidade = as.numeric(as.character(taxa_informalidade)),
    desemprego_informalidade = as.numeric(as.character(desemprego_informalidade))
  )

DES_T <- DES_T %>%
  mutate(
    taxa_desemprego = as.numeric(as.character(taxa_desemprego)),
    taxa_informalidade = as.numeric(as.character(taxa_informalidade)),
    desemprego_informalidade = as.numeric(as.character(desemprego_informalidade))
  )

output_dir <- "C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Desemprego/"

write_xlsx(DES_M, paste0(output_dir, "DES_M.xlsx"))
write_xlsx(DES_T, paste0(output_dir, "DES_T.xlsx"))

write.csv(DES_M, paste0(output_dir, "DES_M.csv"), row.names = FALSE, na = "")
write.csv(DES_T, paste0(output_dir, "DES_T.csv"), row.names = FALSE, na = "")


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
#### PNAD ####
####
# Taxas
PNADB <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Desemprego/PNAD_TOT/PNAD_TOT.xlsx")
PNADB <- PNADB %>% slice(-c(1, 2, 4, n()))
dates <- as.character(PNADB[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
PNADB[1, ] <- t(dates)
colnames(PNADB)[1] <- "municipio"
PNADB[1, 1] <- "municipio"
colnames(PNADB) <- as.character(PNADB[1, ])
PNADB <- PNADB[-1, ]
PNADB[, -1] <- lapply(PNADB[, -1], as.numeric)

PNAD <- PNADB %>%
  pivot_longer(cols = -municipio,
               names_to = "data",
               values_to = "taxa_desemprego") %>%
  mutate(
    taxa_desemprego = as.numeric(taxa_desemprego),
    data = ym(data)
  ) %>%
  mutate(data = format(data, "%Y-%m")) %>%
  arrange(municipio, data)

municipios_selecionados <- c(
  "Aracaju (SE)", "Belo Horizonte (MG)", "Belém (PA)", "Brasília (DF)",
  "Campo Grande (MS)", "Curitiba (PR)", "Fortaleza (CE)", "Goiânia (GO)",
  "Grande Vitória (ES)", "Porto Alegre (RS)", "Recife (PE)", "Rio Branco (AC)",
  "Rio de Janeiro (RJ)", "Salvador (BA)", "São Luís (MA)", "São Paulo (SP)")

PNAD_raw <- PNAD %>%
  filter(municipio %in% municipios_selecionados)


####
#### Mensalizar ####
####
# Indicar periodo (Problema Pandemia)
PNAD_com_periodo <- PNAD_raw %>%
  mutate(
    data_date = ym(data),
    ano = year(data_date),
    periodo = case_when(
      data_date < ym("2020-04") ~ 1,           
      data_date > ym("2022-03") ~ 2,           
      TRUE ~ NA_real_))
mensalizar_com_pausa <- function(df, metodo = "spline") {
  resultados <- list()
  for(m in unique(df$municipio)) {
    df_m <- df %>% filter(municipio == m) %>% arrange(data_date)
    df_pre <- df_m %>% filter(periodo == 1)
    df_pos <- df_m %>% filter(periodo == 2)
    dfs_periodo <- list()
    if(nrow(df_pre) > 0) {
      dfs_periodo[["pre"]] <- interpolar_periodo(df_pre, metodo, 
                                                 nome_periodo = "Pré-Pandemia")
    }
    if(nrow(df_pos) > 0) {
      dfs_periodo[["pos"]] <- interpolar_periodo(df_pos, metodo, 
                                                 nome_periodo = "Pós-Pandemia")
    }
    if(length(dfs_periodo) > 0) {
      df_mensal_m <- bind_rows(dfs_periodo) %>%
        arrange(data)
      df_mensal_m <- df_mensal_m %>%
        mutate(
          periodo = case_when(
            data < ym("2020-04") ~ "Pré-Pandemia",
            data > ym("2022-03") ~ "Pós-Pandemia",
            TRUE ~ "Pandemia (NA)"
          )
        )
      resultados[[m]] <- df_mensal_m
    }
  }
  resultado_final <- bind_rows(resultados) %>%
    mutate(
      ano = year(data),
      mes = month(data),
      trimestre = quarter(data),
      data_str = format(data, "%Y-%m")
    ) %>%
    select(municipio, data, data_str, ano, mes, trimestre, 
           taxa_desemprego, periodo) %>%
    arrange(municipio, data)
  return(resultado_final)
}



interpolar_periodo <- function(df_periodo, metodo, nome_periodo) {
  if(nrow(df_periodo) < 2) {
    return(
      data.frame(
        municipio = df_periodo$municipio[1],
        data = df_periodo$data_date[1],
        taxa_desemprego = df_periodo$taxa_desemprego[1]
      )
    )
  }
  df_periodo <- df_periodo %>% filter(!is.na(taxa_desemprego))
  if(nrow(df_periodo) < 2) {
    return(NULL)
  }
  data_min <- min(df_periodo$data_date, na.rm = TRUE)
  data_max <- max(df_periodo$data_date, na.rm = TRUE)
  seq_mensal <- seq(data_min, data_max, by = "month")
  df_mensal <- data.frame(
    municipio = df_periodo$municipio[1],
    data = seq_mensal,
    data_num = as.numeric(seq_mensal)
  )
  x_orig <- as.numeric(df_periodo$data_date)
  y_orig <- df_periodo$taxa_desemprego
  if(metodo == "linear") {
    df_mensal$taxa_desemprego <- approx(
      x = x_orig, 
      y = y_orig,
      xout = df_mensal$data_num,
      rule = 2,
      method = "linear"
    )$y
  } else if(metodo == "spline") {
    if(length(x_orig) >= 3) {
      spline_fit <- splinefun(x_orig, y_orig, method = "monoH.FC")
      df_mensal$taxa_desemprego <- spline_fit(df_mensal$data_num)
    } else {
      df_mensal$taxa_desemprego <- approx(
        x = x_orig, 
        y = y_orig,
        xout = df_mensal$data_num,
        rule = 2,
        method = "linear"
      )$y
    }
  }
  df_mensal$taxa_desemprego <- pmax(0, pmin(100, df_mensal$taxa_desemprego))
  for(i in 1:nrow(df_periodo)) {
    mes_obs <- df_periodo$data_date[i]
    idx <- which(df_mensal$data == mes_obs)
    if(length(idx) > 0) {
      df_mensal$taxa_desemprego[idx] <- df_periodo$taxa_desemprego[i]
    }
  }
  return(df_mensal %>% select(-data_num))
}

# Aplicar interpolação
PNAD_mensal <- mensalizar_com_pausa(PNAD_com_periodo, metodo = "spline")

completar_meses_pandemia <- function(df_mensal) {
  municipios <- unique(df_mensal$municipio)
  resultados_completos <- list()
  for(m in municipios) {
    df_m <- df_mensal %>% filter(municipio == m) %>% arrange(data)
    data_min <- min(df_m$data, na.rm = TRUE)
    data_max <- max(df_m$data, na.rm = TRUE)
    seq_completa <- seq(data_min, data_max, by = "month")
    df_completo <- data.frame(
      municipio = m,
      data = seq_completa,
      stringsAsFactors = FALSE
    )
    df_completo <- df_completo %>%
      left_join(df_m, by = c("municipio", "data"))
    df_completo <- df_completo %>%
      mutate(
        taxa_desemprego = ifelse(
          data >= ym("2020-04") & data <= ym("2022-03"),
          NA_real_,
          taxa_desemprego
        ),
        periodo = case_when(
          data < ym("2020-04") ~ "Pré-Pandemia",
          data > ym("2022-03") ~ "Pós-Pandemia",
          TRUE ~ "Pandemia (NA)"
        ),
        ano = year(data),
        mes = month(data),
        trimestre = quarter(data),
        data_str = format(data, "%Y-%m")
      )
    resultados_completos[[m]] <- df_completo
  }
  df_final <- bind_rows(resultados_completos) %>%
    select(municipio, data, data_str, ano, mes, trimestre, 
           taxa_desemprego, periodo) %>%
    arrange(municipio, data)
  
  return(df_final)
}

PNAD_mensal_completo <- completar_meses_pandemia(PNAD_mensal)

# Grafico
sp_final <- PNAD_mensal_completo %>% filter(municipio == "São Paulo (SP)")
class(sp_final$data)  

ggplot(sp_final, aes(x = data, y = taxa_desemprego)) +
  geom_line(data = sp_final %>% filter(periodo == "Pré-Pandemia", !is.na(taxa_desemprego)),
            aes(color = "Pré-Pandemia"), size = 1) +
  geom_line(data = sp_final %>% filter(periodo == "Pós-Pandemia", !is.na(taxa_desemprego)),
            aes(color = "Pós-Pandemia"), size = 1) +
  geom_point(data = sp_final %>% filter(!is.na(taxa_desemprego)),
             aes(color = periodo), size = 1.5, alpha = 0.5) +
  annotate("rect",
           xmin = as.Date("2020-04-01"),
           xmax = as.Date("2022-03-01"),
           ymin = -Inf, ymax = Inf,
           fill = "gray80", alpha = 0.3) +
  annotate("text",
           x = as.Date("2021-01-01"),
           y = max(sp_final$taxa_desemprego, na.rm = TRUE) * 0.9,
           label = "PERÍODO SEM DADOS (NA)\n2020 Q2 - 2022 Q1",
           size = 4, color = "gray40", fontface = "bold") +
  labs(
    title = "Taxa de Desemprego - São Paulo (SP)",
    subtitle = "Período da pandemia explicitamente como NA (sem interpolação)",
    x = "Data", y = "Taxa de Desemprego (%)",
    color = "Período",
    caption = "Nota: O período 2020Q2-2022Q1 foi mantido como NA conforme decisão metodológica"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(legend.position = "bottom")

####
#### Extrair ####
####
# Fromatatar
base_mensal <- PNAD_mensal_completo %>%
  select(municipio, data, taxa_desemprego) %>%
  mutate(
    data = format(data, "%Y-%m") 
  ) %>%
  arrange(municipio, data)

base_trimestral <- PNAD_raw %>%
  select(municipio, data, taxa_desemprego) %>%
  arrange(municipio, data)

# Exportar
output_dir <- "C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Desemprego/PNAD_TOT/"
write_xlsx(base_trimestral, paste0(output_dir, "PNAD_t.xlsx"))
write_xlsx(base_mensal, paste0(output_dir, "PNAD_m.xlsx"))


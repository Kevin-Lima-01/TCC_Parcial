library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(ggplot2)
library(writexl)

####
#### PME ####
####
# Desocupados
DES_PME <- read_excel("FGV/TCC/Dados/Desemprego/PME_10/DES_PME_10.xlsx")
DES_PME <- DES_PME %>% slice(-c(1, 2, n()))
dates <- as.character(DES_PME[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
DES_PME[1, ] <- t(dates)
colnames(DES_PME)[1] <- ""
DES_PME[1, 1] <- "data"

colnames(DES_PME)[1] <- "municipio"  
DES_PME[1, 1] <- "municipio"  
DES_PME <- DES_PME[-2, ]
colnames(DES_PME) <- as.character(DES_PME[1, ])
DES_PME <- DES_PME[-1, ]
DES_PME[, -1] <- lapply(DES_PME[, -1], as.numeric)

DES_PME <- DES_PME %>%
  pivot_longer(cols = -municipio,
               names_to = "data",
               values_to = "num_desocupados") %>%
  mutate(
    num_desocupados = as.numeric(num_desocupados) * 1000,
    data = ym(data)
  ) %>%
  mutate(data = format(data, "%Y-%m")) %>%
  arrange(municipio, data)

# PEA
PEA_PME <- read_excel("FGV/TCC/Dados/Desemprego/PME_10/PEA_PME_10.xlsx")
PEA_PME <- PEA_PME %>% slice(-c(1, 2, n()))
dates <- as.character(PEA_PME[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
PEA_PME[1, ] <- t(dates)
colnames(PEA_PME)[1] <- ""
PEA_PME[1, 1] <- "data"

colnames(PEA_PME)[1] <- "municipio"  
PEA_PME[1, 1] <- "municipio"  
PEA_PME <- PEA_PME[-2, ]
colnames(PEA_PME) <- as.character(PEA_PME[1, ])
PEA_PME <- PEA_PME[-1, ]
PEA_PME[, -1] <- lapply(PEA_PME[, -1], as.numeric)

PEA_PME <- PEA_PME %>%
  pivot_longer(cols = -municipio,
               names_to = "data",
               values_to = "num_pea") %>%
  mutate(
    num_pea = as.numeric(num_pea) * 1000,
    data = ym(data)
  ) %>%
  mutate(data = format(data, "%Y-%m")) %>%
  arrange(municipio, data)

# Juntar
PME <- DES_PME %>%
  full_join(PEA_PME, by = c("municipio", "data")) %>%
  arrange(municipio, data)
PME <- PME %>%
  mutate(
    taxa_desemprego = (num_desocupados / num_pea) * 100
  ) %>%
  select(municipio, data, taxa_desemprego)

write_xlsx(PME, "FGV/TCC/Dados/Desemprego/PME_10/PME_sub10_m.xlsx")

####
#### Trimestralizar ####
####
# Reestruturar as bases
DES_PME_trim <- DES_PME %>%
  mutate(
    data = as.Date(paste0(data, "-01")),
    ano = year(data),
    trimestre = quarter(data),
    periodo_trimestre = paste0(ano, "-T", trimestre)
  )

PEA_PME_trim <- PEA_PME %>%
  mutate(
    data = as.Date(paste0(data, "-01")),
    ano = year(data),
    trimestre = quarter(data),
    periodo_trimestre = paste0(ano, "-T", trimestre)
  )

DES_PME_trim <- DES_PME_trim %>%
  group_by(municipio, ano, trimestre, periodo_trimestre) %>%
  summarise(
    num_desocupados = sum(num_desocupados, na.rm = TRUE),
    .groups = "drop"
  )

PEA_PME_trim <- PEA_PME_trim %>%
  group_by(municipio, ano, trimestre, periodo_trimestre) %>%
  summarise(
    num_pea = sum(num_pea, na.rm = TRUE),
    .groups = "drop"
  )

# Taxa trimestral
PME_trim <- DES_PME_trim %>%
  full_join(PEA_PME_trim, by = c("municipio", "ano", "trimestre", "periodo_trimestre"))

PME_trim <- PME_trim %>%
  mutate(
    taxa_desemprego = round((num_desocupados / num_pea) * 100, 2)
  ) %>%
  select(municipio, periodo_trimestre, taxa_desemprego) %>%
  arrange(municipio, periodo_trimestre)

write_xlsx(PME_trim, "FGV/TCC/Dados/Desemprego/PME_10/PME_sub10_t.xlsx")
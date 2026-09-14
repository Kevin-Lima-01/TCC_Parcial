library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(ggplot2)
library(writexl)



####
#### PDE ####
####
# Belo Horizonte
BH <- read_excel("FGV/TCC/Dados/Desemprego/PDE/BH.xls")
BH <- BH[, c(1, 11)]
BH <- BH[-c(1:13), ]
colnames(BH)[1] <- "data"
colnames(BH)[2] <- "des"
BH <- BH[1:(nrow(BH) - 9), ]

n_linhas <- nrow(BH)
BH$data <- format(seq(as.Date("1996-01-01"), 
                      by = "month", 
                      length.out = n_linhas), 
                  "%Y-%m")

BH <- BH %>%
  mutate(
    municipio = "Belo Horizonte (MG)"  
  ) %>%
  select(municipio, data, des)

# Brasilia
BRA <- read_excel("FGV/TCC/Dados/Desemprego/PDE/BRA.xlsx")
BRA <- BRA[, c(1, 13)]
BRA <- BRA[-c(1:11), ]
colnames(BRA)[1] <- "data"
colnames(BRA)[2] <- "des"
BRA <- BRA[1:(nrow(BRA) - 9), ]

n_linhas <- nrow(BRA)
BRA$data <- format(seq(as.Date("2001-03-01"), 
                       by = "month", 
                       length.out = n_linhas), 
                   "%Y-%m")

BRA <- BRA %>%
  mutate(
    municipio = "Brasília (DF)"  
  ) %>%
  select(municipio, data, des)

# Fortaleza
FOR <- read_excel("FGV/TCC/Dados/Desemprego/PDE/FOR.xls")
FOR <- FOR[, c(1, 11)]
FOR <- FOR[-c(1:13), ]
colnames(FOR)[1] <- "data"
colnames(FOR)[2] <- "des"
FOR <- FOR[1:(nrow(FOR) - 10), ]

n_linhas <- nrow(FOR)
FOR$data <- format(seq(as.Date("2008-12-01"), 
                       by = "month", 
                       length.out = n_linhas), 
                   "%Y-%m")

FOR <- FOR %>%
  mutate(
    municipio = "Fortaleza (CE)"  
  ) %>%
  select(municipio, data, des)

# Porto Alegre
POA <- read_excel("FGV/TCC/Dados/Desemprego/PDE/POA.xls")
POA <- POA[, c(1, 11)]
POA <- POA[-c(1:13), ]
colnames(POA)[1] <- "data"
colnames(POA)[2] <- "des"
POA <- POA[1:(nrow(POA) - 12), ]

n_linhas <- nrow(POA)
POA$data <- format(seq(as.Date("1992-06-01"), 
                       by = "month", 
                       length.out = n_linhas), 
                   "%Y-%m")

POA <- POA %>%
  mutate(
    municipio = "Porto Alegre (RS)"  
  ) %>%
  select(municipio, data, des)

# Recife
REC <- read_excel("FGV/TCC/Dados/Desemprego/PDE/REC.xls")
REC <- REC[, c(1, 11)]
REC <- REC[-c(1:13), ]
colnames(REC)[1] <- "data"
colnames(REC)[2] <- "des"
REC <- REC[1:(nrow(REC) - 12), ]

n_linhas <- nrow(REC)
REC$data <- format(seq(as.Date("1997-11-01"), 
                       by = "month", 
                       length.out = n_linhas), 
                   "%Y-%m")

REC <- REC %>%
  mutate(
    municipio = "Recife (PE)"  
  ) %>%
  select(municipio, data, des)

# São Paulo
SP <- read_excel("FGV/TCC/Dados/Desemprego/PDE/SP.xls")
SP <- SP[, c(1, 11)]
SP <- SP[-c(1:13), ]
colnames(SP)[1] <- "data"
colnames(SP)[2] <- "des"
SP <- SP[1:(nrow(SP) - 10), ]

n_linhas <- nrow(SP)
SP$data <- format(seq(as.Date("1988-04-01"), 
                      by = "month", 
                      length.out = n_linhas), 
                  "%Y-%m")

SP <- SP %>%
  mutate(
    municipio = "São Paulo (SP)"  
  ) %>%
  select(municipio, data, des)

# Salvador
SSA <- read_excel("FGV/TCC/Dados/Desemprego/PDE/SSA.xls")
SSA <- SSA[, c(1, 11)]
SSA <- SSA[-c(1:13), ]
colnames(SSA)[1] <- "data"
colnames(SSA)[2] <- "des"
SSA <- SSA[1:(nrow(SSA) - 8), ]

n_linhas <- nrow(SSA)
SSA$data <- format(seq(as.Date("1996-12-01"), 
                       by = "month", 
                       length.out = n_linhas), 
                   "%Y-%m")

SSA <- SSA %>%
  mutate(
    municipio = "Salvador (BA)"  
  ) %>%
  select(municipio, data, des)

####
#### Juntar ####
####

PDE <- bind_rows(BH, BRA, FOR, POA, REC, SP, SSA)

data_min <- min(PDE$data, na.rm = TRUE)
data_max <- max(PDE$data, na.rm = TRUE)
datas_completas <- format(seq(as.Date(paste0(data_min, "-01")), 
                              as.Date(paste0(data_max, "-01")), 
                              by = "month"), 
                          "%Y-%m")
municipios <- unique(PDE$municipio)
grid_completo <- expand.grid(
  municipio = municipios,
  data = datas_completas,
  stringsAsFactors = FALSE
)

PDE <- grid_completo %>%
  left_join(PDE, by = c("municipio", "data")) %>%
  arrange(municipio, data)

write_xlsx(PDE, "FGV/TCC/Dados/Desemprego/PDE/PDE_sub10_m.xlsx")

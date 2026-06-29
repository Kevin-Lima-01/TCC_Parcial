library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(ggplot2)
library(writexl)



####
#### 2020-2026 ####
####
# Ipca
INT1 <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Inflacao/INT/20a26INT.xlsx")
INT1 <- INT1 %>% slice(-c(1, 2, n()))
dates <- as.character(INT1[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
INT1[1, ] <- t(dates)
colnames(INT1)[1] <- ""
INT1[1, 1] <- "data"
INT1[2, 1] <- "indicador"

linha1 <- as.character(unlist(INT1[1, ]))      
linha2 <- as.character(unlist(INT1[2, ]))     
INT1 <- INT1[-(1:2), ]
colnames(INT1) <- paste0("V", 0:(ncol(INT1)-1))  
colnames(INT1)[1] <- "municipio"
INT1 <- INT1 %>%
  pivot_longer(
    cols = -municipio,
    names_to = "col_id",
    values_to = "var"
  ) %>%
  mutate(
    var = as.numeric(var),
    col_num = as.numeric(gsub("V", "", col_id)) + 1  
  )
INT1$data <- linha1[INT1$col_num]
INT1$indicador <- linha2[INT1$col_num]
INT1 <- INT1 %>% select(municipio, data, indicador, var)

# Peso
PNT1 <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Inflacao/PNT/20a26PNT.xlsx")
PNT1 <- PNT1 %>% slice(-c(1, 2, n()))
dates <- as.character(PNT1[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
PNT1[1, ] <- t(dates)
colnames(PNT1)[1] <- ""
PNT1[1, 1] <- "data"
PNT1[2, 1] <- "indicador"

linha1 <- as.character(unlist(PNT1[1, ]))      
linha2 <- as.character(unlist(PNT1[2, ]))     
PNT1 <- PNT1[-(1:2), ]
colnames(PNT1) <- paste0("V", 0:(ncol(PNT1)-1))  
colnames(PNT1)[1] <- "municipio"
PNT1 <- PNT1 %>%
  pivot_longer(
    cols = -municipio,
    names_to = "col_id",
    values_to = "peso"
  ) %>%
  mutate(
    peso = as.numeric(peso),
    col_num = as.numeric(gsub("V", "", col_id)) + 1  
  )
PNT1$data <- linha1[PNT1$col_num]
PNT1$indicador <- linha2[PNT1$col_num]
PNT1 <- PNT1 %>% select(municipio, data, indicador, peso)

PNT1 <- PNT1 %>%
  group_by(municipio, data) %>%
  mutate(
    soma_pesos = sum(peso, na.rm = TRUE),
    peso_normalizado = peso / soma_pesos
  ) %>%
  ungroup()

# Juntar
NT1 <- INT1 %>%
  left_join(
    PNT1 %>% select(municipio, data, indicador, peso_normalizado),
    by = c("municipio", "data", "indicador")
  )
NT1 <- NT1 %>%
  group_by(municipio, data) %>%
  summarise(
    VNT = sum(var * peso_normalizado, na.rm = TRUE),
    n_indicadores = n(),  
    .groups = "drop"
  )
NT1 <- NT1 %>%
  mutate(VNT = na_if(VNT, 0))

# Itens usados
df_indicadores <- data.frame(
  indicador = sort(unique(INT1$indicador))
)
print(df_indicadores)


####
#### 2012-2019 ####
####
# Ipca
INT2 <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Inflacao/INT/12a19INT.xlsx")
INT2 <- INT2 %>% slice(-c(1, 2, n()))
dates <- as.character(INT2[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
INT2[1, ] <- t(dates)
colnames(INT2)[1] <- ""
INT2[1, 1] <- "data"
INT2[2, 1] <- "indicador"

linha1 <- as.character(unlist(INT2[1, ]))      
linha2 <- as.character(unlist(INT2[2, ]))     
INT2 <- INT2[-(1:2), ]
colnames(INT2) <- paste0("V", 0:(ncol(INT2)-1))  
colnames(INT2)[1] <- "municipio"
INT2 <- INT2 %>%
  pivot_longer(
    cols = -municipio,
    names_to = "col_id",
    values_to = "var"
  ) %>%
  mutate(
    var = as.numeric(var),
    col_num = as.numeric(gsub("V", "", col_id)) + 1  
  )
INT2$data <- linha1[INT2$col_num]
INT2$indicador <- linha2[INT2$col_num]
INT2 <- INT2 %>% select(municipio, data, indicador, var)

# Peso
PNT2 <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Inflacao/PNT/12a19PNT.xlsx")
PNT2 <- PNT2 %>% slice(-c(1, 2, n()))
dates <- as.character(PNT2[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
PNT2[1, ] <- t(dates)
colnames(PNT2)[1] <- ""
PNT2[1, 1] <- "data"
PNT2[2, 1] <- "indicador"

linha1 <- as.character(unlist(PNT2[1, ]))      
linha2 <- as.character(unlist(PNT2[2, ]))     
PNT2 <- PNT2[-(1:2), ]
colnames(PNT2) <- paste0("V", 0:(ncol(PNT2)-1))  
colnames(PNT2)[1] <- "municipio"
PNT2 <- PNT2 %>%
  pivot_longer(
    cols = -municipio,
    names_to = "col_id",
    values_to = "peso"
  ) %>%
  mutate(
    peso = as.numeric(peso),
    col_num = as.numeric(gsub("V", "", col_id)) + 1  
  )
PNT2$data <- linha1[PNT2$col_num]
PNT2$indicador <- linha2[PNT2$col_num]
PNT2 <- PNT2 %>% select(municipio, data, indicador, peso)

PNT2 <- PNT2 %>%
  group_by(municipio, data) %>%
  mutate(
    soma_pesos = sum(peso, na.rm = TRUE),
    peso_normalizado = peso / soma_pesos
  ) %>%
  ungroup()

# Juntar
NT2 <- INT2 %>%
  left_join(
    PNT2 %>% select(municipio, data, indicador, peso_normalizado),
    by = c("municipio", "data", "indicador")
  )
NT2 <- NT2 %>%
  group_by(municipio, data) %>%
  summarise(
    VNT = sum(var * peso_normalizado, na.rm = TRUE),
    n_indicadores = n(),  
    .groups = "drop"
  )
NT2 <- NT2 %>%
  mutate(VNT = na_if(VNT, 0))

# Itens usados
df_indicadores <- data.frame(
  indicador = sort(unique(INT2$indicador))
)
print(df_indicadores)



####
#### 2006-2011 ####
####
# Ipca
INT3 <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Inflacao/INT/06a11INT.xlsx")
INT3 <- INT3 %>% slice(-c(1, 2, n()))
dates <- as.character(INT3[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
INT3[1, ] <- t(dates)
colnames(INT3)[1] <- ""
INT3[1, 1] <- "data"
INT3[2, 1] <- "indicador"

linha1 <- as.character(unlist(INT3[1, ]))      
linha2 <- as.character(unlist(INT3[2, ]))     
INT3 <- INT3[-(1:2), ]
colnames(INT3) <- paste0("V", 0:(ncol(INT3)-1))  
colnames(INT3)[1] <- "municipio"
INT3 <- INT3 %>%
  pivot_longer(
    cols = -municipio,
    names_to = "col_id",
    values_to = "var"
  ) %>%
  mutate(
    var = as.numeric(var),
    col_num = as.numeric(gsub("V", "", col_id)) + 1  
  )
INT3$data <- linha1[INT3$col_num]
INT3$indicador <- linha2[INT3$col_num]
INT3 <- INT3 %>% select(municipio, data, indicador, var)

# Peso
PNT3 <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Inflacao/PNT/06a11PNT.xlsx")
PNT3 <- PNT3 %>% slice(-c(1, 2, n()))
dates <- as.character(PNT3[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
PNT3[1, ] <- t(dates)
colnames(PNT3)[1] <- ""
PNT3[1, 1] <- "data"
PNT3[2, 1] <- "indicador"

linha1 <- as.character(unlist(PNT3[1, ]))      
linha2 <- as.character(unlist(PNT3[2, ]))     
PNT3 <- PNT3[-(1:2), ]
colnames(PNT3) <- paste0("V", 0:(ncol(PNT3)-1))  
colnames(PNT3)[1] <- "municipio"
PNT3 <- PNT3 %>%
  pivot_longer(
    cols = -municipio,
    names_to = "col_id",
    values_to = "peso"
  ) %>%
  mutate(
    peso = as.numeric(peso),
    col_num = as.numeric(gsub("V", "", col_id)) + 1  
  )
PNT3$data <- linha1[PNT3$col_num]
PNT3$indicador <- linha2[PNT3$col_num]
PNT3 <- PNT3 %>% select(municipio, data, indicador, peso)

PNT3 <- PNT3 %>%
  group_by(municipio, data) %>%
  mutate(
    soma_pesos = sum(peso, na.rm = TRUE),
    peso_normalizado = peso / soma_pesos
  ) %>%
  ungroup()

# Juntar
NT3 <- INT3 %>%
  left_join(
    PNT3 %>% select(municipio, data, indicador, peso_normalizado),
    by = c("municipio", "data", "indicador")
  )
NT3 <- NT3 %>%
  group_by(municipio, data) %>%
  summarise(
    VNT = sum(var * peso_normalizado, na.rm = TRUE),
    n_indicadores = n(),  
    .groups = "drop"
  )
NT3 <- NT3 %>%
  mutate(VNT = na_if(VNT, 0))

# Itens usados
df_indicadores <- data.frame(
  indicador = sort(unique(INT3$indicador))
)
print(df_indicadores)



####
#### 1999-2006 ####
####
# Ipca
INT4 <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Inflacao/INT/99a06INT.xlsx")
INT4 <- INT4 %>% slice(-c(1, 2, n()))
dates <- as.character(INT4[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
INT4[1, ] <- t(dates)
colnames(INT4)[1] <- ""
INT4[1, 1] <- "data"
INT4[2, 1] <- "indicador"

linha1 <- as.character(unlist(INT4[1, ]))      
linha2 <- as.character(unlist(INT4[2, ]))     
INT4 <- INT4[-(1:2), ]
colnames(INT4) <- paste0("V", 0:(ncol(INT4)-1))  
colnames(INT4)[1] <- "municipio"
INT4 <- INT4 %>%
  pivot_longer(
    cols = -municipio,
    names_to = "col_id",
    values_to = "var"
  ) %>%
  mutate(
    var = as.numeric(var),
    col_num = as.numeric(gsub("V", "", col_id)) + 1  
  )
INT4$data <- linha1[INT4$col_num]
INT4$indicador <- linha2[INT4$col_num]
INT4 <- INT4 %>% select(municipio, data, indicador, var)

# Peso
PNT4 <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Inflacao/PNT/99a06PNT.xlsx")
PNT4 <- PNT4 %>% slice(-c(1, 2, n()))
dates <- as.character(PNT4[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
PNT4[1, ] <- t(dates)
colnames(PNT4)[1] <- ""
PNT4[1, 1] <- "data"
PNT4[2, 1] <- "indicador"

linha1 <- as.character(unlist(PNT4[1, ]))      
linha2 <- as.character(unlist(PNT4[2, ]))     
PNT4 <- PNT4[-(1:2), ]
colnames(PNT4) <- paste0("V", 0:(ncol(PNT4)-1))  
colnames(PNT4)[1] <- "municipio"
PNT4 <- PNT4 %>%
  pivot_longer(
    cols = -municipio,
    names_to = "col_id",
    values_to = "peso"
  ) %>%
  mutate(
    peso = as.numeric(peso),
    col_num = as.numeric(gsub("V", "", col_id)) + 1  
  )
PNT4$data <- linha1[PNT4$col_num]
PNT4$indicador <- linha2[PNT4$col_num]
PNT4 <- PNT4 %>% select(municipio, data, indicador, peso)

PNT4 <- PNT4 %>%
  group_by(municipio, data) %>%
  mutate(
    soma_pesos = sum(peso, na.rm = TRUE),
    peso_normalizado = peso / soma_pesos
  ) %>%
  ungroup()

# Juntar
NT4 <- INT4 %>%
  left_join(
    PNT4 %>% select(municipio, data, indicador, peso_normalizado),
    by = c("municipio", "data", "indicador")
  )
NT4 <- NT4 %>%
  group_by(municipio, data) %>%
  summarise(
    VNT = sum(var * peso_normalizado, na.rm = TRUE),
    n_indicadores = n(),  
    .groups = "drop"
  )
NT4 <- NT4 %>%
  mutate(VNT = na_if(VNT, 0))

# Itens usados
df_indicadores <- data.frame(
  indicador = sort(unique(INT4$indicador))
)
print(df_indicadores)



####
#### 1991-1999 ####
####
# Ipca
INT5 <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Inflacao/INT/91a99INT.xlsx")
INT5 <- INT5 %>% slice(-c(1, 2, n()))
dates <- as.character(INT5[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
INT5[1, ] <- t(dates)
colnames(INT5)[1] <- ""
INT5[1, 1] <- "data"
INT5[2, 1] <- "indicador"

linha1 <- as.character(unlist(INT5[1, ]))      
linha2 <- as.character(unlist(INT5[2, ]))     
INT5 <- INT5[-(1:2), ]
colnames(INT5) <- paste0("V", 0:(ncol(INT5)-1))  
colnames(INT5)[1] <- "municipio"
INT5 <- INT5 %>%
  pivot_longer(
    cols = -municipio,
    names_to = "col_id",
    values_to = "var"
  ) %>%
  mutate(
    var = as.numeric(var),
    col_num = as.numeric(gsub("V", "", col_id)) + 1  
  )
INT5$data <- linha1[INT5$col_num]
INT5$indicador <- linha2[INT5$col_num]
INT5 <- INT5 %>% select(municipio, data, indicador, var)

# Peso
PNT5 <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Inflacao/PNT/91a99PNT.xlsx")
PNT5 <- PNT5 %>% slice(-c(1, 2, n()))
dates <- as.character(PNT5[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
PNT5[1, ] <- t(dates)
colnames(PNT5)[1] <- ""
PNT5[1, 1] <- "data"
PNT5[2, 1] <- "indicador"

linha1 <- as.character(unlist(PNT5[1, ]))      
linha2 <- as.character(unlist(PNT5[2, ]))     
PNT5 <- PNT5[-(1:2), ]
colnames(PNT5) <- paste0("V", 0:(ncol(PNT5)-1))  
colnames(PNT5)[1] <- "municipio"
PNT5 <- PNT5 %>%
  pivot_longer(
    cols = -municipio,
    names_to = "col_id",
    values_to = "peso"
  ) %>%
  mutate(
    peso = as.numeric(peso),
    col_num = as.numeric(gsub("V", "", col_id)) + 1  
  )
PNT5$data <- linha1[PNT5$col_num]
PNT5$indicador <- linha2[PNT5$col_num]
PNT5 <- PNT5 %>% select(municipio, data, indicador, peso)

PNT5 <- PNT5 %>%
  group_by(municipio, data) %>%
  mutate(
    soma_pesos = sum(peso, na.rm = TRUE),
    peso_normalizado = peso / soma_pesos
  ) %>%
  ungroup()

# Juntar
NT5 <- INT5 %>%
  left_join(
    PNT5 %>% select(municipio, data, indicador, peso_normalizado),
    by = c("municipio", "data", "indicador")
  )
NT5 <- NT5 %>%
  group_by(municipio, data) %>%
  summarise(
    VNT = sum(var * peso_normalizado, na.rm = TRUE),
    n_indicadores = n(),  
    .groups = "drop"
  )
NT5 <- NT5 %>%
  mutate(VNT = na_if(VNT, 0))

# Itens usados
df_indicadores <- data.frame(
  indicador = sort(unique(INT5$indicador))
)
print(df_indicadores)



####
#### Consolidação ####
####
# Construção
NT <- bind_rows(
  NT1 %>% select(-n_indicadores),
  NT2 %>% select(-n_indicadores),
  NT3 %>% select(-n_indicadores),
  NT4 %>% select(-n_indicadores),
  NT5 %>% select(-n_indicadores)
)
NT <- NT %>%
  complete(municipio, data)

# Plot
df_plot <- NT %>%
  filter(municipio == "São Paulo (SP)") %>%
  mutate(data_date = ym(data))

ggplot(df_plot, aes(x = data_date, y = VNT)) +
  geom_line(color = "#2c3e50", linewidth = 0.8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "VNT - São Paulo (SP)", x = "", y = "VNT") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.minor = element_blank()
  )

ggplot(df_plot %>% filter(data_date >= ymd("1996-01-01")), 
       aes(x = data_date, y = VNT)) +
  geom_line(color = "#27ae60", linewidth = 0.8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "VNT - São Paulo (SP) - Após 1996", x = "", y = "VNT") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.minor = element_blank()
  )

write_xlsx(NT, "C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/NT.xlsx")
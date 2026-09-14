library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(writexl)



####
#### 2020-2026 ####
####
# Ipca
IT1 <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Inflacao/IT/20a26IT.xlsx")
IT1 <- IT1 %>% slice(-c(1, 2, n()))
dates <- as.character(IT1[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
IT1[1, ] <- t(dates)
colnames(IT1)[1] <- ""
IT1[1, 1] <- "data"
IT1[2, 1] <- "indicador"

linha1 <- as.character(unlist(IT1[1, ]))      
linha2 <- as.character(unlist(IT1[2, ]))     
IT1 <- IT1[-(1:2), ]
colnames(IT1) <- paste0("V", 0:(ncol(IT1)-1))  
colnames(IT1)[1] <- "municipio"
IT1 <- IT1 %>%
  pivot_longer(
    cols = -municipio,
    names_to = "col_id",
    values_to = "var"
  ) %>%
  mutate(
    var = as.numeric(var),
    col_num = as.numeric(gsub("V", "", col_id)) + 1  
  )
IT1$data <- linha1[IT1$col_num]
IT1$indicador <- linha2[IT1$col_num]
IT1 <- IT1 %>% select(municipio, data, indicador, var)

# Peso
PT1 <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Inflacao/PT/20a26PT.xlsx")
PT1 <- PT1 %>% slice(-c(1, 2, n()))
dates <- as.character(PT1[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
PT1[1, ] <- t(dates)
colnames(PT1)[1] <- ""
PT1[1, 1] <- "data"
PT1[2, 1] <- "indicador"

linha1 <- as.character(unlist(PT1[1, ]))      
linha2 <- as.character(unlist(PT1[2, ]))     
PT1 <- PT1[-(1:2), ]
colnames(PT1) <- paste0("V", 0:(ncol(PT1)-1))  
colnames(PT1)[1] <- "municipio"
PT1 <- PT1 %>%
  pivot_longer(
    cols = -municipio,
    names_to = "col_id",
    values_to = "peso"
  ) %>%
  mutate(
    peso = as.numeric(peso),
    col_num = as.numeric(gsub("V", "", col_id)) + 1  
  )
PT1$data <- linha1[PT1$col_num]
PT1$indicador <- linha2[PT1$col_num]
PT1 <- PT1 %>% select(municipio, data, indicador, peso)

PT1 <- PT1 %>%
  group_by(municipio, data) %>%
  mutate(
    soma_pesos = sum(peso, na.rm = TRUE),
    peso_normalizado = peso / soma_pesos
  ) %>%
  ungroup()

# Juntar
T1 <- IT1 %>%
  left_join(
    PT1 %>% select(municipio, data, indicador, peso_normalizado),
    by = c("municipio", "data", "indicador")
  )
T1 <- T1 %>%
  group_by(municipio, data) %>%
  summarise(
    VT = sum(var * peso_normalizado, na.rm = TRUE),
    n_indicadores = n(),  
    .groups = "drop"
  )
T1 <- T1 %>%
  mutate(VT = na_if(VT, 0))

# Itens usados
df_indicadores <- data.frame(
  indicador = sort(unique(IT1$indicador))
)
print(df_indicadores)



####
#### 2012-2019 ####
####
# Ipca
IT2 <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Inflacao/IT/12a19IT.xlsx")
IT2 <- IT2 %>% slice(-c(1, 2, n()))
dates <- as.character(IT2[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
IT2[1, ] <- t(dates)
colnames(IT2)[1] <- ""
IT2[1, 1] <- "data"
IT2[2, 1] <- "indicador"

linha1 <- as.character(unlist(IT2[1, ]))      
linha2 <- as.character(unlist(IT2[2, ]))     
IT2 <- IT2[-(1:2), ]
colnames(IT2) <- paste0("V", 0:(ncol(IT2)-1))  
colnames(IT2)[1] <- "municipio"
IT2 <- IT2 %>%
  pivot_longer(
    cols = -municipio,
    names_to = "col_id",
    values_to = "var"
  ) %>%
  mutate(
    var = as.numeric(var),
    col_num = as.numeric(gsub("V", "", col_id)) + 1  
  )
IT2$data <- linha1[IT2$col_num]
IT2$indicador <- linha2[IT2$col_num]
IT2 <- IT2 %>% select(municipio, data, indicador, var)

# Peso
PT2 <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Inflacao/PT/12a19PT.xlsx")
PT2 <- PT2 %>% slice(-c(1, 2, n()))
dates <- as.character(PT2[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
PT2[1, ] <- t(dates)
colnames(PT2)[1] <- ""
PT2[1, 1] <- "data"
PT2[2, 1] <- "indicador"

linha1 <- as.character(unlist(PT2[1, ]))      
linha2 <- as.character(unlist(PT2[2, ]))     
PT2 <- PT2[-(1:2), ]
colnames(PT2) <- paste0("V", 0:(ncol(PT2)-1))  
colnames(PT2)[1] <- "municipio"
PT2 <- PT2 %>%
  pivot_longer(
    cols = -municipio,
    names_to = "col_id",
    values_to = "peso"
  ) %>%
  mutate(
    peso = as.numeric(peso),
    col_num = as.numeric(gsub("V", "", col_id)) + 1  
  )
PT2$data <- linha1[PT2$col_num]
PT2$indicador <- linha2[PT2$col_num]
PT2 <- PT2 %>% select(municipio, data, indicador, peso)

PT2 <- PT2 %>%
  group_by(municipio, data) %>%
  mutate(
    soma_pesos = sum(peso, na.rm = TRUE),
    peso_normalizado = peso / soma_pesos
  ) %>%
  ungroup()

# Juntar
T2 <- IT2 %>%
  left_join(
    PT2 %>% select(municipio, data, indicador, peso_normalizado),
    by = c("municipio", "data", "indicador")
  )
T2 <- T2 %>%
  group_by(municipio, data) %>%
  summarise(
    VT = sum(var * peso_normalizado, na.rm = TRUE),
    n_indicadores = n(),  
    .groups = "drop"
  )
T2 <- T2 %>%
  mutate(VT = na_if(VT, 0))

# Itens usados
df_indicadores <- data.frame(
  indicador = sort(unique(IT2$indicador))
)
print(df_indicadores)



####
#### 2006-2011 ####
####
# Ipca
IT3 <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Inflacao/IT/06a11IT.xlsx")
IT3 <- IT3 %>% slice(-c(1, 2, n()))
dates <- as.character(IT3[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
IT3[1, ] <- t(dates)
colnames(IT3)[1] <- ""
IT3[1, 1] <- "data"
IT3[2, 1] <- "indicador"

linha1 <- as.character(unlist(IT3[1, ]))      
linha2 <- as.character(unlist(IT3[2, ]))     
IT3 <- IT3[-(1:2), ]
colnames(IT3) <- paste0("V", 0:(ncol(IT3)-1))  
colnames(IT3)[1] <- "municipio"
IT3 <- IT3 %>%
  pivot_longer(
    cols = -municipio,
    names_to = "col_id",
    values_to = "var"
  ) %>%
  mutate(
    var = as.numeric(var),
    col_num = as.numeric(gsub("V", "", col_id)) + 1  
  )
IT3$data <- linha1[IT3$col_num]
IT3$indicador <- linha2[IT3$col_num]
IT3 <- IT3 %>% select(municipio, data, indicador, var)

# Peso
PT3 <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Inflacao/PT/06a11PT.xlsx")
PT3 <- PT3 %>% slice(-c(1, 2, n()))
dates <- as.character(PT3[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
PT3[1, ] <- t(dates)
colnames(PT3)[1] <- ""
PT3[1, 1] <- "data"
PT3[2, 1] <- "indicador"

linha1 <- as.character(unlist(PT3[1, ]))      
linha2 <- as.character(unlist(PT3[2, ]))     
PT3 <- PT3[-(1:2), ]
colnames(PT3) <- paste0("V", 0:(ncol(PT3)-1))  
colnames(PT3)[1] <- "municipio"
PT3 <- PT3 %>%
  pivot_longer(
    cols = -municipio,
    names_to = "col_id",
    values_to = "peso"
  ) %>%
  mutate(
    peso = as.numeric(peso),
    col_num = as.numeric(gsub("V", "", col_id)) + 1  
  )
PT3$data <- linha1[PT3$col_num]
PT3$indicador <- linha2[PT3$col_num]
PT3 <- PT3 %>% select(municipio, data, indicador, peso)

PT3 <- PT3 %>%
  group_by(municipio, data) %>%
  mutate(
    soma_pesos = sum(peso, na.rm = TRUE),
    peso_normalizado = peso / soma_pesos
  ) %>%
  ungroup()

# Juntar
T3 <- IT3 %>%
  left_join(
    PT3 %>% select(municipio, data, indicador, peso_normalizado),
    by = c("municipio", "data", "indicador")
  )
T3 <- T3 %>%
  group_by(municipio, data) %>%
  summarise(
    VT = sum(var * peso_normalizado, na.rm = TRUE),
    n_indicadores = n(),  
    .groups = "drop"
  )
T3 <- T3 %>%
  mutate(VT = na_if(VT, 0))

# Itens usados
df_indicadores <- data.frame(
  indicador = sort(unique(IT3$indicador))
)
print(df_indicadores)



####
#### 1999-2006 ####
####
# Ipca
IT4 <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Inflacao/IT/99a06IT.xlsx")
IT4 <- IT4 %>% slice(-c(1, 2, n()))
dates <- as.character(IT4[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
IT4[1, ] <- t(dates)
colnames(IT4)[1] <- ""
IT4[1, 1] <- "data"
IT4[2, 1] <- "indicador"

linha1 <- as.character(unlist(IT4[1, ]))      
linha2 <- as.character(unlist(IT4[2, ]))     
IT4 <- IT4[-(1:2), ]
colnames(IT4) <- paste0("V", 0:(ncol(IT4)-1))  
colnames(IT4)[1] <- "municipio"
IT4 <- IT4 %>%
  pivot_longer(
    cols = -municipio,
    names_to = "col_id",
    values_to = "var"
  ) %>%
  mutate(
    var = as.numeric(var),
    col_num = as.numeric(gsub("V", "", col_id)) + 1  
  )
IT4$data <- linha1[IT4$col_num]
IT4$indicador <- linha2[IT4$col_num]
IT4 <- IT4 %>% select(municipio, data, indicador, var)

# Peso
PT4 <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Inflacao/PT/99a06PT.xlsx")
PT4 <- PT4 %>% slice(-c(1, 2, n()))
dates <- as.character(PT4[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
PT4[1, ] <- t(dates)
colnames(PT4)[1] <- ""
PT4[1, 1] <- "data"
PT4[2, 1] <- "indicador"

linha1 <- as.character(unlist(PT4[1, ]))      
linha2 <- as.character(unlist(PT4[2, ]))     
PT4 <- PT4[-(1:2), ]
colnames(PT4) <- paste0("V", 0:(ncol(PT4)-1))  
colnames(PT4)[1] <- "municipio"
PT4 <- PT4 %>%
  pivot_longer(
    cols = -municipio,
    names_to = "col_id",
    values_to = "peso"
  ) %>%
  mutate(
    peso = as.numeric(peso),
    col_num = as.numeric(gsub("V", "", col_id)) + 1  
  )
PT4$data <- linha1[PT4$col_num]
PT4$indicador <- linha2[PT4$col_num]
PT4 <- PT4 %>% select(municipio, data, indicador, peso)

PT4 <- PT4 %>%
  group_by(municipio, data) %>%
  mutate(
    soma_pesos = sum(peso, na.rm = TRUE),
    peso_normalizado = peso / soma_pesos
  ) %>%
  ungroup()

# Juntar
T4 <- IT4 %>%
  left_join(
    PT4 %>% select(municipio, data, indicador, peso_normalizado),
    by = c("municipio", "data", "indicador")
  )
T4 <- T4 %>%
  group_by(municipio, data) %>%
  summarise(
    VT = sum(var * peso_normalizado, na.rm = TRUE),
    n_indicadores = n(),  
    .groups = "drop"
  )
T4 <- T4 %>%
  mutate(VT = na_if(VT, 0))

# Itens usados
df_indicadores <- data.frame(
  indicador = sort(unique(IT4$indicador))
)
print(df_indicadores)



####
#### 1991-1999 ####
####
# Ipca
IT5 <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Inflacao/IT/91a99IT.xlsx")
IT5 <- IT5 %>% slice(-c(1, 2, n()))
dates <- as.character(IT5[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
IT5[1, ] <- t(dates)
colnames(IT5)[1] <- ""
IT5[1, 1] <- "data"
IT5[2, 1] <- "indicador"

linha1 <- as.character(unlist(IT5[1, ]))      
linha2 <- as.character(unlist(IT5[2, ]))     
IT5 <- IT5[-(1:2), ]
colnames(IT5) <- paste0("V", 0:(ncol(IT5)-1))  
colnames(IT5)[1] <- "municipio"
IT5 <- IT5 %>%
  pivot_longer(
    cols = -municipio,
    names_to = "col_id",
    values_to = "var"
  ) %>%
  mutate(
    var = as.numeric(var),
    col_num = as.numeric(gsub("V", "", col_id)) + 1  
  )
IT5$data <- linha1[IT5$col_num]
IT5$indicador <- linha2[IT5$col_num]
IT5 <- IT5 %>% select(municipio, data, indicador, var)

# Peso
PT5 <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/Inflacao/PT/91a99PT.xlsx")
PT5 <- PT5 %>% slice(-c(1, 2, n()))
dates <- as.character(PT5[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
PT5[1, ] <- t(dates)
colnames(PT5)[1] <- ""
PT5[1, 1] <- "data"
PT5[2, 1] <- "indicador"

linha1 <- as.character(unlist(PT5[1, ]))      
linha2 <- as.character(unlist(PT5[2, ]))     
PT5 <- PT5[-(1:2), ]
colnames(PT5) <- paste0("V", 0:(ncol(PT5)-1))  
colnames(PT5)[1] <- "municipio"
PT5 <- PT5 %>%
  pivot_longer(
    cols = -municipio,
    names_to = "col_id",
    values_to = "peso"
  ) %>%
  mutate(
    peso = as.numeric(peso),
    col_num = as.numeric(gsub("V", "", col_id)) + 1  
  )
PT5$data <- linha1[PT5$col_num]
PT5$indicador <- linha2[PT5$col_num]
PT5 <- PT5 %>% select(municipio, data, indicador, peso)

PT5 <- PT5 %>%
  group_by(municipio, data) %>%
  mutate(
    soma_pesos = sum(peso, na.rm = TRUE),
    peso_normalizado = peso / soma_pesos
  ) %>%
  ungroup()

# Juntar
T5 <- IT5 %>%
  left_join(
    PT5 %>% select(municipio, data, indicador, peso_normalizado),
    by = c("municipio", "data", "indicador")
  )
T5 <- T5 %>%
  group_by(municipio, data) %>%
  summarise(
    VT = sum(var * peso_normalizado, na.rm = TRUE),
    n_indicadores = n(),  
    .groups = "drop"
  )
T5 <- T5 %>%
  mutate(VT = na_if(VT, 0))

# Itens usados
df_indicadores <- data.frame(
  indicador = sort(unique(IT5$indicador))
)
print(df_indicadores)



####
#### Consolidação ####
####
# Construção
T <- bind_rows(
  T1 %>% select(-n_indicadores),
  T2 %>% select(-n_indicadores),
  T3 %>% select(-n_indicadores),
  T4 %>% select(-n_indicadores),
  T5 %>% select(-n_indicadores)
)
T <- T %>%
  complete(municipio, data)

# Plot
df_plot <- T %>%
  filter(municipio == "São Paulo (SP)") %>%
  mutate(data_date = ym(data))

ggplot(df_plot, aes(x = data_date, y = VT)) +
  geom_line(color = "#2c3e50", linewidth = 0.8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "VT - São Paulo (SP)", x = "", y = "VT") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.minor = element_blank()
  )

ggplot(df_plot %>% filter(data_date >= ymd("1996-01-01")), 
       aes(x = data_date, y = VT)) +
  geom_line(color = "#27ae60", linewidth = 0.8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "VT - São Paulo (SP) - Após 1996", x = "", y = "VT") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.minor = element_blank()
  )

write_xlsx(T, "C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/T.xlsx")
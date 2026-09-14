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
IPCA1 <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/IPCA/20a26IPCA.xlsx")
IPCA1 <- IPCA1 %>% slice(-c(1, 2, n()))
dates <- as.character(IPCA1[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
IPCA1[1, ] <- t(dates)
colnames(IPCA1)[1] <- ""
IPCA1[1, 1] <- "data"
IPCA1[2, 1] <- "indicador"

linha1 <- as.character(unlist(IPCA1[1, ]))      
linha2 <- as.character(unlist(IPCA1[2, ]))     
IPCA1 <- IPCA1[-(1:2), ]
colnames(IPCA1) <- paste0("V", 0:(ncol(IPCA1)-1))  
colnames(IPCA1)[1] <- "municipio"
IPCA1 <- IPCA1 %>%
  pivot_longer(
    cols = -municipio,
    names_to = "col_id",
    values_to = "var"
  ) %>%
  mutate(
    var = as.numeric(var),
    col_num = as.numeric(gsub("V", "", col_id)) + 1  
  )
IPCA1$data <- linha1[IPCA1$col_num]
IPCA1$indicador <- linha2[IPCA1$col_num]
IPCA1 <- IPCA1 %>% select(municipio, data, indicador, var)
IPCA1 <- IPCA1[, -3]



####
#### 2012-2019 ####
####
# Ipca
IPCA2 <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/IPCA/12a19IPCA.xlsx")
IPCA2 <- IPCA2 %>% slice(-c(1, 2, n()))
dates <- as.character(IPCA2[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
IPCA2[1, ] <- t(dates)
colnames(IPCA2)[1] <- ""
IPCA2[1, 1] <- "data"
IPCA2[2, 1] <- "indicador"

linha1 <- as.character(unlist(IPCA2[1, ]))      
linha2 <- as.character(unlist(IPCA2[2, ]))     
IPCA2 <- IPCA2[-(1:2), ]
colnames(IPCA2) <- paste0("V", 0:(ncol(IPCA2)-1))  
colnames(IPCA2)[1] <- "municipio"
IPCA2 <- IPCA2 %>%
  pivot_longer(
    cols = -municipio,
    names_to = "col_id",
    values_to = "var"
  ) %>%
  mutate(
    var = as.numeric(var),
    col_num = as.numeric(gsub("V", "", col_id)) + 1  
  )
IPCA2$data <- linha1[IPCA2$col_num]
IPCA2$indicador <- linha2[IPCA2$col_num]
IPCA2 <- IPCA2 %>% select(municipio, data, indicador, var)
IPCA2 <- IPCA2[, -3]



####
#### 2006-2011 ####
####
# Ipca
IPCA3 <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/IPCA/06a11IPCA.xlsx")
IPCA3 <- IPCA3 %>% slice(-c(1, 2, n()))
dates <- as.character(IPCA3[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
IPCA3[1, ] <- t(dates)
colnames(IPCA3)[1] <- ""
IPCA3[1, 1] <- "data"
IPCA3[2, 1] <- "indicador"

linha1 <- as.character(unlist(IPCA3[1, ]))      
linha2 <- as.character(unlist(IPCA3[2, ]))     
IPCA3 <- IPCA3[-(1:2), ]
colnames(IPCA3) <- paste0("V", 0:(ncol(IPCA3)-1))  
colnames(IPCA3)[1] <- "municipio"
IPCA3 <- IPCA3 %>%
  pivot_longer(
    cols = -municipio,
    names_to = "col_id",
    values_to = "var"
  ) %>%
  mutate(
    var = as.numeric(var),
    col_num = as.numeric(gsub("V", "", col_id)) + 1  
  )
IPCA3$data <- linha1[IPCA3$col_num]
IPCA3$indicador <- linha2[IPCA3$col_num]
IPCA3 <- IPCA3 %>% select(municipio, data, indicador, var)
IPCA3 <- IPCA3[, -3]



####
#### 1999-2006 ####
####
# Ipca
IPCA4 <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/IPCA/99a06IPCA.xlsx")
IPCA4 <- IPCA4 %>% slice(-c(1, 2, n()))
dates <- as.character(IPCA4[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
IPCA4[1, ] <- t(dates)
colnames(IPCA4)[1] <- ""
IPCA4[1, 1] <- "data"
IPCA4[2, 1] <- "indicador"

linha1 <- as.character(unlist(IPCA4[1, ]))      
linha2 <- as.character(unlist(IPCA4[2, ]))     
IPCA4 <- IPCA4[-(1:2), ]
colnames(IPCA4) <- paste0("V", 0:(ncol(IPCA4)-1))  
colnames(IPCA4)[1] <- "municipio"
IPCA4 <- IPCA4 %>%
  pivot_longer(
    cols = -municipio,
    names_to = "col_id",
    values_to = "var"
  ) %>%
  mutate(
    var = as.numeric(var),
    col_num = as.numeric(gsub("V", "", col_id)) + 1  
  )
IPCA4$data <- linha1[IPCA4$col_num]
IPCA4$indicador <- linha2[IPCA4$col_num]
IPCA4 <- IPCA4 %>% select(municipio, data, indicador, var)
IPCA4 <- IPCA4[, -3]



####
#### 1991-1999 ####
####
# Ipca
IPCA5 <- read_excel("C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/IPCA/91a99IPCA.xlsx")
IPCA5 <- IPCA5 %>% slice(-c(1, 2, n()))
dates <- as.character(IPCA5[1, ])
dates <- na.locf(dates)
dates <- format(dmy(paste("01", dates)), "%Y-%m")
dates <- c(NA, dates)
IPCA5[1, ] <- t(dates)
colnames(IPCA5)[1] <- ""
IPCA5[1, 1] <- "data"
IPCA5[2, 1] <- "indicador"

linha1 <- as.character(unlist(IPCA5[1, ]))      
linha2 <- as.character(unlist(IPCA5[2, ]))     
IPCA5 <- IPCA5[-(1:2), ]
colnames(IPCA5) <- paste0("V", 0:(ncol(IPCA5)-1))  
colnames(IPCA5)[1] <- "municipio"
IPCA5 <- IPCA5 %>%
  pivot_longer(
    cols = -municipio,
    names_to = "col_id",
    values_to = "var"
  ) %>%
  mutate(
    var = as.numeric(var),
    col_num = as.numeric(gsub("V", "", col_id)) + 1  
  )
IPCA5$data <- linha1[IPCA5$col_num]
IPCA5$indicador <- linha2[IPCA5$col_num]
IPCA5 <- IPCA5 %>% select(municipio, data, indicador, var)
IPCA5 <- IPCA5[, -3]



####
#### Consolidação ####
####
# Construção
IPCA <- bind_rows(
  IPCA1,
  IPCA2,
  IPCA3,
  IPCA4,
  IPCA5
)
IPCA <- IPCA %>%
  complete(municipio, data)

colnames(IPCA)[3] <- "ipca"

# Plot
df_plot <- IPCA %>%
  filter(municipio == "São Paulo (SP)") %>%
  mutate(data_date = ym(data))

ggplot(df_plot, aes(x = data_date, y = ipca)) +
  geom_line(color = "#2c3e50", linewidth = 0.8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "IPCA - São Paulo (SP)", x = "", y = "IPCA") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.minor = element_blank()
  )

ggplot(df_plot %>% filter(data_date >= ymd("1996-01-01")), 
       aes(x = data_date, y = ipca)) +
  geom_line(color = "#27ae60", linewidth = 0.8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "IPCA - São Paulo (SP) - Após 1996", x = "", y = "IPCA") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.minor = element_blank()
  )

write_xlsx(IPCA, "C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados/IPCA.xlsx")
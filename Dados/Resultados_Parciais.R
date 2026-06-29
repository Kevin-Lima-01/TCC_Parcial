###############################################################################
# A INCLINAÇÃO DA CURVA DE PHILLIPS: EVIDÊNCIAS DOS MUNICÍPIOS BRASILEIROS
# Replicação adaptada de Hazell, Herreño, Nakamura & Steinsson (QJE, 2022)
#
# Estrutura do script:
#   0. Pacotes e parâmetros globais
#   1. Leitura e construção da base mensal (DES_M + INFLA_M -> BASE_M)
#   2. Variáveis derivadas (lags, períodos, soma descontada para κ)
#   3. Gráficos descritivos — caso São Paulo
#   4. Regressões principais: estimativas de ψ (curva "reduzida", eq. 19 do
#      paper) e κ (curva "estrutural" via soma descontada, eq. 17 do paper)
#   5. Achatamento da curva ao longo do tempo (sub-períodos, com/sem FE de tempo)
#   6. Curva de Phillips: scatterplots binados por sub-período (Figura V do paper)
#   7. Gráficos comparativos de coeficientes (BC, psi vs kappa, kappa por
#      variável, curva pré/pós 2015)
#   8. Regressão com desemprego_informalidade (margem alternativa de slack)
#   9. Robustez: transição de fonte de dados PME -> PNADC (abr/2016)
#  10. Exportação consolidada de tabelas e gráficos
###############################################################################


## ============================================================================
## 0. PACOTES E PARÂMETROS GLOBAIS
## ============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(ggplot2)
library(writexl)
library(data.table)
library(PNADcIBGE)
library(broom)
library(fixest)
library(patchwork)   # para unir múltiplos ggplots em um painel só
# (se não tiver instalado: install.packages("patchwork"))

# Caminhos de entrada/saída -- ajuste apenas aqui se mudar de máquina
PASTA_DADOS  <- "C:/Users/KEVIN/Desktop/Lições FGV/TCC/Dados"
PASTA_SAIDA  <- "C:/Users/KEVIN/Desktop/Lições FGV/TCC/Resultados"
if (!dir.exists(PASTA_SAIDA)) dir.create(PASTA_SAIDA, recursive = TRUE)

# Parâmetros estruturais usados nas estimativas de κ (equação 17 do paper)
BETA_DESCONTO <- 0.99   # fator de desconto trimestral do paper; mantido como
# aproximação mensal por simplicidade (ver Seção 4)
HORIZONTE_H   <- 24     # truncamento da soma descontada (meses)

# Data de corte da mudança de pesquisa de desemprego (PME -> PNADC contínua)
CORTE_FONTE <- ym("2016-04")


## ============================================================================
## 1. LEITURA E CONSTRUÇÃO DA BASE MENSAL
## ============================================================================
# DES_M: taxa de desemprego e informalidade por município-mês (PME até
#        mar/2016, PNADC contínua a partir de abr/2016)
# INFLA_M: inflação non-tradable (VNT_12m), tradable (VT_12m) e IPCA total
#        (VIPCA_12m) acumulada em 12 meses, por município-mês, além do nível
#        relativo de preços non-tradable (phat_N) usado como controle

DES_M <- read.csv(file.path(PASTA_DADOS, "Desemprego/DES_M.csv")) %>%
  mutate(
    across(c(taxa_desemprego, taxa_informalidade, desemprego_informalidade),
           as.numeric),
    data_date   = ym(data),
    fonte_pnadc = as.integer(data_date >= CORTE_FONTE)
  ) %>%
  select(-fonte)

INFLA_M <- read_excel(file.path(PASTA_DADOS, "Inflacao/infla_m.xlsx")) %>%
  filter(data >= "2002-03", data <= "2026-01")

# Junção das duas bases por município e mês. Mantemos data_date e
# fonte_pnadc calculados uma única vez aqui (evita recomputação duplicada
# que existia na versão anterior do script).
BASE_M <- DES_M %>%
  left_join(INFLA_M, by = c("municipio", "data")) %>%
  mutate(
    periodo = ifelse(year(data_date) < 2015, "2002-2014", "2015-2026")
  )

cat("Base construída:", nrow(BASE_M), "linhas |",
    n_distinct(BASE_M$municipio), "municípios |",
    "de", format(min(BASE_M$data_date)), "a", format(max(BASE_M$data_date)), "\n")


## ============================================================================
## 2. VARIÁVEIS DERIVADAS
## ============================================================================

# --- 2.1 Lags de 12 meses --------------------------------------------------
# Análogo ao lag de 4 trimestres (equação 19 do paper): usamos desemprego e
# preço relativo no início do período de 12 meses sobre o qual a inflação é
# acumulada, reduzindo problemas de simultaneidade.
BASE_M <- BASE_M %>%
  arrange(municipio, data_date) %>%
  group_by(municipio) %>%
  mutate(
    u_L12      = lag(taxa_desemprego, 12),
    u_inf_L12  = lag(desemprego_informalidade, 12),
    phat_N_L12 = lag(phat_N, 12)
  ) %>%
  ungroup()

# --- 2.2 Períodos de análise -------------------------------------------------
BASE_M <- BASE_M %>%
  mutate(
    # Sub-períodos finos usados na Tabela 3 (evolução de psi no tempo)
    periodo_fino = case_when(
      data_date <  ym("2014-01")                                ~ "2002-2013",
      data_date >= ym("2014-01") & data_date <= ym("2016-12")   ~ "2014-2016",
      data_date >  ym("2016-12") & data_date <  ym("2022-01")   ~ "2017-2021",
      data_date >= ym("2022-01")                                ~ "2022-2026"
    ),
    # Dummy do período Dilma / recessão (usada na Tabela 5)
    dilma = as.integer(data_date >= ym("2014-01") & data_date <= ym("2016-12"))
  )

# --- 2.3 Soma descontada para estimar kappa (equação 17 do paper) ----------
# Em vez do loop manual (lento e propenso a erro de índice fora do vetor),
# usamos um filtro de médias móveis ponderadas geometricamente com pesos
# beta^j, truncado em H meses à frente. Isso reproduz exatamente
# sum_{j=0}^{H} beta^j * x[t+j], mas de forma vetorizada e sem risco de
# estourar os limites do vetor perto do fim de cada painel de município.
soma_descontada_frente <- function(x, beta, H) {
  pesos <- beta^(0:H)                       # pesos beta^0, beta^1, ..., beta^H
  n     <- length(x)
  out   <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    fim <- i + H
    if (fim > n) next                       # não há H meses futuros: fica NA
    janela <- x[i:fim]
    if (anyNA(janela)) next                 # qualquer buraco invalida a soma
    out[i] <- sum(pesos * janela)
  }
  out
}

BASE_M <- BASE_M %>%
  arrange(municipio, data_date) %>%
  group_by(municipio) %>%
  mutate(
    soma_u    = soma_descontada_frente(taxa_desemprego, BETA_DESCONTO, HORIZONTE_H),
    soma_phat = soma_descontada_frente(phat_N,           BETA_DESCONTO, HORIZONTE_H)
  ) %>%
  ungroup()


## ============================================================================
## 3. GRÁFICOS DESCRITIVOS — CASO SÃO PAULO
## ============================================================================
# Dois gráficos complementares:
#   (a) dispersão desemprego x inflação, por tipo de bem (o que já existia)
#   (b) série temporal com desemprego, desemprego_informalidade e as três
#       medidas de inflação no mesmo painel, para visualizar comovimento e
#       a transição de fonte de dados ao longo do tempo

sp_dados <- BASE_M %>%
  filter(municipio == "São Paulo (SP)")

# --- 3.1 Dispersão desemprego x inflação (por tipo de bem) ------------------
sp_long_disp <- sp_dados %>%
  filter(!is.na(taxa_desemprego)) %>%
  select(data_date, taxa_desemprego, VNT_12m, VT_12m, VIPCA_12m) %>%
  pivot_longer(c(VNT_12m, VT_12m, VIPCA_12m),
               names_to  = "serie", values_to = "inflacao") %>%
  mutate(serie = recode(serie,
                        VNT_12m   = "Non-Tradable",
                        VT_12m    = "Tradable",
                        VIPCA_12m = "IPCA Total"))

fig_sp_dispersao <- ggplot(sp_long_disp, aes(x = taxa_desemprego, y = inflacao, color = serie)) +
  geom_point(size = 1.2, alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +
  facet_wrap(~ serie, scales = "free_y") +
  labs(
    title    = "São Paulo — Desemprego vs. Inflação (2002–2026)",
    subtitle = "Variação acumulada 12 meses, dados mensais",
    x = "Taxa de Desemprego (%)", y = "Inflação (%)",
    caption  = "Fonte: PME+PNADC/IBGE e IPCA/IBGE"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position  = "none",
        plot.title       = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle    = element_text(hjust = 0.5))

print(fig_sp_dispersao)

# --- 3.2 Série temporal unificada: desemprego (formal + informal) e --------
#         as três medidas de inflação, todas no mesmo eixo do tempo.
# Como desemprego (%) e inflação (%) já estão em escalas comparáveis,
# plotamos tudo em painéis empilhados (facet) compartilhando o eixo x, o
# que evita o problema de eixo y duplo (sempre enganoso em ggplot) e ainda
# permite comparação visual direta entre as séries.
sp_long_serie <- sp_dados %>%
  select(data_date, fonte_pnadc, taxa_desemprego, desemprego_informalidade,
         VNT_12m, VT_12m, VIPCA_12m) %>%
  pivot_longer(
    cols      = c(taxa_desemprego, desemprego_informalidade, VNT_12m, VT_12m, VIPCA_12m),
    names_to  = "serie", values_to = "valor"
  ) %>%
  mutate(
    grupo = case_when(
      serie %in% c("taxa_desemprego", "desemprego_informalidade") ~ "Desemprego",
      TRUE                                                        ~ "Inflação"
    ),
    serie = recode(serie,
                   taxa_desemprego          = "Desemprego (total)",
                   desemprego_informalidade = "Desemprego (informal)",
                   VNT_12m                  = "Inflação Non-Tradable",
                   VT_12m                   = "Inflação Tradable",
                   VIPCA_12m                = "IPCA Total")
  )

fig_sp_serie <- ggplot(sp_long_serie, aes(x = data_date, y = valor, color = serie)) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  geom_vline(xintercept = as.numeric(CORTE_FONTE), linetype = "dashed",
             color = "gray50", linewidth = 0.4) +
  facet_wrap(~ grupo, ncol = 1, scales = "free_y") +
  labs(
    title    = "São Paulo — Desemprego e Inflação ao Longo do Tempo",
    subtitle = "Linha pontilhada cinza = transição PME → PNADC contínua (abr/2016)",
    x = "Data", y = "%", color = NULL,
    caption  = "Fonte: PME+PNADC/IBGE e IPCA/IBGE"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 9),
        legend.position = "bottom")

print(fig_sp_serie)


## ============================================================================
## 4. REGRESSÕES PRINCIPAIS
## ============================================================================
# Notação (consistente com o paper):
#   psi_*   -> especificação "reduzida" (eq. 19): usa desemprego corrente ou
#              defasado diretamente como regressor. Mistura efeito direto do
#              desemprego com sua persistência, por isso psi >= kappa.
#   kappa_* -> especificação "estrutural" (eq. 17): usa a soma descontada de
#              desemprego futuro esperado, isolando o parâmetro estrutural
#              da curva de Phillips (o que o modelo do paper chama de kappa).
#
# Em todas, incluímos FE de município (absorve nível médio de cada local) e
# FE de mês-ano (absorve expectativas de inflação de longo prazo, comuns a
# todos os municípios em um dado mês -- o ponto central do paper).
#
# fonte_pnadc NÃO entra nas especificações com FE de tempo: como a virada de
# pesquisa (abr/2016) ocorre no mesmo mês para todos os municípios, ela é
# absorvida integralmente pelo FE de tempo (perfeitamente colinear). Mantê-la
# nessas especificações não muda nada além de gerar o aviso de colinearidade
# do fixest. Ela é útil apenas em especificações SEM FE de tempo (Seção 5).

# --- 4.1 Estimativas de psi (amostra completa) ------------------------------
psi_nt   <- feols(VNT_12m   ~ taxa_desemprego + phat_N | municipio + data,
                  data = BASE_M, cluster = ~municipio)
psi_ipca <- feols(VIPCA_12m ~ taxa_desemprego          | municipio + data,
                  data = BASE_M, cluster = ~municipio)
psi_t    <- feols(VT_12m    ~ taxa_desemprego          | municipio + data,
                  data = BASE_M, cluster = ~municipio)

etable(psi_nt, psi_ipca, psi_t,
       headers = c("Non-Tradable", "IPCA Total", "Tradable"),
       title   = "Tabela 1 — Estimativas de psi (amostra completa, mensal)",
       digits  = 4,
       notes   = "Erros padrão clusterizados por município. FE de município e mês-ano.")

# --- 4.2 Estimativas de kappa via soma descontada (amostra completa) -------
kappa_nt   <- feols(VNT_12m   ~ soma_u + soma_phat | municipio + data,
                    data = BASE_M, cluster = ~municipio)
kappa_ipca <- feols(VIPCA_12m ~ soma_u              | municipio + data,
                    data = BASE_M, cluster = ~municipio)
kappa_t    <- feols(VT_12m    ~ soma_u              | municipio + data,
                    data = BASE_M, cluster = ~municipio)

etable(kappa_nt, kappa_ipca, kappa_t,
       headers = c("Non-Tradable", "IPCA Total", "Tradable"),
       title   = paste0("Tabela 2 — Estimativa de kappa via Soma Descontada ",
                        "(beta=", BETA_DESCONTO, ", H=", HORIZONTE_H, ")"),
       digits  = 4,
       notes   = "Análogo à equação (17) do paper. kappa é o parâmetro estrutural da curva;
                  psi (demais tabelas) incorpora também a persistência do desemprego.")

# --- 4.3 Robustez: especificação contemporânea vs. lag de 12 meses ---------
psi_nt_lag   <- feols(VNT_12m   ~ u_L12 + phat_N_L12 | municipio + data,
                      data = BASE_M, cluster = ~municipio)
psi_ipca_lag <- feols(VIPCA_12m ~ u_L12              | municipio + data,
                      data = BASE_M, cluster = ~municipio)
psi_t_lag    <- feols(VT_12m    ~ u_L12              | municipio + data,
                      data = BASE_M, cluster = ~municipio)

etable(psi_nt, psi_nt_lag, psi_ipca, psi_ipca_lag, psi_t, psi_t_lag,
       headers = c("NT contemp.", "NT lag-12", "IPCA contemp.", "IPCA lag-12",
                   "T contemp.", "T lag-12"),
       title   = "Tabela 3 — Robustez: Contemporâneo vs. Lag de 12 Meses",
       digits  = 4,
       notes   = "Lag de 12 meses análogo ao lag de 4 trimestres da equação (19) do paper.")


## ============================================================================
## 5. ACHATAMENTO DA CURVA AO LONGO DO TEMPO
## ============================================================================
# Replica a lógica central do paper (Tabela II e Figura V): comparar a
# inclinação estimada COM e SEM efeitos fixos de tempo. Sem FE de tempo, a
# inclinação capta tanto o efeito real do desemprego quanto a covariância
# espúria com mudanças em expectativas de inflação de longo prazo -- por
# isso tende a aparecer mais "achatada" no tempo recente mesmo que o
# parâmetro estrutural não tenha mudado.

# --- 5.1 Dois grandes sub-períodos (pré/pós 2015) ---------------------------
psi_pre_sem_fte <- feols(VNT_12m ~ taxa_desemprego + phat_N | municipio,
                         data = filter(BASE_M, periodo == "2002-2014"),
                         cluster = ~municipio)
psi_pos_sem_fte <- feols(VNT_12m ~ taxa_desemprego + phat_N | municipio,
                         data = filter(BASE_M, periodo == "2015-2026"),
                         cluster = ~municipio)
psi_pre_com_fte <- feols(VNT_12m ~ taxa_desemprego + phat_N | municipio + data,
                         data = filter(BASE_M, periodo == "2002-2014"),
                         cluster = ~municipio)
psi_pos_com_fte <- feols(VNT_12m ~ taxa_desemprego + phat_N | municipio + data,
                         data = filter(BASE_M, periodo == "2015-2026"),
                         cluster = ~municipio)

etable(psi_pre_sem_fte, psi_pos_sem_fte, psi_pre_com_fte, psi_pos_com_fte,
       headers = c("Pré (sem gama_t)", "Pós (sem gama_t)",
                   "Pré (com gama_t)", "Pós (com gama_t)"),
       title   = "Tabela 4 — Achatamento da Curva de Phillips (Non-Tradable, mensal)",
       digits  = 4,
       notes   = "Contraste entre colunas 1-2 e 3-4 isola o efeito de expectativas de inflação.")

# --- 5.2 Sub-períodos finos, sempre com FE de tempo --------------------------
rodar_sub <- function(periodo_val) {
  feols(VNT_12m ~ taxa_desemprego + phat_N | municipio + data,
        data    = filter(BASE_M, periodo_fino == periodo_val),
        cluster = ~municipio)
}
psi_p1 <- rodar_sub("2002-2013")
psi_p2 <- rodar_sub("2014-2016")
psi_p3 <- rodar_sub("2017-2021")
psi_p4 <- rodar_sub("2022-2026")

etable(psi_p1, psi_p2, psi_p3, psi_p4,
       headers = c("2002-2013", "2014-2016 (Dilma)",
                   "2017-2021 (pos-imp.)", "2022-2026 (BC indep.)"),
       title   = "Tabela 5 — Evolução de psi por Sub-período (com gama_t)",
       digits  = 4,
       notes   = "Variável dependente: inflação NT acumulada 12 meses.
                  FE de município e mês-ano. Cluster por município.")

# --- 5.3 Independência formal do Banco Central (jan/2022) ------------------
psi_pre_bc_sem <- feols(VNT_12m ~ taxa_desemprego + phat_N | municipio,
                        data = filter(BASE_M, data_date < ym("2022-01")), cluster = ~municipio)
psi_pos_bc_sem <- feols(VNT_12m ~ taxa_desemprego + phat_N | municipio,
                        data = filter(BASE_M, data_date >= ym("2022-01")), cluster = ~municipio)
psi_pre_bc_com <- feols(VNT_12m ~ taxa_desemprego + phat_N | municipio + data,
                        data = filter(BASE_M, data_date < ym("2022-01")), cluster = ~municipio)
psi_pos_bc_com <- feols(VNT_12m ~ taxa_desemprego + phat_N | municipio + data,
                        data = filter(BASE_M, data_date >= ym("2022-01")), cluster = ~municipio)

etable(psi_pre_bc_sem, psi_pos_bc_sem, psi_pre_bc_com, psi_pos_bc_com,
       headers = c("Pré-BC (sem gama_t)", "Pós-BC (sem gama_t)",
                   "Pré-BC (com gama_t)", "Pós-BC (com gama_t)"),
       title   = "Tabela 6 — Independência do BC e Inclinação da Curva de Phillips",
       digits  = 4,
       notes   = "Marco: janeiro/2022. Contraste sem/com gama_t separa ancoragem de
                  expectativas de mudança estrutural na curva.")

# --- 5.4 Efeito diferencial do período Dilma/recessão (2014-2016) ----------
psi_dilma <- feols(VNT_12m ~ taxa_desemprego + taxa_desemprego:dilma + phat_N
                   | municipio + data,
                   data = BASE_M, cluster = ~municipio)

etable(psi_nt, psi_dilma,
       headers = c("Baseline", "Com interação Dilma"),
       title   = "Tabela 7 — Efeito Diferencial da Recessão Dilma (2014-2016)",
       digits  = 4,
       notes   = "Interação taxa_desemprego x dilma captura desvio da sensibilidade
                  durante 2014-2016 em relação à média amostral.")


## ============================================================================
## 6. CURVA DE PHILLIPS: SCATTERPLOTS BINADOS POR SUB-PERÍODO (Fig. V do paper)
## ============================================================================
# Lógica (idêntica à Figura V de Hazell et al. 2022):
#   1. Residualizar inflação e desemprego (defasados 12m) contra phat_N e FE,
#      isolando a variação "limpa" de outros controles.
#   2. Agrupar os resíduos em 20 bins (vintiles) de desemprego, dentro de
#      cada período, e plotar a média de cada bin.
#   3. Comparar o painel SEM FE de tempo (inclui variação de expectativas)
#      com o painel COM FE de tempo (variação regional pura) -- mostrando
#      visualmente por que a curva "parece" mais achatada na primeira
#      especificação.
#
# Generalizamos para os 4 sub-períodos finos (não apenas 2), o que permite
# ver a evolução completa da inclinação ao longo do tempo, não só um
# contraste binário pré/pós.

base_res <- BASE_M %>%
  filter(!is.na(u_L12), !is.na(phat_N_L12), !is.na(VNT_12m), !is.na(periodo_fino))

base_res <- base_res %>%
  mutate(
    res_inf_sem = residuals(feols(VNT_12m ~ phat_N_L12 | municipio,        data = base_res)),
    res_u_sem   = residuals(feols(u_L12   ~ phat_N_L12 | municipio,        data = base_res)),
    res_inf_com = residuals(feols(VNT_12m ~ phat_N_L12 | municipio + data, data = base_res)),
    res_u_com   = residuals(feols(u_L12   ~ phat_N_L12 | municipio + data, data = base_res))
  )

# Paleta com 4 cores, uma por sub-período fino (mais informativa do que
# apenas 2 tons de cinza/azul-laranja da versão anterior)
paleta_periodos <- c(
  "2002-2013" = "#3B4252",
  "2014-2016" = "#BF616A",
  "2017-2021" = "#5E81AC",
  "2022-2026" = "#A3BE8C"
)

scatter_binado <- function(df, x_var, y_var, titulo, paleta) {
  df %>%
    group_by(periodo_fino) %>%
    mutate(bin = ntile(.data[[x_var]], 20)) %>%
    ungroup() %>%
    group_by(bin, periodo_fino) %>%
    summarise(x = mean(.data[[x_var]], na.rm = TRUE),
              y = mean(.data[[y_var]], na.rm = TRUE),
              .groups = "drop") %>%
    ggplot(aes(x = x, y = y, color = periodo_fino)) +
    geom_point(size = 2.3, alpha = 0.85) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +
    scale_color_manual(values = paleta) +
    labs(title = titulo,
         x = "Desemprego defasado 12m (resíduo, p.p.)",
         y = "Inflação NT (resíduo, p.p.)",
         color = "Período") +
    theme_minimal(base_size = 11) +
    theme(plot.title      = element_text(hjust = 0.5, face = "bold", size = 10),
          legend.position = "bottom")
}

fig_sem <- scatter_binado(base_res, "res_u_sem", "res_inf_sem",
                          "Sem FE de Tempo\n(inclui variação de expectativas)",
                          paleta_periodos)
fig_com <- scatter_binado(base_res, "res_u_com", "res_inf_com",
                          "Com FE de Tempo\n(variação regional pura)",
                          paleta_periodos)

# Painel único lado a lado (equivalente à Figura V do paper, com legenda
# compartilhada via patchwork)
fig_curva_phillips_periodos <- (fig_sem | fig_com) +
  plot_annotation(
    title = "Curva de Phillips Regional por Sub-período",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  )

print(fig_curva_phillips_periodos)


## ============================================================================
## 7. GRÁFICOS COMPARATIVOS DE COEFICIENTES
## ============================================================================
# Quatro gráficos comparativos. Todos traçam a RETA implícita em cada
# inclinação estimada -- y = média(y) + inclinação*(x - média(x)) -- sem
# intervalo de confiança, focando apenas na comparação visual da inclinação
# entre retas (a comparação que o paper faz na Figura V). Todos usam as
# funções auxiliares construir_retas() e grafico_retas(), definidas abaixo.
#
# Em 7.1 (BC) e 7.4 (pré/pós 2015), focamos apenas na especificação com
# efeito fixo de tempo (gama_t) -- a que isola a variação regional "limpa"
# de mudanças em expectativas de inflação de longo prazo.
#
# Em 7.2 (psi vs. kappa) e 7.3 (kappa por variável dependente), as retas
# comparadas vêm de regressores e/ou variáveis dependentes diferentes; por
# isso construir_retas() permite informar uma variável independente e uma
# variável dependente por modelo (ver parâmetros variaveis e var_y), além
# de padronizar o eixo x em desvios-padrão quando os regressores estão em
# escalas diferentes (psi vs. kappa, em 7.2 -- não necessário em 7.3, onde
# o regressor soma_u é o mesmo nas três retas).
#
# --- Funções auxiliares para gráficos de RETAS ------------------------------
construir_retas <- function(modelos, nomes, variaveis, dados_lista, var_y,
                            padronizar = FALSE, n_grid = 100) {
  # var_y pode ser uma única string (reciclada para todos os modelos) ou um
  # vetor com uma variável dependente por modelo -- necessário quando os
  # modelos comparados têm variáveis dependentes diferentes (ex.: kappa_nt,
  # kappa_ipca e kappa_t, que usam VNT_12m, VIPCA_12m e VT_12m respectivamente).
  if (length(var_y) == 1) var_y <- rep(var_y, length(modelos))
  
  saida <- vector("list", length(modelos))
  for (k in seq_along(modelos)) {
    var_k      <- variaveis[k]
    y_k        <- var_y[k]
    coef_linha <- tidy(modelos[[k]]) %>% filter(term == var_k)
    df_k       <- dados_lista[[k]]
    x_orig     <- df_k[[var_k]]
    y_obs      <- df_k[[y_k]]
    
    mean_x <- mean(x_orig, na.rm = TRUE)
    mean_y <- mean(y_obs,  na.rm = TRUE)
    
    if (padronizar) {
      sd_x     <- sd(x_orig, na.rm = TRUE)
      slope    <- coef_linha$estimate * sd_x   # efeito por 1 DP do regressor
      x_plot   <- (x_orig - mean_x) / sd_x     # regressor em unidades de DP
      x_centro <- 0
    } else {
      slope    <- coef_linha$estimate
      x_plot   <- x_orig
      x_centro <- mean_x
    }
    
    x_seq <- seq(min(x_plot, na.rm = TRUE), max(x_plot, na.rm = TRUE),
                 length.out = n_grid)
    
    saida[[k]] <- data.frame(
      modelo = nomes[k],
      x      = x_seq,
      y      = mean_y + slope * (x_seq - x_centro)
    )
  }
  bind_rows(saida) %>%
    mutate(modelo = factor(modelo, levels = nomes))
}

# Template visual comum aos gráficos de retas: apenas a reta (geom_line),
# sem faixa de IC.
grafico_retas <- function(df_retas, paleta, titulo, subtitulo, x_lab, y_lab) {
  ggplot(df_retas, aes(x = x, y = y, color = modelo)) +
    geom_line(linewidth = 1.1) +
    scale_color_manual(values = paleta) +
    labs(title = titulo, subtitle = subtitulo, x = x_lab, y = y_lab, color = NULL) +
    theme_minimal(base_size = 11) +
    theme(plot.title      = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle   = element_text(hjust = 0.5, size = 9),
          legend.position = "bottom")
}

# --- 7.1 Independência do BC: retas pré/pós, apenas especificação com FE -
# Conforme pedido: foca só no caso com efeito fixo de tempo (psi_pre_bc_com
# e psi_pos_bc_com, Seção 5.3 -- a especificação que de fato isola a
# inclinação "limpa" de mudanças em expectativas de inflação de longo
# prazo) e traça a reta implícita em cada inclinação estimada, em vez do
# ponto + IC. Marco: independência formal do BC em janeiro/2022.
dados_bc_pre <- BASE_M %>%
  filter(data_date < ym("2022-01")) %>%
  filter(!is.na(taxa_desemprego), !is.na(phat_N), !is.na(VNT_12m))

dados_bc_pos <- BASE_M %>%
  filter(data_date >= ym("2022-01")) %>%
  filter(!is.na(taxa_desemprego), !is.na(phat_N), !is.na(VNT_12m))

retas_bc <- construir_retas(
  modelos     = list(psi_pre_bc_com, psi_pos_bc_com),
  nomes       = c("Pré-BC (até dez/2021)", "Pós-BC (a partir de jan/2022)"),
  variaveis   = c("taxa_desemprego", "taxa_desemprego"),
  dados_lista = list(dados_bc_pre, dados_bc_pos),
  var_y       = "VNT_12m",
  padronizar  = FALSE
)

fig_comp_bc <- grafico_retas(
  retas_bc,
  paleta    = c("Pré-BC (até dez/2021)"            = "#5E81AC",
                "Pós-BC (a partir de jan/2022)"     = "#BF616A"),
  titulo    = "Independência do Banco Central — Inclinação da Curva de Phillips",
  subtitulo = "Apenas especificação com FE de município e mês-ano (gama_t).",
  x_lab     = "Taxa de desemprego (%)",
  y_lab     = "Inflação Non-Tradable acumulada 12m (%)"
)

print(fig_comp_bc)

# --- 7.2 psi vs. kappa, apenas Non-Tradable: duas retas ---------------------
# Conforme pedido: foca só em NT (psi_nt e kappa_nt) e traça as duas retas,
# uma por especificação. Como os regressores estão em escalas diferentes
# (taxa_desemprego, em p.p., vs. soma_u, soma descontada multi-período),
# padronizamos o eixo x em desvios-padrão do regressor -- sem isso as duas
# retas não seriam comparáveis no mesmo gráfico (mesma lógica de
# normalização que já existia aqui, agora aplicada à reta em vez do ponto).
dados_psi_nt <- BASE_M %>%
  filter(!is.na(taxa_desemprego), !is.na(phat_N), !is.na(VNT_12m))

dados_kappa_nt <- BASE_M %>%
  filter(!is.na(soma_u), !is.na(soma_phat), !is.na(VNT_12m))

retas_psi_kappa <- construir_retas(
  modelos     = list(psi_nt, kappa_nt),
  nomes       = c("psi (desemprego corrente)", "kappa (soma descontada)"),
  variaveis   = c("taxa_desemprego", "soma_u"),
  dados_lista = list(dados_psi_nt, dados_kappa_nt),
  var_y       = "VNT_12m",
  padronizar  = TRUE
)

fig_comp_psi_kappa <- grafico_retas(
  retas_psi_kappa,
  paleta    = c("psi (desemprego corrente)" = "#D08770",
                "kappa (soma descontada)"   = "#88C0D0"),
  titulo    = "psi vs. kappa — Inclinação da Curva de Phillips (Non-Tradable)",
  subtitulo = "Eixo x padronizado (desvios-padrão do regressor) para tornar as duas especificações comparáveis.",
  x_lab     = "Regressor de desemprego (desvios-padrão)",
  y_lab     = "Inflação Non-Tradable acumulada 12m (%)"
)

print(fig_comp_psi_kappa)

# --- 7.3 kappa por variável dependente: NT vs. IPCA Total vs. Tradable -----
# Conforme pedido: as três retas (NT, IPCA Total, Tradable) juntas no mesmo
# gráfico. Os três modelos (kappa_nt, kappa_ipca, kappa_t, Seção 4.2) usam
# o MESMO regressor (soma_u), então não é preciso padronizar o eixo x aqui
# -- diferente do gráfico 7.2, onde psi e kappa vêm de regressores em
# escalas diferentes. O que varia entre as três retas é a variável
# dependente (NT, IPCA Total ou Tradable), o que testa diretamente a
# previsão do modelo do paper de que o IPCA total -- por incluir bens
# tradeable, precificados nacionalmente -- deve ter uma inclinação menor
# (mais próxima de zero, reta mais "achatada") do que a non-tradable.
dados_kappa_ipca <- BASE_M %>%
  filter(!is.na(soma_u), !is.na(VIPCA_12m))

dados_kappa_t <- BASE_M %>%
  filter(!is.na(soma_u), !is.na(VT_12m))

retas_kappa_var <- construir_retas(
  modelos     = list(kappa_nt, kappa_ipca, kappa_t),
  nomes       = c("Non-Tradable", "IPCA Total", "Tradable"),
  variaveis   = rep("soma_u", 3),
  dados_lista = list(dados_kappa_nt, dados_kappa_ipca, dados_kappa_t),
  var_y       = c("VNT_12m", "VIPCA_12m", "VT_12m"),
  padronizar  = FALSE
)

fig_comp_kappa_var <- grafico_retas(
  retas_kappa_var,
  paleta    = c("Non-Tradable" = "#3B4252", "IPCA Total" = "#5E81AC", "Tradable" = "#A3BE8C"),
  titulo    = "kappa via Soma Descontada — NT vs. IPCA Total vs. Tradable",
  subtitulo = paste0("beta=", BETA_DESCONTO, ", H=", HORIZONTE_H, " meses."),
  x_lab     = "Soma descontada de desemprego futuro (soma_u)",
  y_lab     = "Inflação acumulada 12m (%)"
)

print(fig_comp_kappa_var)

# --- 7.4 Curva de Phillips pré e pós 2015: retas, apenas com FE de tempo --
# Conforme pedido: foca só na especificação com efeito fixo de tempo
# (psi_pre_com_fte e psi_pos_com_fte, Seção 5.1) e traça a reta implícita
# em cada inclinação, comparando os dois sub-períodos diretamente.
dados_periodo_pre <- BASE_M %>%
  filter(periodo == "2002-2014") %>%
  filter(!is.na(taxa_desemprego), !is.na(phat_N), !is.na(VNT_12m))

dados_periodo_pos <- BASE_M %>%
  filter(periodo == "2015-2026") %>%
  filter(!is.na(taxa_desemprego), !is.na(phat_N), !is.na(VNT_12m))

retas_pre_pos <- construir_retas(
  modelos     = list(psi_pre_com_fte, psi_pos_com_fte),
  nomes       = c("2002-2014", "2015-2026"),
  variaveis   = c("taxa_desemprego", "taxa_desemprego"),
  dados_lista = list(dados_periodo_pre, dados_periodo_pos),
  var_y       = "VNT_12m",
  padronizar  = FALSE
)

fig_comp_pre_pos <- grafico_retas(
  retas_pre_pos,
  paleta    = c("2002-2014" = "#3B4252", "2015-2026" = "#A3BE8C"),
  titulo    = "Curva de Phillips Pré e Pós 2015 — Inclinação Estimada",
  subtitulo = "Apenas especificação com FE de município e mês-ano (gama_t).",
  x_lab     = "Taxa de desemprego (%)",
  y_lab     = "Inflação Non-Tradable acumulada 12m (%)"
)

print(fig_comp_pre_pos)


## ============================================================================
## 8. REGRESSÃO COM DESEMPREGO_INFORMALIDADE
## ============================================================================
# Motivação: a taxa de desemprego "aberta" (taxa_desemprego) não captura
# folga no mercado de trabalho que se manifesta via informalidade em vez de
# desemprego declarado -- relevante no Brasil, onde uma parcela grande da
# força de trabalho migra para a informalidade em recessões em vez de ficar
# desempregada. desemprego_informalidade funciona como uma medida alternativa
# (mais ampla) de slack. Comparamos psi estimado com cada medida, separada e
# conjuntamente, contemporâneo e defasado 12m.

psi_inf_contemp <- feols(VNT_12m ~ desemprego_informalidade + phat_N | municipio + data,
                         data = BASE_M, cluster = ~municipio)

psi_inf_lag <- feols(VNT_12m ~ u_inf_L12 + phat_N_L12 | municipio + data,
                     data = BASE_M, cluster = ~municipio)

# Especificação conjunta: desemprego aberto E informalidade ao mesmo tempo,
# para ver se a informalidade adiciona poder explicativo além do desemprego
# aberto (ou se as duas são redundantes/colineares).
psi_conjunta <- feols(VNT_12m ~ taxa_desemprego + desemprego_informalidade + phat_N
                      | municipio + data,
                      data = BASE_M, cluster = ~municipio)

etable(psi_nt, psi_inf_contemp, psi_inf_lag, psi_conjunta,
       headers = c("Desemp. aberto", "Desemp. informalidade",
                   "Informalidade lag-12", "Conjunta (aberto + informal)"),
       title   = "Tabela 8 — Curva de Phillips com Desemprego + Informalidade",
       digits  = 4,
       notes   = "desemprego_informalidade soma desempregados abertos e trabalhadores
                  informais, capturando uma noção mais ampla de folga no mercado de
                  trabalho. Especificação 'Conjunta' testa se a informalidade adiciona
                  informação além do desemprego aberto.")


## ============================================================================
## 9. ROBUSTEZ: TRANSIÇÃO DE FONTE DE DADOS (PME -> PNADC, abr/2016)
## ============================================================================
# Pergunta direta: a mudança de pesquisa de desemprego distorce a inclinação
# estimada da curva de Phillips?
#
# Por que fonte_pnadc nunca aparece nas regressões com FE de tempo (Seções
# 4-7): a transição ocorre no mesmo mês para TODOS os municípios -- não há
# sobreposição temporal entre PME e PNADC na base. Logo, fonte_pnadc é uma
# função exata do FE de tempo (perfeitamente colinear) e é descartada
# automaticamente pelo feols. Isso não é erro: confirmamos abaixo.
#
# A pergunta que importa -- e que a dummy aditiva NUNCA poderia responder,
# colinear ou não -- é se a INCLINAÇÃO (não o nível) da curva muda na
# transição. Para isso, dividimos a amostra nas duas pesquisas e comparamos
# psi estimado dentro de cada uma.

# --- 8.1 Confirmação do corte seco (sem sobreposição de fontes) ------------
n_fontes_por_mes <- BASE_M %>%
  distinct(data_date, fonte_pnadc) %>%
  count(data_date) %>%
  summarise(max_fontes_em_um_mes = max(n))

cat("Maior número de fontes distintas em um único mês:",
    n_fontes_por_mes$max_fontes_em_um_mes,
    "(1 = corte seco, sem sobreposição PME/PNADC)\n")

# --- 8.2 psi estimado separadamente em cada fonte (split-sample) -----------
# Equivalente direto da Tabela II do paper (pré/pós-1990): em vez de
# misturar tudo com uma dummy redundante, estimamos a inclinação inteira
# dentro de cada pesquisa.
psi_pme   <- feols(VNT_12m ~ taxa_desemprego + phat_N | municipio + data,
                   data = filter(BASE_M, fonte_pnadc == 0), cluster = ~municipio)
psi_pnadc <- feols(VNT_12m ~ taxa_desemprego + phat_N | municipio + data,
                   data = filter(BASE_M, fonte_pnadc == 1), cluster = ~municipio)

etable(psi_pme, psi_pnadc,
       headers = c("Somente PME (até mar/2016)", "Somente PNADC (a partir de abr/2016)"),
       title   = "Tabela 9 — psi Estimado Separadamente por Fonte de Dados",
       digits  = 4,
       notes   = "Ambas as colunas incluem FE de município e mês-ano. Coeficientes
                  similares entre as colunas indicam que a mudança de pesquisa não
                  distorce a relação estimada entre desemprego e inflação.")

# --- 8.3 Teste formal de quebra na inclinação (interação) -------------------
# A interação taxa_desemprego:fonte_pnadc SOBREVIVE ao FE de tempo porque
# varia também entre municípios dentro do mesmo mês (desemprego não é igual
# para todos os municípios), diferente da dummy pura fonte_pnadc (que é
# igual para todos os municípios em um dado mês, e por isso morre).
psi_quebra_fonte <- feols(VNT_12m ~ taxa_desemprego + taxa_desemprego:fonte_pnadc + phat_N
                          | municipio + data,
                          data = BASE_M, cluster = ~municipio)

etable(psi_nt, psi_quebra_fonte,
       headers = c("Baseline (pooled)", "Com interação Fonte"),
       title   = "Tabela 10 — Teste de Quebra na Transição PME -> PNADC",
       digits  = 4,
       notes   = "Coeficiente em taxa_desemprego:fonte_pnadc mede o desvio da inclinação
                  no período PNADC relativo ao período PME. Não significativo = a
                  mudança de pesquisa não distorce psi.")

# --- 8.4 Gráfico simples e direto: nível médio de desemprego em torno do corte
fig_continuidade_fonte <- BASE_M %>%
  filter(!is.na(taxa_desemprego)) %>%
  group_by(data_date, fonte_pnadc) %>%
  summarise(media_u = mean(taxa_desemprego, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = data_date, y = media_u, color = factor(fonte_pnadc))) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = as.numeric(CORTE_FONTE), linetype = "dashed", color = "red") +
  scale_color_manual(values = c("0" = "gray40", "1" = "steelblue"),
                     labels = c("PME", "PNADC")) +
  labs(title    = "Taxa de Desemprego Média — PME vs. PNADC",
       subtitle = "Linha vermelha = início da PNADC contínua (abr/2016)",
       x = "Data", y = "Taxa de Desemprego (%)", color = "Fonte") +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 9))

print(fig_continuidade_fonte)


## ============================================================================
## 10. EXPORTAÇÃO CONSOLIDADA
## ============================================================================
# Reúne todas as tabelas de regressão em um único arquivo de texto (mais
# fácil de revisar/colar no TCC do que abrir 10 outputs separados no
# console) e salva todos os gráficos em um único PDF multi-página.

# --- 10.1 Tabelas consolidadas em um único .txt -----------------------------
sink(file.path(PASTA_SAIDA, "Tabelas_Consolidadas.txt"))

etable(psi_nt, psi_ipca, psi_t,
       headers = c("Non-Tradable", "IPCA Total", "Tradable"),
       title = "Tabela 1 — psi (amostra completa)", digits = 4)

etable(kappa_nt, kappa_ipca, kappa_t,
       headers = c("Non-Tradable", "IPCA Total", "Tradable"),
       title = "Tabela 2 — kappa via soma descontada", digits = 4)

etable(psi_nt, psi_nt_lag, psi_ipca, psi_ipca_lag, psi_t, psi_t_lag,
       headers = c("NT contemp.", "NT lag-12", "IPCA contemp.", "IPCA lag-12",
                   "T contemp.", "T lag-12"),
       title = "Tabela 3 — Contemporâneo vs. lag-12", digits = 4)

etable(psi_pre_sem_fte, psi_pos_sem_fte, psi_pre_com_fte, psi_pos_com_fte,
       headers = c("Pré sem FE-t", "Pós sem FE-t", "Pré com FE-t", "Pós com FE-t"),
       title = "Tabela 4 — Achatamento pré/pós 2015", digits = 4)

etable(psi_p1, psi_p2, psi_p3, psi_p4,
       headers = c("2002-2013", "2014-2016", "2017-2021", "2022-2026"),
       title = "Tabela 5 — Evolução por sub-período fino", digits = 4)

etable(psi_pre_bc_sem, psi_pos_bc_sem, psi_pre_bc_com, psi_pos_bc_com,
       headers = c("Pré-BC sem FE-t", "Pós-BC sem FE-t", "Pré-BC com FE-t", "Pós-BC com FE-t"),
       title = "Tabela 6 — Independência do BC (jan/2022)", digits = 4)

etable(psi_nt, psi_dilma,
       headers = c("Baseline", "Interação Dilma"),
       title = "Tabela 7 — Efeito diferencial Dilma", digits = 4)

etable(psi_nt, psi_inf_contemp, psi_inf_lag, psi_conjunta,
       headers = c("Desemp. aberto", "Informalidade", "Informalidade lag-12", "Conjunta"),
       title = "Tabela 8 — Desemprego vs. informalidade", digits = 4)

etable(psi_pme, psi_pnadc,
       headers = c("Somente PME", "Somente PNADC"),
       title = "Tabela 9 — Split-sample por fonte de dados", digits = 4)

etable(psi_nt, psi_quebra_fonte,
       headers = c("Baseline", "Interação fonte"),
       title = "Tabela 10 — Teste de quebra PME -> PNADC", digits = 4)

sink()

# --- 10.2 Gráficos consolidados em um único PDF multi-página ----------------
pdf(file.path(PASTA_SAIDA, "Graficos_Consolidados.pdf"), width = 10, height = 7)
print(fig_sp_dispersao)
print(fig_sp_serie)
print(fig_curva_phillips_periodos)
print(fig_comp_bc)
print(fig_comp_psi_kappa)
print(fig_comp_kappa_var)
print(fig_comp_pre_pos)
print(fig_continuidade_fonte)
dev.off()

cat("\nExportação concluída. Arquivos salvos em:\n -",
    file.path(PASTA_SAIDA, "Tabelas_Consolidadas.txt"), "\n -",
    file.path(PASTA_SAIDA, "Graficos_Consolidados.pdf"), "\n")
# ============================================================
# TCC - Curva de Phillips Municipal
# RAIS: download agregado + cálculo dos pesos setoriais S_bar_xi
#
# Insumo fixo (não varia no tempo) do instrumento de
# tradeable-demand spillovers (Seção 7.3 do texto):
#   TradableDemand_it = sum_x S_bar_xi * cresc_3anos_setor_x_excl_i,t
# Este script produz o S_bar_xi. O crescimento setorial
# nacional leave-one-out (a partir do CAGED) fica em outro
# arquivo (02_caged_...).
# ============================================================

library(basedosdados)
library(dplyr)
library(readr)
library(tidyr)

# ---- 0. Configurar projeto de faturamento no BigQuery ------
basedosdados::set_billing_id("project-1f404c2e-eaf5-4163-8d5")

# ---- 1. Municípios da amostra (códigos IBGE de 7 dígitos) --
municipios <- tibble::tribble(
  ~municipio,           ~uf, ~id_municipio,
  "Aracaju",            "SE", "2800308",
  "Belém",              "PA", "1501402",
  "Belo Horizonte",     "MG", "3106200",
  "Brasília",           "DF", "5300108",
  "Campo Grande",       "MS", "5002704",
  "Curitiba",           "PR", "4106902",
  "Fortaleza",          "CE", "2304400",
  "Goiânia",            "GO", "5208707",
  "Porto Alegre",       "RS", "4314902",
  "Recife",             "PE", "2611606",
  "Rio Branco",         "AC", "1200401",
  "Rio de Janeiro",     "RJ", "3304557",
  "Salvador",           "BA", "2927408",
  "São Luís",           "MA", "2111300",
  "São Paulo",          "SP", "3550308",
  "Vitória",            "ES", "3205309"
)

ids_municipios <- paste0("'", municipios$id_municipio, "'", collapse = ", ")

# ---- 2. Query agregada direto no BigQuery -------------------
# NÃO baixar o microdado linha a linha (ordem de bilhões de
# vínculos). Agregamos direto no BigQuery e só trazemos o
# resultado já resumido para o R.
query <- sprintf("
  SELECT
    ano,
    id_municipio,
    SUBSTR(cnae_2, 1, 2) AS cnae_divisao,
    COUNT(*) AS vinculos_ativos
  FROM `basedosdados.br_me_rais.microdados_vinculos`
  WHERE id_municipio IN (%s)
    AND vinculo_ativo_3112 = '1'
    AND ano BETWEEN 2002 AND 2023
  GROUP BY ano, id_municipio, cnae_divisao
  ORDER BY id_municipio, ano, cnae_divisao
", ids_municipios)

rais_municipio_setor_ano <- basedosdados::read_sql(query)

# ---- 3. Conferência básica ----------------------------------
glimpse(rais_municipio_setor_ano)

rais_municipio_setor_ano %>% distinct(id_municipio) %>% nrow()      # esperado: 16
rais_municipio_setor_ano %>% summarise(ano_min = min(ano), ano_max = max(ano))

# NOTA: 2002-2005 não têm cnae_divisao classificado (RAIS só
# passou a usar CNAE 2.0 a partir do ano-base 2006). Confirmado:
# os 16 municípios têm exatamente 1 linha NA por ano nesse
# intervalo, com o total de vínculos do município inteiro.
rais_municipio_setor_ano %>%
  filter(is.na(cnae_divisao)) %>%
  distinct(id_municipio, ano) %>%
  count(ano)

readr::write_csv(rais_municipio_setor_ano, "rais_municipio_setor_ano.csv")

# ---- 4. Definir período-base para os pesos -------------------
# Não pode incluir 2002-2005 (sem classificação setorial). Usamos
# os primeiros anos com cobertura setorial completa como
# baseline "pré-choques", para que o peso não capture variação
# contemporânea ao painel.
ano_base_inicio <- 2006
ano_base_fim    <- 2008

# ---- 5. Filtrar linhas com setor classificado -----------------
rais_setorizado <- rais_municipio_setor_ano %>%
  filter(!is.na(cnae_divisao))

# ---- 6. Emprego total por município-ano ------------------------
emprego_total_municipio_ano <- rais_setorizado %>%
  group_by(id_municipio, ano) %>%
  summarise(emprego_total = sum(vinculos_ativos, na.rm = TRUE),
            .groups = "drop")

# ---- 7. Participação setorial em cada ano -----------------------
participacao_setorial <- rais_setorizado %>%
  left_join(emprego_total_municipio_ano, by = c("id_municipio", "ano")) %>%
  mutate(participacao = vinculos_ativos / emprego_total)

# Checagem: participações de um município-ano devem somar ~1
participacao_setorial %>%
  group_by(id_municipio, ano) %>%
  summarise(soma = sum(participacao), .groups = "drop") %>%
  filter(abs(soma - 1) > 0.01)   # deve retornar 0 linhas

# ---- 8. Média no período-base -> S_bar_xi ------------------------
pesos_setoriais_S_xi <- participacao_setorial %>%
  filter(ano >= ano_base_inicio, ano <= ano_base_fim) %>%
  group_by(id_municipio, cnae_divisao) %>%
  summarise(S_xi = mean(participacao, na.rm = TRUE), .groups = "drop")

# Checagem: soma de S_xi por município deve ficar próxima de 1
pesos_setoriais_S_xi %>%
  group_by(id_municipio) %>%
  summarise(soma_S_xi = sum(S_xi)) %>%
  arrange(soma_S_xi)

# Checagem: número de setores e concentração do maior peso por
# município (setores muito concentrados geram menos variação
# útil no instrumento)
pesos_setoriais_S_xi %>%
  group_by(id_municipio) %>%
  summarise(n_setores = n(), maior_peso = max(S_xi)) %>%
  arrange(n_setores)

# ---- 9. Salvar para a etapa do CAGED ------------------------------
readr::write_csv(pesos_setoriais_S_xi, "pesos_setoriais_S_xi.csv")
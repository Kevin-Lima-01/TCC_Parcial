# ============================================================
# TCC - Curva de Phillips Municipal
# CAGED: download agregado (nacional + 16 municípios) por
#        setor (CNAE 2 dígitos, via cnae_2_subclasse) x mês
#
# Tabelas confirmadas via INFORMATION_SCHEMA + amostras:
#   - br_me_caged.microdados_movimentacao  (Novo CAGED)
#   - br_me_caged.microdados_antigos       (histórico, 2007-2019)
# Setor extraído de cnae_2_subclasse (7 dígitos, só numérico) em
# AMBAS as tabelas - cnae_2 de microdados_antigos é outro nível
# (classe, 5 dígitos) e não deve ser usado aqui.
# saldo_movimentacao já vem +1/-1 por linha nas duas tabelas,
# então SUM() soma corretamente sem tratar tipo_movimentacao /
# admitidos_desligados manualmente.
#
# LIMITAÇÃO DE DADOS confirmada: microdados_antigos só cobre
# 2007-2019. Não há CAGED municipal setorizado via basedosdados
# para 2002-2006 - mesmo intervalo em que a RAIS também não tem
# classificação setorial. O instrumento só poderá ser construído
# a partir de ~2007.
# ============================================================

library(basedosdados)
library(dplyr)
library(readr)

# ---- 0. Faturamento (mesmo projeto usado na RAIS) ------------
basedosdados::set_billing_id("project-1f404c2e-eaf5-4163-8d5")

# ---- 1. Municípios da amostra (mesmos 16 da RAIS) -------------
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

# ---- 2. CAGED NACIONAL - Novo CAGED ----------------------------
# Sem filtro de município (Brasil inteiro).
query_nacional_novo <- "
  SELECT
    ano,
    mes,
    SUBSTR(cnae_2_subclasse, 1, 2) AS cnae_divisao,
    SUM(saldo_movimentacao) AS saldo_nacional
  FROM `basedosdados.br_me_caged.microdados_movimentacao`
  GROUP BY ano, mes, cnae_divisao
  ORDER BY ano, mes, cnae_divisao
"
caged_nacional_novo <- basedosdados::read_sql(query_nacional_novo)

# ---- 3. CAGED NACIONAL - microdados_antigos (2007-2019) ----------
query_nacional_antigo <- "
  SELECT
    ano,
    mes,
    SUBSTR(cnae_2_subclasse, 1, 2) AS cnae_divisao,
    SUM(saldo_movimentacao) AS saldo_nacional
  FROM `basedosdados.br_me_caged.microdados_antigos`
  GROUP BY ano, mes, cnae_divisao
  ORDER BY ano, mes, cnae_divisao
"
caged_nacional_antigo <- basedosdados::read_sql(query_nacional_antigo)

# ---- 4. Combinar nacional (antigo + novo) e marcar a fonte -----
caged_nacional_setor_mes <- bind_rows(
  caged_nacional_antigo %>% mutate(fonte = "antigo"),
  caged_nacional_novo   %>% mutate(fonte = "novo")
) %>%
  arrange(ano, mes, cnae_divisao)

# ---- 5. CAGED dos 16 municípios - Novo CAGED --------------------
query_municipio_novo <- sprintf("
  SELECT
    ano,
    mes,
    id_municipio,
    SUBSTR(cnae_2_subclasse, 1, 2) AS cnae_divisao,
    SUM(saldo_movimentacao) AS saldo_municipio
  FROM `basedosdados.br_me_caged.microdados_movimentacao`
  WHERE id_municipio IN (%s)
  GROUP BY ano, mes, id_municipio, cnae_divisao
  ORDER BY id_municipio, ano, mes, cnae_divisao
", ids_municipios)
caged_municipio_novo <- basedosdados::read_sql(query_municipio_novo)

# ---- 6. CAGED dos 16 municípios - microdados_antigos --------------
query_municipio_antigo <- sprintf("
  SELECT
    ano,
    mes,
    id_municipio,
    SUBSTR(cnae_2_subclasse, 1, 2) AS cnae_divisao,
    SUM(saldo_movimentacao) AS saldo_municipio
  FROM `basedosdados.br_me_caged.microdados_antigos`
  WHERE id_municipio IN (%s)
  GROUP BY ano, mes, id_municipio, cnae_divisao
  ORDER BY id_municipio, ano, mes, cnae_divisao
", ids_municipios)
caged_municipio_antigo <- basedosdados::read_sql(query_municipio_antigo)

# ---- 7. Combinar município (antigo + novo) -----------------------
caged_municipio_setor_mes <- bind_rows(
  caged_municipio_antigo %>% mutate(fonte = "antigo"),
  caged_municipio_novo   %>% mutate(fonte = "novo")
) %>%
  arrange(id_municipio, ano, mes, cnae_divisao)

# ---- 8. Conferências ----------------------------------------------
# Cobertura temporal de cada pedaço - esperado: antigo 2007-2019,
# novo a partir de 2020.
caged_nacional_setor_mes %>%
  group_by(fonte) %>%
  summarise(ano_min = min(ano), ano_max = max(ano))

caged_municipio_setor_mes %>%
  group_by(fonte) %>%
  summarise(ano_min = min(ano), ano_max = max(ano))

# Todos os 16 municípios aparecem em cada fonte?
caged_municipio_setor_mes %>%
  group_by(fonte) %>%
  summarise(n_municipios = n_distinct(id_municipio))  # esperado: 16 nas duas

# Existe algum mês sem nenhum dado nacional? (buraco de cobertura,
# incluindo o hiato conhecido de 2002-2006)
caged_nacional_setor_mes %>%
  distinct(ano, mes) %>%
  arrange(ano, mes) %>%
  mutate(ano_mes = ano * 100 + mes) %>%
  mutate(salto = ano_mes - lag(ano_mes)) %>%
  filter(salto > 1 | is.na(salto))

# Checagem de continuidade na transição antigo -> novo (jan/2020):
# o saldo nacional total não deveria dar um salto absurdo de
# nível entre dez/2019 e jan/2020
caged_nacional_setor_mes %>%
  group_by(ano, mes) %>%
  summarise(saldo_total = sum(saldo_nacional), .groups = "drop") %>%
  filter(ano %in% c(2019, 2020)) %>%
  arrange(ano, mes) %>%
  print(n = 24)

# ---- 9. Salvar para a etapa de cálculo do instrumento --------------
readr::write_csv(caged_nacional_setor_mes, "caged_nacional_setor_mes.csv")
readr::write_csv(caged_municipio_setor_mes, "caged_municipio_setor_mes.csv")
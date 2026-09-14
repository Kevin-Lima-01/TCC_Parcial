# ============================================================
# TCC - Curva de Phillips Municipal
# Construção do instrumento tradeable-demand spillovers
# (VERSÃO COMPLETA - normalizada em log-crescimento)
#
# TradableDemand_it = soma_x [ S_bar_xi * Delta36m_log_S_{-i,x,t} ]
#
# onde:
#   S_bar_xi              = peso setorial fixo (RAIS, baseline 2006-2008),
#                           restrito à INDÚSTRIA DE TRANSFORMAÇÃO (Seção C,
#                           CNAE 10-33 - ver passo 1b) e renormalizado para
#                           somar 1 dentro desse subconjunto em cada
#                           município. Agropecuária (A) e extrativa (B)
#                           foram deliberadamente excluídas: são setores de
#                           commodity quase pura, cujo preço nacional/
#                           internacional contamina a inflação por um canal
#                           de custo que não passa pelo emprego local
#                           (ver diagnóstico na Seção 11.5-11.7 do TCC).
#   Delta36m_log_S_{-i,x,t} = log(estoque_{-i,x,t}) - log(estoque_{-i,x,t-36})
#   estoque_{-i,x,t}      = estoque de emprego formal do setor x no
#                           RESTO DO BRASIL (nacional menos o próprio
#                           município i), reconstruído mensalmente
#
# RECONSTRUÇÃO DO ESTOQUE MENSAL:
# RAIS dá o nível real em 31/dez de cada ano (a partir de 2006).
# CAGED dá o fluxo líquido mensal (admissões - desligamentos).
# Para cada mês m do ano Y: estoque_m = estoque_RAIS_dez_(Y-1) +
# soma acumulada do saldo CAGED de jan/Y até m. O "erro" do CAGED
# em relação à RAIS não se acumula por décadas: a âncora é
# reiniciada com o dado real todo mês de janeiro.
#
# LIMITE DE COBERTURA: só é possível construir o estoque a partir
# de 2007 (precisa da RAIS de dez/2006, que é o primeiro ano com
# classificação CNAE 2.0) - mesmo corte onde o CAGED "antigos"
# começa. Consistente, não é coincidência.
# ============================================================

library(basedosdados)
library(dplyr)
library(tidyr)
library(readr)
library(zoo)      # soma móvel / defasagem para a janela de 36 meses
library(ggplot2)
library(scales)
library(bit64)    # necessário para detectar/converter colunas integer64
# que vêm de downloads via basedosdados::read_sql

# ---- 0. CAMINHOS DOS ARQUIVOS -------------------------------
caminho_rais_municipio_setor_ano  <- "FGV/TCC/Dados/IV/RAIS/rais_municipio_setor_ano.csv"
caminho_pesos_setoriais_S_xi      <- "FGV/TCC/Dados/IV/RAIS/pesos_setoriais_S_xi.csv"
caminho_caged_nacional_setor_mes  <- "FGV/TCC/Dados/IV/CAGED/caged_nacional_setor_mes.csv"
caminho_caged_municipio_setor_mes <- "FGV/TCC/Dados/IV/CAGED/caged_municipio_setor_mes.csv"

# Onde salvar/ler o novo CSV da RAIS nacional (baixado neste script)
caminho_rais_nacional_setor_ano <- "FGV/TCC/Dados/IV/RAIS/rais_nacional_setor_ano.csv"

# Projeto de faturamento do BigQuery (mesmo da RAIS/CAGED)
projeto_billing <- "project-1f404c2e-eaf5-4163-8d5"

# Município usado para ilustrar tudo com gráficos
municipio_exemplo_id   <- "3550308"   # São Paulo
municipio_exemplo_nome <- "São Paulo"

# Pasta de saída dos gráficos
pasta_graficos <- "FGV/TCC/Dados/IV/graficos_instrumento"
dir.create(pasta_graficos, showWarnings = FALSE)

# ---- 1. Carregar os 4 CSVs já existentes -----------------------
rais_municipio_setor_ano  <- readr::read_csv(caminho_rais_municipio_setor_ano,
                                             col_types = cols(id_municipio = col_character(),
                                                              cnae_divisao = col_character()))
pesos_setoriais_S_xi      <- readr::read_csv(caminho_pesos_setoriais_S_xi,
                                             col_types = cols(id_municipio = col_character(),
                                                              cnae_divisao = col_character()))
caged_nacional_setor_mes  <- readr::read_csv(caminho_caged_nacional_setor_mes,
                                             col_types = cols(cnae_divisao = col_character()))
caged_municipio_setor_mes <- readr::read_csv(caminho_caged_municipio_setor_mes,
                                             col_types = cols(id_municipio = col_character(),
                                                              cnae_divisao = col_character()))

# ---- 1b. Classificação tradable/non-tradable e renormalização ----
# VERSÃO REVISADA (pós-diagnóstico de viés de commodities, ver TCC
# Seção 11.5-11.7): a primeira versão deste instrumento usava
# agropecuária (A) + extrativa (B) + transformação (C) como
# "tradable". A forma reduzida mostrou o instrumento afetando
# Tradable/IPCA MAIS do que Non-Tradable - o oposto do canal de
# demanda por trabalho local que o instrumento deveria capturar -
# e o teste de Sargan rejeitou a validade conjunta com o lag do
# desemprego. Diagnóstico: A e B são setores de commodity quase
# pura (preço formado em mercado nacional/internacional, sujeito a
# choques de preço que vazam para a inflação por um canal de custo,
# não de emprego local), então entravam pesado na composição do
# instrumento em municípios com base extrativa/agro relevante.
#
# Restringimos agora SÓ à indústria de transformação (Seção C,
# CNAE 10-33): é o setor tradable com produto mais diversificado e
# heterogêneo, mais perto do "manufacturing" usado no paper original
# (dados americanos, onde a base tradable de cada estado já é
# predominantemente industrial, não extrativa/agro).
#
# EXCLUSAO_COMMODITY_MANUFATURA: subconjunto de divisões dentro da
# própria transformação que ainda têm preço fortemente atrelado a
# commodity/insumo importado (processamento de alimentos ligado a
# grãos/carne, papel e celulose, refino de petróleo/biocombustíveis,
# metalurgia básica). Deixamos como FALSE por padrão (só tira A e B);
# mude para TRUE para rodar como teste de robustez adicional e ver
# se o sinal se comporta melhor ao remover também esses subsetores.
EXCLUIR_TRANSFORMACAO_COMMODITY <- FALSE

divisoes_transformacao_commodity <- sprintf("%02d", c(10, 11, 17, 19, 24))
# 10-11: alimentos e bebidas (preço puxado por grãos/proteína animal)
# 17:    papel e celulose (preço puxado por commodity florestal)
# 19:    coque, petróleo e biocombustíveis (preço puxado por petróleo)
# 24:    metalurgia básica (preço puxado por minério/aço internacional)

setores_tradable <- sprintf("%02d", 10:33)   # Seção C - indústria de transformação

if (EXCLUIR_TRANSFORMACAO_COMMODITY) {
  setores_tradable <- setdiff(setores_tradable, divisoes_transformacao_commodity)
}

# Restringe aos setores tradable (agora só manufatura) e RENORMALIZA
# para que os pesos de cada município voltem a somar 1 entre si
# (senão o instrumento fica artificialmente pequeno em municípios
# com pouca indústria de transformação).
pesos_tradable_S_xi <- pesos_setoriais_S_xi %>%
  filter(cnae_divisao %in% setores_tradable) %>%
  group_by(id_municipio) %>%
  mutate(S_xi = S_xi / sum(S_xi)) %>%
  ungroup()

# Checagem 1: pesos renormalizados devem somar 1 por município
pesos_tradable_S_xi %>%
  group_by(id_municipio) %>%
  summarise(soma_S_xi = sum(S_xi)) %>%
  arrange(soma_S_xi)

# Checagem 2: quanto do emprego de cada município é indústria de
# transformação ANTES da renormalização? Com A e B fora, municípios
# de base extrativa/agro forte (ex. Belém, Rio Branco, Grande Vitória)
# tendem a cair bastante aqui - podem passar a depender de poucas
# divisões CNAE para todo o instrumento. Olhar caso a caso; um
# município com fatia manufatureira muito baixa (<5-10%) é candidato
# a ter o instrumento mais ruidoso/fraco nesta versão.
pesos_setoriais_S_xi %>%
  mutate(tradable = cnae_divisao %in% setores_tradable) %>%
  group_by(id_municipio, tradable) %>%
  summarise(soma_original = sum(S_xi), .groups = "drop") %>%
  filter(tradable) %>%
  arrange(soma_original)

# ---- 2. Baixar a RAIS NACIONAL por setor-ano (NOVO) --------------
# Mesma lógica da query municipal, mas SEM filtro de município -
# Brasil inteiro. Varre a tabela inteira: pode consumir dezenas de
# GB de cota do BigQuery (ainda dentro do 1 TB/mês gratuito).
basedosdados::set_billing_id(projeto_billing)

query_rais_nacional <- "
  SELECT
    ano,
    SUBSTR(cnae_2, 1, 2) AS cnae_divisao,
    COUNT(*) AS emprego_nacional
  FROM `basedosdados.br_me_rais.microdados_vinculos`
  WHERE vinculo_ativo_3112 = '1'
    AND ano BETWEEN 2006 AND 2023
  GROUP BY ano, cnae_divisao
  ORDER BY ano, cnae_divisao
"
rais_nacional_setor_ano <- basedosdados::read_sql(query_rais_nacional)

# CORREÇÃO DE TIPO: o BigQuery devolve colunas inteiras como
# "integer64" (pacote bit64), diferente do "double" comum que o
# read_csv usa ao carregar os outros 3 arquivos. Sem essa conversão,
# qualquer join entre esta tabela (recém-baixada) e as tabelas
# carregadas de CSV quebra com "incompatible types".
# Diagnóstico: rode sapply(rais_nacional_setor_ano, class) e veja
# se alguma coluna aparece como "integer64" antes de dar join nela.
rais_nacional_setor_ano <- rais_nacional_setor_ano %>%
  mutate(across(where(bit64::is.integer64), as.numeric))

readr::write_csv(rais_nacional_setor_ano, caminho_rais_nacional_setor_ano)

# Checagem rápida: emprego nacional deve ser MUITO maior que a
# soma dos 16 municípios da amostra (senão algo está errado)
rais_nacional_setor_ano %>% summarise(total_nacional = sum(emprego_nacional))
rais_municipio_setor_ano %>%
  filter(!is.na(cnae_divisao)) %>%
  summarise(total_16_municipios = sum(vinculos_ativos))

# ---- 3. Grade completa município x setor x mês (mesma lógica de antes)
# Usa pesos_tradable_S_xi (só indústria de transformação, já
# renormalizados) - agropecuária, extrativa e non-tradable nem
# entram na grade, então nem entram no cálculo de estoque/crescimento
# mais à frente.
calendario <- caged_nacional_setor_mes %>% distinct(ano, mes)

setores_por_municipio <- pesos_tradable_S_xi %>%
  distinct(id_municipio, cnae_divisao)

grade_completa <- setores_por_municipio %>%
  tidyr::crossing(calendario)

# ---- 4. Saldo mensal excluindo o município (fluxo) ----------------
caged_municipio_completo <- grade_completa %>%
  left_join(
    caged_municipio_setor_mes %>%
      select(ano, mes, id_municipio, cnae_divisao, saldo_municipio),
    by = c("id_municipio", "cnae_divisao", "ano", "mes")
  ) %>%
  mutate(saldo_municipio = coalesce(saldo_municipio, 0L))

caged_excl_i <- caged_municipio_completo %>%
  left_join(
    caged_nacional_setor_mes %>%
      select(ano, mes, cnae_divisao, saldo_nacional),
    by = c("ano", "mes", "cnae_divisao")
  ) %>%
  mutate(
    saldo_nacional = coalesce(saldo_nacional, 0L),
    saldo_excl_i   = saldo_nacional - saldo_municipio,
    data_ref       = ano * 12L + mes
  ) %>%
  arrange(id_municipio, cnae_divisao, data_ref)

# ---- 5. Estoque anual excl. município (RAIS nacional - RAIS município)
estoque_dez_excl_i_setor_ano <- rais_nacional_setor_ano %>%
  full_join(
    rais_municipio_setor_ano %>%
      filter(!is.na(cnae_divisao)) %>%
      select(ano, id_municipio, cnae_divisao, vinculos_ativos),
    by = c("ano", "cnae_divisao")
  ) %>%
  # se um município não tem registro num setor-ano, trata como 0
  mutate(vinculos_ativos = coalesce(vinculos_ativos, 0L)) %>%
  filter(!is.na(id_municipio)) %>%   # descarta combinações sem município (fora do escopo)
  mutate(estoque_dez_excl_i = emprego_nacional - vinculos_ativos)

# Checagem: estoque_dez_excl_i nunca deveria ser negativo
estoque_dez_excl_i_setor_ano %>% filter(estoque_dez_excl_i < 0) %>% nrow()

# ---- 6. Reconstruir o estoque MENSAL excl. município ---------------
# Âncora: o estoque de dez/(Y-1) vale para todos os meses do ano Y,
# somado ao fluxo acumulado do próprio ano Y até aquele mês.
ancora_dez_excl_i <- estoque_dez_excl_i_setor_ano %>%
  transmute(id_municipio, cnae_divisao,
            ano_ancora_para = ano + 1L,
            estoque_dez_excl_i)

estoque_mensal_excl_i <- caged_excl_i %>%
  group_by(id_municipio, cnae_divisao, ano) %>%
  mutate(fluxo_acumulado_no_ano = cumsum(saldo_excl_i)) %>%
  ungroup() %>%
  left_join(ancora_dez_excl_i,
            by = c("id_municipio", "cnae_divisao", "ano" = "ano_ancora_para")) %>%
  mutate(estoque_mensal = estoque_dez_excl_i + fluxo_acumulado_no_ano)

# Checagem: cobertura válida deve começar em 2007 (falta âncora
# para 2006, que precisaria da RAIS de dez/2005 - não existe)
estoque_mensal_excl_i %>%
  filter(!is.na(estoque_mensal)) %>%
  summarise(ano_min = min(ano), ano_max = max(ano))

# Checagem: algum estoque mensal ficou <= 0? (inviabiliza o log)
estoque_mensal_excl_i %>% filter(estoque_mensal <= 0) %>% nrow()

# ---- 7. Crescimento em log de 36 meses ------------------------------
estoque_mensal_excl_i <- estoque_mensal_excl_i %>%
  arrange(id_municipio, cnae_divisao, data_ref) %>%
  group_by(id_municipio, cnae_divisao) %>%
  mutate(
    log_estoque   = log(estoque_mensal),
    cresc_36m_log = log_estoque - dplyr::lag(log_estoque, 36)
  ) %>%
  ungroup()

# ---- 8. Combinar com os pesos S_bar_xi (tradable, renormalizados) ---
instrumento_final <- estoque_mensal_excl_i %>%
  filter(!is.na(cresc_36m_log)) %>%
  left_join(pesos_tradable_S_xi, by = c("id_municipio", "cnae_divisao")) %>%
  group_by(id_municipio, ano, mes) %>%
  summarise(
    tradable_demand_log = sum(S_xi * cresc_36m_log, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(id_municipio, ano, mes)

readr::write_csv(instrumento_final, "instrumento_tradable_demand_log.csv")

# ============================================================
# GRÁFICOS EXPLORATÓRIOS - todos ilustrados com São Paulo
# ============================================================

# ---- Gráfico 1: pesos setoriais S_bar_xi (tradable) de São Paulo ---
top_setores_sp <- pesos_tradable_S_xi %>%
  filter(id_municipio == municipio_exemplo_id) %>%
  arrange(desc(S_xi)) %>%
  slice_head(n = 15)

g1 <- ggplot(top_setores_sp, aes(x = reorder(cnae_divisao, S_xi), y = S_xi)) +
  geom_col(fill = "#2c7fb8") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = paste("Pesos setoriais tradable (S_bar_xi renormalizado) —", municipio_exemplo_nome),
    subtitle = "Participação média no emprego formal TRADABLE, baseline 2006-2008",
    x = "Divisão CNAE (2 dígitos)",
    y = "Participação no emprego tradable"
  ) +
  theme_minimal()

print(g1)
ggsave(file.path(pasta_graficos, "01_pesos_setoriais_sp.png"), g1,
       width = 8, height = 6, dpi = 150)

# ---- Gráfico 2: estoque reconstruído (excl. SP) - setor principal --
setor_principal_sp <- top_setores_sp$cnae_divisao[1]

estoque_setor_principal <- estoque_mensal_excl_i %>%
  filter(id_municipio == municipio_exemplo_id,
         cnae_divisao == setor_principal_sp,
         !is.na(estoque_mensal)) %>%
  mutate(data = as.Date(sprintf("%d-%02d-01", ano, mes)))

g2 <- ggplot(estoque_setor_principal, aes(x = data, y = estoque_mensal)) +
  geom_line(color = "#2c7fb8") +
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dotted", color = "grey30") +
  scale_y_continuous(labels = comma_format()) +
  labs(
    title = paste("Estoque reconstruído (excl.", municipio_exemplo_nome, ")"),
    subtitle = paste0("Setor CNAE ", setor_principal_sp,
                      " — âncora RAIS (dez) + fluxo acumulado CAGED"),
    x = NULL, y = "Estoque de emprego formal (vínculos)"
  ) +
  theme_minimal()

print(g2)
ggsave(file.path(pasta_graficos, "02_estoque_reconstruido_setor_principal_sp.png"), g2,
       width = 8, height = 5, dpi = 150)

# ---- Gráfico 3: crescimento em log de 36 meses (excl. SP) -----------
crescimento_log_setor_principal <- estoque_mensal_excl_i %>%
  filter(id_municipio == municipio_exemplo_id,
         cnae_divisao == setor_principal_sp,
         !is.na(cresc_36m_log)) %>%
  mutate(data = as.Date(sprintf("%d-%02d-01", ano, mes)))

g3 <- ggplot(crescimento_log_setor_principal, aes(x = data, y = cresc_36m_log)) +
  geom_line(color = "#d95f0e") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dotted", color = "grey30") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = paste("Crescimento log de 36 meses (excl.", municipio_exemplo_nome, ")"),
    subtitle = paste0("Setor CNAE ", setor_principal_sp,
                      " — Delta log(estoque_t) - log(estoque_t-36)"),
    x = NULL, y = "Crescimento acumulado (log)"
  ) +
  theme_minimal()

print(g3)
ggsave(file.path(pasta_graficos, "03_crescimento_log36m_setor_principal_sp.png"), g3,
       width = 8, height = 5, dpi = 150)

# ---- Gráfico 4: instrumento final (TradableDemand log) para SP -----
instrumento_sp <- instrumento_final %>%
  filter(id_municipio == municipio_exemplo_id) %>%
  mutate(data = as.Date(sprintf("%d-%02d-01", ano, mes)))

g4 <- ggplot(instrumento_sp, aes(x = data, y = tradable_demand_log)) +
  geom_line(color = "#31a354") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dotted", color = "grey30") +
  labs(
    title = paste("Instrumento TradableDemand (log-crescimento) —", municipio_exemplo_nome),
    subtitle = "Soma ponderada do crescimento log setorial nacional excl. o município (36 meses)",
    x = NULL, y = "TradableDemand_it"
  ) +
  theme_minimal()

print(g4)
ggsave(file.path(pasta_graficos, "04_instrumento_log_sp.png"), g4,
       width = 8, height = 5, dpi = 150)

# ---- Gráfico 5: distribuição do instrumento entre municípios --------
ano_referencia <- instrumento_final %>%
  filter(!is.na(tradable_demand_log)) %>%
  summarise(ano = max(ano)) %>%
  pull(ano)

snapshot_transversal <- instrumento_final %>%
  filter(ano == ano_referencia) %>%
  group_by(id_municipio) %>%
  summarise(tradable_demand_media_ano = mean(tradable_demand_log, na.rm = TRUE),
            .groups = "drop")

g5 <- ggplot(snapshot_transversal,
             aes(x = reorder(id_municipio, tradable_demand_media_ano),
                 y = tradable_demand_media_ano)) +
  geom_col(fill = "#756bb1") +
  coord_flip() +
  labs(
    title = paste("Instrumento TradableDemand (log) por município —", ano_referencia),
    subtitle = "Variação transversal usada na identificação de kappa/psi",
    x = "Município (código IBGE)", y = "TradableDemand médio no ano"
  ) +
  theme_minimal()

print(g5)
ggsave(file.path(pasta_graficos, "05_instrumento_transversal_municipios.png"), g5,
       width = 8, height = 6, dpi = 150)
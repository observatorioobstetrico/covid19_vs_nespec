
# Bibliotecas -------------------------------------------------------------

#manipulacao
library(dplyr)
library(tidyr)
library(forcats)
library(lubridate)
#descritiva
library(gtsummary)


# Importacao dos dados ----------------------------------------------------

# srag real
srag1621 <- readRDS("01.dados/srag_16-21_[all].rds")

# srag predita
srag1621_pred <- readRDS("01.dados/srag_16-21_[pred].rds")


# Manipulacao dos dados ---------------------------------------------------

## Base real de SRAG ####
d_srag_real <- srag1621 |> 
  mutate(
    # casos indefinidos = na
    class_caso = as.factor(
      if_else(class_caso == "Agente etiológico não especificado", NA_character_, class_caso)
    ),
    # covid-19 = covid-19 real
    class_caso = fct_recode(as.factor(class_caso), "COVID-19r" = "COVID-19"),
    # outros agentes = outros agente real
    class_caso = fct_recode(
      class_caso, 
      "Outros agentes confirmadosr" = "Outros agentes confirmados"
    )
  ) |>
  rename(class_caso_real_pred = class_caso) |> 
  select(
    ano, sg_uf, class_gest_puerp, nu_idade_n, raca, escolaridade, vacina,
    dt_sin_pri, febre, tosse, garganta, dispneia, desc_resp, saturacao, diarreia,
    cardiopatia, pneumopatia, renal, obesidade,
    evolucao, class_caso_real_pred
  ) |>  
  drop_na()

# numero de notificacoes de srag no dia
d_not <- d_srag_real |> 
  mutate(dt_sin_pri = as.Date(dt_sin_pri, "%d/%m/%Y")) |> 
  group_by(dt_sin_pri) |> 
  summarise(not_casos = n())

d_srag_real <- d_srag_real |> 
  mutate_at("dt_sin_pri", dmy) |> 
  left_join(d_not, by = "dt_sin_pri")


## Base predita de SRAG ####
d_srag_pred <- srag1621_pred |> 
  mutate(
    # covid-19 = covid-19 predito
    class_caso_pred = fct_recode(class_caso_pred, "COVID-19p" = "COVID-19"),
    # outros agentes = outros agente predito
    class_caso_pred = fct_recode(
      class_caso_pred, 
      "Outros agentes confirmadosp" = "Outros agentes confirmados"
    ),
  ) |> 
  rename(class_caso_real_pred = class_caso_pred) |> 
  select(-class_caso)


## Base real e predita de SRAG ####
d_srag_real_pred <- d_srag_real |> 
  full_join(d_srag_pred) |> 
  relocate(ano, sg_uf, not_casos)


# Analise descritiva ------------------------------------------------------

theme_gtsummary_language("pt", big.mark = "")

tbl_cov <- d_srag_real_pred |> 
  mutate(
    class_caso_real_pred = fct_recode(
      class_caso_real_pred, 
      "COVID-19 real" = "COVID-19r", 
      "COVID-19 predita" = "COVID-19p",
      "Outros agentes confirmados real" = "Outros agentes confirmadosr", 
      "Outros agentes confirmados predita" = "Outros agentes confirmadosp"
    ),
    class_caso_real_pred = fct_relevel(
      class_caso_real_pred, 
      "COVID-19 real", "COVID-19 predita", 
      "Outros agentes confirmados real", "Outros agentes confirmados predita"
    )
  ) |> 
  tbl_summary(
    include = c(
      not_casos, nu_idade_n, raca, escolaridade, vacina, class_gest_puerp,
      febre, tosse, garganta, dispneia, desc_resp, saturacao, diarreia,
      cardiopatia, pneumopatia, renal, obesidade
    ),
    label = list(
      not_casos ~ "Notificação de SRAG por dia; média ± dp",
      nu_idade_n ~ "Idade (anos); média ± dp",
      raca ~ "Raça, N (%)",
      escolaridade ~ "Escolaridade, N (%)",
      vacina ~ "Vacina contra Influenza, N (%)",
      class_gest_puerp ~ "Momento gestacional, N (%)",
      febre ~ "Febre, N (%)", 
      tosse ~ "Tosse, N (%)",
      garganta ~ "Dor de garganta, N (%)",
      dispneia ~ "Dispneia, N (%)",
      desc_resp ~ "Desconforto respiratório, N (%)",
      saturacao ~ "Sat O₂ < 95%, N (%)",
      diarreia ~ "Diarreia, N (%)",
      cardiopatia ~ "Cardiopatia, N (%)", 
      pneumopatia ~ "Pneumopatia crônica, N (%)",
      renal ~ "Doença renal crônica, N (%)",
      obesidade ~ "Obesidade, N (%)"
    ),
    by = class_caso_real_pred,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 2, 
      all_categorical() ~ c(0, 1)
    )
  ) |> 
  modify_header(label ~ "**Variável**") |> 
  bold_labels() |>
  modify_footnote(update = everything() ~ NA) |> 
  as_gt() |> 
  gt::tab_source_note(
    gt::md(
      "COVID-19: *Coronavirus Disease 2019*; 
      dp: desvio-padrão; N: número; % porcentagem."
    )
  )


# Base final --------------------------------------------------------------

# base de srag nao especificada (2016-2019)
d_srag_nespec_1619 <- srag1621 |> 
  filter(ano < 2020 & class_caso == "Agente etiológico não especificado") |> 
  select(
    ano, sg_uf, class_gest_puerp, nu_idade_n, raca, escolaridade, vacina,
    dt_sin_pri, febre, tosse, garganta, dispneia, desc_resp, saturacao, diarreia,
    cardiopatia, pneumopatia, renal, obesidade,
    evolucao, class_caso
  ) |>  
  drop_na() |> 
  rename(class_caso_real_pred = class_caso)

# notificacoes de srag nao especificada no dia  
d_not_nespec <- d_srag_nespec_1619 |> 
  mutate(dt_sin_pri = as.Date(dt_sin_pri, "%d/%m/%Y")) |> 
  group_by(dt_sin_pri) |> 
  summarise(not_casos = n())

# base de srag nao especificada (2016-2019) + notificacoes de casos
d_srag_nespec_1619 <- d_srag_nespec_1619 |> 
  mutate_at("dt_sin_pri", dmy) |> 
  left_join(d_not_nespec, by = "dt_sin_pri")

# base final
d_srag_final <- d_srag_real_pred |> 
  full_join(d_srag_nespec_1619) |> 
  mutate(
    class_caso_real_pred = case_when(
      class_caso_real_pred %in% 
        c("COVID-19", "Outros agentes confirmados", "Agente etiológico não especificado") ~ NA_character_,
      TRUE ~ class_caso_real_pred
    )
  ) |>
  mutate(
    class_caso_final = case_when(
      class_caso_real_pred == "COVID-19r" ~ "COVID-19",
      class_caso_real_pred == "Outros agentes confirmadosr" ~ "Outros agentes confirmados",
      class_caso_real_pred == "COVID-19p" ~ "COVID-19",
      class_caso_real_pred == "Outros agentes confirmadosp" ~ "Outros agentes confirmados",
      TRUE ~ class_caso_real_pred
    )
  ) |> 
  relocate(class_caso_real_pred, class_caso_final, .after = last_col())
  
saveRDS(d_srag_final, "./01.dados/srag_16-21_[final].rds")


# Exportacao da tabela ----------------------------------------------------

gt::gtsave(tbl_cov, filename = "03.resultados/descritiva/tab_cov.png")


# Limpa ambiente ----------------------------------------------------------

rm(list = ls())




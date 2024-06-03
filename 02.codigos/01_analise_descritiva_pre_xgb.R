
# Bibliotecas -------------------------------------------------------------

#manipulacao
library(dplyr)
library(tidyr)
#descritiva
library(gtsummary)


# Importacao dos dados ----------------------------------------------------

srag1621 <- readRDS("01.dados/srag_16-21_[all].rds")


# Manipulacao dos dados ---------------------------------------------------
  
d_srag <- srag1621 |> 
  # casos indefinidos = na
  mutate(
    class_caso = if_else(
      class_caso == "Agente etiológico não especificado", NA_character_, class_caso
    )
  ) |> 
  select(
    ano, sg_uf, class_gest_puerp, nu_idade_n, raca, escolaridade, vacina,
    dt_sin_pri, febre, tosse, garganta, dispneia, desc_resp, saturacao, diarreia,
    cardiopatia, pneumopatia, renal, obesidade,
    class_caso
  ) |>  
  drop_na()

# numero de notificacoes de srag por dia
d_not <- d_srag |> 
  mutate(dt_sin_pri = as.Date(dt_sin_pri, "%d/%m/%Y")) |> 
  group_by(dt_sin_pri) |> 
  summarise(not_casos = n())

d_srag <- d_srag |> 
  mutate_at("dt_sin_pri", dmy) |> 
  left_join(d_not, by = "dt_sin_pri") |> 
  select(-dt_sin_pri)


# Funcoes auxiliares ------------------------------------------------------

## C de Cohen ####
my_cohen_d <- function(data, variable, by, ...) {
  rstatix::cohens_d(data, as.formula(glue::glue("{variable} ~ {by}")))$effsize
}

## V de Cramer ####
my_cramer_v <- function(data, variable, by, ...) {
  table(data[[variable]], data[[by]]) |> 
    rstatix::cramer_v()
}


# Analise descritiva ------------------------------------------------------

theme_gtsummary_language("pt", big.mark = "")

## Tabela - Caracateristicas sociodemograficas ####
tbl_dem <- d_srag |> 
  tbl_summary(
    include = c(not_casos, nu_idade_n, raca, escolaridade, vacina, class_gest_puerp),
    label = list(
      not_casos ~ "Notificação de SRAG por dia; média ± dp",
      nu_idade_n ~ "Idade (anos); média ± dp",
      raca ~ "Raça, N (%)",
      escolaridade ~ "Escolaridade, N (%)",
      vacina ~ "Vacina contra Influenza, N (%)",
      class_gest_puerp ~ "Momento gestacional, N (%)"
    ),
    by = class_caso,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 2, 
      all_categorical() ~ c(0, 1)
    )
  ) |> 
  add_p() |> 
  add_stat(
    fns = list(
      all_continuous() ~ my_cohen_d,
      all_categorical() ~ my_cramer_v
    )
  ) |> 
  modify_header(
    label ~ "**Característica**",
    p.value ~ "**p**",
    add_stat_1 ~ "**Tamanho do efeito**"
  ) |> 
  bold_labels() |>
  modify_footnote(update = everything() ~ NA) |> 
  as_gt() |> 
  gt::tab_source_note(
    gt::md(
      "COVID-19: *Coronavirus Disease 2019*; 
      dp: desvio-padrão; N: número; p: valor-p; % porcentagem;
      \nTamanho do efeito dado pelo V de Cramér, exceto na variável Idade, em que foi utilizado D de Cohen."
    )
  )


## Tabela - Sintomas ####
tbl_sin <- d_srag |> 
  tbl_summary(
    include = c(febre, tosse, garganta, dispneia, desc_resp, saturacao, diarreia),
    label = list(
      febre ~ "Febre", 
      tosse ~ "Tosse",
      garganta ~ "Dor de garganta",
      dispneia ~ "Dispneia",
      desc_resp ~ "Desconforto respiratório",
      saturacao ~ "Sat O₂ < 95%",
      diarreia ~ "Diarreia"
    ),
    by = class_caso,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 2, 
      all_categorical() ~ c(0, 1)
    )
  ) |> 
  add_p() |> 
  add_stat(fns = all_categorical() ~ my_cramer_v) |> 
  modify_header(
    label ~ "**Sintoma, N (%)**", 
    p.value ~ "**p**",
    add_stat_1 ~ "**Tamanho do efeito**"
  ) |> 
  bold_labels() |> 
  modify_footnote(update = everything() ~ NA) |> 
  as_gt() |> 
  gt::tab_source_note(
    gt::md(
      "COVID-19: *Coronavirus Disease 2019*; 
      dp: desvio-padrão; N: número; p: valor-p; % porcentagem;
      \nTamanho do efeito dado pelo V de Cramér."
    )
  )


## Tabela - Comorbidades ####
tbl_com <- d_srag |> 
  tbl_summary(
    include = c(cardiopatia, pneumopatia, renal, obesidade),
    label = list(
      cardiopatia ~ "Cardiopatia", 
      pneumopatia ~ "Pneumopatia crônica",
      renal ~ "Doença renal crônica",
      obesidade ~ "Obesidade"
    ),
    by = class_caso,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 2, 
      all_categorical() ~ c(0, 1)
    )
  ) |> 
  add_p() |>
  add_stat(fns = all_categorical() ~ my_cramer_v)|> 
  modify_header(
    label ~ "**Comorbidade, N (%)**",
    p.value ~ "**p**",
    add_stat_1 ~ "**Tamanho do efeito**"
  ) |> 
  bold_labels() |> 
  modify_footnote(update = everything() ~ NA) |> 
  as_gt() |> 
  gt::tab_source_note(
    gt::md(
      "COVID-19: *Coronavirus Disease 2019*; 
      dp: desvio-padrão; N: número; p: valor-p; % porcentagem;
      \nTamanho do efeito dado pelo V de Cramér."
    )
  )


# Exportacao de tabelas ---------------------------------------------------

gt::gtsave(tbl_dem, filename = "03.resultados/descritiva/tab_dem.png")
gt::gtsave(tbl_sin, filename = "03.resultados/descritiva/tab_sin.png")
gt::gtsave(tbl_com, filename = "03.resultados/descritiva/tab_com.png")


# Limpa ambiente ----------------------------------------------------------

rm(list = ls())





  
  

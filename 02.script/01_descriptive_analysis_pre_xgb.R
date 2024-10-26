
# Libraries ---------------------------------------------------------------

# If these packages are not installed on your machine, install them using the command lines:
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("lubridate")
# install.packages("gtsummary")
# install.packages("gt")

#manipulation
library(dplyr)
library(tidyr)
library(lubridate)
#descriptive
library(gtsummary)
library(gt)


# Data import -------------------------------------------------------------

srag1621 <- readRDS("01.data/srag_16-21_[all].rds")


# Data manipulation -------------------------------------------------------

d_srag <- srag1621 |> 
  # undefinied cases = na
  mutate(class_caso = if_else(class_caso == "Unspecified etiological agent", NA_character_, class_caso)) |> 
  dplyr::select(
    ano, sg_uf, class_gest_puerp, nu_idade_n, raca, escolaridade, vacina,
    dt_sin_pri, febre, tosse, garganta, dispneia, desc_resp, saturacao, diarreia,
    cardiopatia, pneumopatia, renal, obesidade,
    class_caso
  ) |>  
  drop_na()

# sars notifications in the admission day
d_not <- d_srag |> 
  mutate(dt_sin_pri = as.Date(dt_sin_pri, "%d/%m/%Y")) |> 
  group_by(dt_sin_pri) |> 
  summarise(not_casos = n())

d_srag <- d_srag |> 
  mutate_at("dt_sin_pri", dmy) |> 
  left_join(d_not, by = "dt_sin_pri") |> 
  dplyr::select(-dt_sin_pri)


# Auxiliary functions -----------------------------------------------------

## Cohen's C ####
my_cohen_d <- function(data, variable, by, ...) {
  sprintf("%.2f", rstatix::cohens_d(data, as.formula(glue::glue("{variable} ~ {by}")))$effsize)
}

## Cramer's V ####
my_cramer_v <- function(data, variable, by, ...) {
  sprintf("%.2f", table(data[[variable]], data[[by]]) |> rstatix::cramer_v())
}


# Descriptive analysis ----------------------------------------------------

## Table 1 - Sociodemographic characteristics ####
tbl01 <- d_srag |> 
  tbl_summary(
    include = c(not_casos, nu_idade_n, raca, escolaridade, vacina, class_gest_puerp),
    label = list(
      not_casos ~ "SARS notifications in the admission day; mean ± sd",
      nu_idade_n ~ "Age (years); mean ± sd",
      raca ~ "Race, N (%)",
      escolaridade ~ "Education level, N (%)",
      vacina ~ "Influenza vaccination, N (%)",
      class_gest_puerp ~ "Gestational stage, N (%)"
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
    label ~ "**Characteristics**",
    p.value ~ "**p**",
    add_stat_1 ~ "**Effect size**"
  ) |> 
  bold_labels() |>
  modify_footnote(update = everything() ~ NA) |> 
  as_gt() |> 
  gt::tab_source_note(
    gt::md(
      "COVID-19: Coronavirus Disease 2019; N: number; p: p-value; sd: standard deviation; %: percentage;
      Effect size given by Cramer’s V, except in the Age variable, where Cohen’s C was used."
    )
  )


## Table 2 - Symptoms ####
tbl02 <- d_srag |> 
  tbl_summary(
    include = c(febre, tosse, garganta, dispneia, desc_resp, saturacao, diarreia),
    label = list(
      febre ~ "Fever", 
      tosse ~ "Cough",
      garganta ~ "Sore throat",
      dispneia ~ "Dyspnea",
      desc_resp ~ "Respiratory discomfort",
      saturacao ~ "O2 saturation < 95%",
      diarreia ~ "Diarrhea"
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
    label ~ "**Symptom, N (%)**", 
    p.value ~ "**p**",
    add_stat_1 ~ "**Effect size**"
  ) |> 
  bold_labels() |> 
  modify_footnote(update = everything() ~ NA) |> 
  as_gt() |> 
  gt::tab_source_note(
    gt::md(
      "COVID-19: Coronavirus Disease 2019; N: number; p: p-value; %: percentage;
      Effect size given by Cramer’s V."
    )
  )


## Table 3 - Comorbidities ####
tbl03 <- d_srag |> 
  tbl_summary(
    include = c(cardiopatia, pneumopatia, renal, obesidade),
    label = list(
      cardiopatia ~ "Heart disease", 
      pneumopatia ~ "Chronic lung disease",
      renal ~ "Chronic kidney disease",
      obesidade ~ "Obesity"
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
    label ~ "**Comorbidity, N (%)**",
    p.value ~ "**p**",
    add_stat_1 ~ "**Effect size**"
  ) |> 
  bold_labels() |> 
  modify_footnote(update = everything() ~ NA) |> 
  as_gt() |> 
  gt::tab_source_note(
    gt::md(
      "COVID-19: Coronavirus Disease 2019; N: number; p: p-value; %: percentage;
      Effect size given by Cramer’s V."
    )
  )


# Tables export -----------------------------------------------------------

gt::gtsave(tbl01, filename = "03.results/tabs/table01.png")
gt::gtsave(tbl02, filename = "03.results/tabs/table02.png")
gt::gtsave(tbl03, filename = "03.results/tabs/table03.png")


# Clean environment -------------------------------------------------------

rm(list = ls())





  
  

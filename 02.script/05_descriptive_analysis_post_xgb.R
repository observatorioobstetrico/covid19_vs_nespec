
# Libraries ---------------------------------------------------------------

# If these packages are not installed on your machine, install them using the command lines:
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("forcats")
# install.packages("lubridate")
# install.packages("gtsummary")

#manipulation
library(dplyr)
library(tidyr)
library(forcats)
library(lubridate)
#descriptive
library(gtsummary)


# Data import -------------------------------------------------------------

# confirmed sars
srag1621 <- readRDS("01.dados/srag_16-21_[all].rds")

# predicted sars
srag1621_pred <- readRDS("01.dados/srag_16-21_[pred].rds")


# Data manipulation -------------------------------------------------------

## Real dataset of SARS ####
d_srag_real <- srag1621 |> 
  mutate(
    class_caso = as.factor(if_else(class_caso == "Unspecified etiological agent", NA_character_, class_caso)), #undefined cases = na
    class_caso = fct_recode(as.factor(class_caso), "COVID-19r"="COVID-19"),
    class_caso = fct_recode(class_caso, "Other confirmed agentsr"="Other confirmed agents")
  ) |>
  rename(class_caso_real_pred = class_caso) |> 
  dplyr::select(
    ano, sg_uf, class_gest_puerp, nu_idade_n, raca, escolaridade, vacina,
    dt_sin_pri, febre, tosse, garganta, dispneia, desc_resp, saturacao, diarreia,
    cardiopatia, pneumopatia, renal, obesidade,
    evolucao, class_caso_real_pred
  ) |>  
  drop_na()

# sars notifications in the admission day
d_not <- d_srag_real |> 
  mutate(dt_sin_pri = as.Date(dt_sin_pri, "%d/%m/%Y")) |> 
  group_by(dt_sin_pri) |> 
  summarise(not_casos = n())

d_srag_real <- d_srag_real |> 
  mutate_at("dt_sin_pri", dmy) |> 
  left_join(d_not, by = "dt_sin_pri")


## Predicted dataset of SARS ####
d_srag_pred <- srag1621_pred |> 
  mutate(
    class_caso_pred = fct_recode(class_caso_pred, "COVID-19p"="COVID-19"),
    class_caso_pred = fct_recode(class_caso_pred, "Other confirmed agentsp"="Other confirmed agents")
  ) |> 
  rename(class_caso_real_pred = class_caso_pred) |> 
  dplyr::select(-class_caso)


## Real and predicted dataset of SARS ####
d_srag_real_pred <- d_srag_real |> 
  full_join(d_srag_pred) |> 
  relocate(ano, sg_uf, not_casos)


# Descriptive analysis ----------------------------------------------------

tbl02 <- d_srag_real_pred |> 
  mutate(
    class_caso_real_pred = fct_recode(
      class_caso_real_pred, 
      "Real COVID-19"="COVID-19r", 
      "Predicted COVID-19"="COVID-19p",
      "Real other confirmed agents"="Other confirmed agentsr", 
      "Predicted other confirmed agents"="Other confirmed agentsp"
    ),
    class_caso_real_pred = fct_relevel(
      class_caso_real_pred, 
      "Real COVID-19", "Predicted COVID-19", 
      "Real other confirmed agents", "Predicted other confirmed agents"
    )
  ) |> 
  tbl_summary(
    include = c(
      not_casos, nu_idade_n, raca, escolaridade, vacina, class_gest_puerp,
      febre, tosse, garganta, dispneia, desc_resp, saturacao, diarreia,
      cardiopatia, pneumopatia, renal, obesidade
    ),
    label = list(
      not_casos ~ "SARS notifications in the admission day; mean ± sd",
      nu_idade_n ~ "Age (years); mean ± sd",
      raca ~ "Race, N (%)",
      escolaridade ~ "Education level, N (%)",
      vacina ~ "Influenza vaccination, N (%)",
      class_gest_puerp ~ "Gestational stage, N (%)",
      febre ~ "Fever, N (%)", 
      tosse ~ "Cough, N (%)",
      garganta ~ "Sore throat, N (%)",
      dispneia ~ "Dyspnea, N (%)",
      desc_resp ~ "Respiratory discomfort, N (%)",
      saturacao ~ "O2 saturation < 95%, N (%)",
      diarreia ~ "Diarrhea, N (%)",
      cardiopatia ~ "Heart disease, N (%)", 
      pneumopatia ~ "Chronic lung disease, N (%)",
      renal ~ "Chronic kidney disease, N (%)",
      obesidade ~ "Obesity, N (%)"
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
  modify_header(label ~ "**Variable**") |> 
  bold_labels() |>
  modify_footnote(update = everything() ~ NA) |> 
  as_gt() |> 
  gt::tab_source_note(
    gt::md(
      "COVID-19: Coronavirus Disease 2019; N: number; 
      sd: standard deviation; %: percentage."
    )
  )


# Final dataset -----------------------------------------------------------

# dataset of unspecified sars (2016-2019)
d_srag_nespec_1619 <- srag1621 |> 
  filter(ano < 2020 & class_caso == "Unspecified etiological agent") |> 
  dplyr::select(
    ano, sg_uf, class_gest_puerp, nu_idade_n, raca, escolaridade, vacina,
    dt_sin_pri, febre, tosse, garganta, dispneia, desc_resp, saturacao, diarreia,
    cardiopatia, pneumopatia, renal, obesidade,
    evolucao, class_caso
  ) |>  
  drop_na() |> 
  rename(class_caso_real_pred = class_caso)

# sars notifications in the admission day
d_not_nespec <- d_srag_nespec_1619 |> 
  mutate(dt_sin_pri = as.Date(dt_sin_pri, "%d/%m/%Y")) |> 
  group_by(dt_sin_pri) |> 
  summarise(not_casos = n())

# dataset of unspecified sars (2016-2019) + sars notifications
d_srag_nespec_1619 <- d_srag_nespec_1619 |> 
  mutate_at("dt_sin_pri", dmy) |> 
  left_join(d_not_nespec, by = "dt_sin_pri")

# final dataset
d_srag_final <- d_srag_real_pred |> 
  full_join(d_srag_nespec_1619) |> 
  mutate(
    class_caso_real_pred = case_when(
      class_caso_real_pred %in% c("COVID-19", "OOther confirmed agents", "Unspecified etiological agent") ~ NA_character_,
      TRUE ~ class_caso_real_pred
    )
  ) |>
  mutate(
    class_caso_final = case_when(
      class_caso_real_pred == "COVID-19r" ~ "COVID-19",
      class_caso_real_pred == "Other confirmed agentsr" ~ "Other confirmed agents",
      class_caso_real_pred == "COVID-19p" ~ "COVID-19",
      class_caso_real_pred == "Other confirmed agentsp" ~ "Other confirmed agents",
      TRUE ~ class_caso_real_pred
    )
  ) |> 
  relocate(class_caso_real_pred, class_caso_final, .after = last_col())
  
saveRDS(d_srag_final, "./01.dados/srag_16-21_[final].rds")


# Table export ------------------------------------------------------------

gt::gtsave(tbl02, filename = "03.results/supplementary/table02.png")


# Clean environment -------------------------------------------------------

rm(list = ls())




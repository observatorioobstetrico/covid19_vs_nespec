
# Libraries ---------------------------------------------------------------

# If these packages are not installed on your machine, install them using the command lines:
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("tidyr")
# install.packages("forcats")
# install.packages("janitor")

#manipulation
library(dplyr) 
library(stringr)
library(tidyr)
library(forcats)
library(janitor)


# Data import -------------------------------------------------------------

# 2016
srag16 <- readr::read_csv2("./01.data/sivep-gripe/INFLUD16.csv", guess_max = 100000) |> 
  select(
    SG_UF, ID_MN_RESI, DT_NOTIFIC, DT_SIN_PRI, CS_SEXO, DT_NASC, NU_IDADE_N, CLASSI_FIN, CS_GESTANT, 
    PUERPERA, CS_RACA, CS_ESCOL_N, VACINA, FEBRE, TOSSE, GARGANTA, DISPNEIA, 
    DESC_RESP, SATURACAO, DIARREIA, CARDIOPATI, PNEUMOPATI, RENAL, OBESIDADE, 
    UTI, SUPORT_VEN, EVOLUCAO
  )

# 2017
srag17 <- readr::read_csv2("./01.data/sivep-gripe/INFLUD17.csv", guess_max = 100000) |> 
  select(
    SG_UF, ID_MN_RESI, DT_NOTIFIC, DT_SIN_PRI, CS_SEXO, DT_NASC, NU_IDADE_N, CLASSI_FIN, CS_GESTANT, 
    PUERPERA, CS_RACA, CS_ESCOL_N, VACINA, FEBRE, TOSSE, GARGANTA, DISPNEIA, 
    DESC_RESP, SATURACAO, DIARREIA, CARDIOPATI, PNEUMOPATI, RENAL, OBESIDADE, 
    UTI, SUPORT_VEN, EVOLUCAO
  )

# 2018
srag18 <- readr::read_csv2("./01.data/sivep-gripe/INFLUD18.csv", guess_max = 100000) |> 
  select(
    SG_UF, ID_MN_RESI, DT_NOTIFIC, DT_SIN_PRI, CS_SEXO, DT_NASC, NU_IDADE_N, CLASSI_FIN, CS_GESTANT, 
    PUERPERA, CS_RACA, CS_ESCOL_N, VACINA, FEBRE, TOSSE, GARGANTA, DISPNEIA, 
    DESC_RESP, SATURACAO, DIARREIA, CARDIOPATI, PNEUMOPATI, RENAL, OBESIDADE, 
    UTI, SUPORT_VEN, EVOLUCAO
  )

# 2019
srag19 <- readr::read_csv2("./01.data/sivep-gripe/INFLUD19.csv", guess_max = 100000) |> 
  select(
    SG_UF, ID_MN_RESI, CO_MUN_RES, DT_SIN_PRI, CS_SEXO, NU_IDADE_N, CLASSI_FIN, 
    CS_GESTANT, PUERPERA, CS_RACA, CS_ESCOL_N, VACINA, FEBRE, TOSSE, GARGANTA, 
    DISPNEIA, DESC_RESP, SATURACAO, DIARREIA, CARDIOPATI, PNEUMOPATI, RENAL, 
    OBESIDADE, UTI, SUPORT_VEN, EVOLUCAO
  )

# 2020
srag20 <- readr::read_csv2("./01.data/sivep-gripe/INFLUD20-01-05-2023.csv", guess_max = 1200000) |> 
  select(
    SG_UF, ID_MN_RESI, CO_MUN_RES, DT_SIN_PRI, CS_SEXO, NU_IDADE_N, CLASSI_FIN, 
    CS_GESTANT, PUERPERA, CS_RACA, CS_ESCOL_N, VACINA, FEBRE, TOSSE, GARGANTA, 
    DISPNEIA, DESC_RESP, SATURACAO, DIARREIA, DOR_ABD, FADIGA, PERD_OLFT, PERD_PALA, 
    DIABETES, CARDIOPATI, PNEUMOPATI, RENAL, OBESIDADE, UTI, SUPORT_VEN, EVOLUCAO
  )

# 2021
srag21 <- readr::read_csv2("./01.data/sivep-gripe/INFLUD21-01-05-2023.csv", guess_max = 2000000) |> 
  select(
    SG_UF, ID_MN_RESI, CO_MUN_RES, DT_SIN_PRI, CS_SEXO, NU_IDADE_N, CLASSI_FIN, 
    CS_GESTANT, PUERPERA, CS_RACA, CS_ESCOL_N, VACINA, FEBRE, TOSSE, GARGANTA,
    DISPNEIA, DESC_RESP, SATURACAO, DIARREIA, DOR_ABD, FADIGA, PERD_OLFT, PERD_PALA, 
    DIABETES, CARDIOPATI, PNEUMOPATI, RENAL, OBESIDADE, VACINA_COV, DOSE_1_COV,
    UTI, SUPORT_VEN, EVOLUCAO
  )


# Data manipulation -------------------------------------------------------

## Merging datasets ####
# 2016 + 2017
srag1617 <- full_join(srag16, srag17)

# 2016 + 2017 + 2018
srag161718 <- full_join(srag1617, srag18) |> 
  # recategoriza variaveis
  mutate(
    # uf
    SG_UF = ifelse(SG_UF == 11, "RO",
             ifelse(SG_UF == 12, "AC",
              ifelse(SG_UF == 13 , "AM",
               ifelse(SG_UF == 14, "RR",
                ifelse(SG_UF == 15, "PA",
                 ifelse(SG_UF == 16, "AP",
                  ifelse(SG_UF == 17, "TO",
                   ifelse(SG_UF == 21, "MA",
                    ifelse(SG_UF == 22, "PI",
                     ifelse(SG_UF == 23, "CE",
                      ifelse(SG_UF == 24, "RN",
                       ifelse(SG_UF == 25, "PB",
                        ifelse(SG_UF == 26, "PE",
                         ifelse(SG_UF == 27, "AL",
                          ifelse(SG_UF == 28, "SE",
                           ifelse(SG_UF == 29, "BA",
                            ifelse(SG_UF == 31, "MG",
                             ifelse(SG_UF == 32, "ES",
                              ifelse(SG_UF == 33, "RJ",
                               ifelse(SG_UF == 35, "SP",
                                ifelse(SG_UF == 41, "PR",
                                 ifelse(SG_UF == 42, "SC",
                                  ifelse(SG_UF == 43, "RS",
                                   ifelse(SG_UF == 50, "MS",
                                    ifelse(SG_UF == 51, "MT",
                                     ifelse(SG_UF == 52, "GO",
                                      ifelse(SG_UF == 53, "DF", SG_UF))))))))))))))))))))))))))),
    #idade
    NU_IDADE_N = ifelse(str_sub(NU_IDADE_N, end = 1) == "1", (as.numeric(str_sub(NU_IDADE_N, start = 2)) / 8760),
                   ifelse(str_sub(NU_IDADE_N, end = 1) == "2", (as.numeric(str_sub(NU_IDADE_N, start = 2)) / 365.25),
                     ifelse(str_sub(NU_IDADE_N, end = 1) == "3", (as.numeric(str_sub(NU_IDADE_N, start = 2)) / 12), as.numeric(str_sub(NU_IDADE_N, start = 2))))),
    #escolaridade
    CS_ESCOL_N = ifelse(CS_ESCOL_N == 0, 0,
                   ifelse(CS_ESCOL_N == 1, 1,
                     ifelse(CS_ESCOL_N == 2, 3,
                       ifelse(CS_ESCOL_N == 3, 4, 
                         ifelse(CS_ESCOL_N == 10, 5, CS_ESCOL_N)))))
  )

# 2016 + 2017 + 2018 + 2019
srag16171819 <- full_join(srag161718 |> mutate_at("ID_MN_RESI", as.character), srag19)

# 2016 + 2017 + 2018 + 2019 + 2020
srag1617181920 <- full_join(srag16171819, srag20)

# merge 2016, 2017, 2018, 2019, 2020 and 2021
srag161718192021 <- full_join(srag1617181920, srag21)


## Recategorizing variables ####
srag161718192021 <- srag161718192021 |> 
  clean_names() |> 
  filter(
    # symptom data from 16/01 to 21/11
    as.Date(dt_sin_pri, format = "%d/%m/%Y") < as.Date("01-12-2021", format = "%d-%m-%Y"),
    # female sex
    cs_sexo == "F",
    # age between 10 and 55 years
    nu_idade_n > 9 & nu_idade_n < 56
  ) |> 
  # demographic variables
  mutate(
    # year
    ano = str_sub(dt_sin_pri, start = 7),
    # gestational stage
    class_gest_puerp = case_when(
      cs_gestant == 1 ~ "First trimester", 
      cs_gestant == 2 ~ "Second trimester",
      cs_gestant == 3 ~ "Third trimester", 
      cs_gestant == 4 ~ "Ignored",
      cs_gestant == 5 & puerpera == 1 ~ "Postpartum",
      cs_gestant == 9 & puerpera == 1 ~ "Postpartum",
      TRUE ~ NA_character_
    ),
    # race
    raca = case_when(
      cs_raca == 1 ~ "White",
      cs_raca %in% c(2, 3, 4, 5) ~ "Non-white",
      cs_raca == 9 ~ "Ignored",
      TRUE ~ "Blank"
    ),
    # education level
    escolaridade = case_when(
      cs_escol_n %in% c(0, 1, 2) ~ "Up to elementary",
      cs_escol_n == 3 ~ "High school", 
      cs_escol_n == 4 ~ "Higher education",
      cs_escol_n == 9 ~ "Ignored",
      TRUE ~ "Blank"
    ),
    # influenza vaccination
    vacina = case_when(
      vacina == 1  ~ "Yes", 
      vacina == 2 ~ "No",
      vacina == 9 ~ "Ignored",
      TRUE ~ "Blank"
    ),
    # covid-19 vaccination
    vacina_cov19 = case_when(
      vacina_cov == 1 | !is.na(dose_1_cov) ~ "Yes",
      vacina_cov == 2 ~ "No",
      vacina_cov == 9 ~ "Ignored",
      TRUE ~ "Blank"
    )
  ) |> 
  # symptom variables
  mutate(
    # fever
    febre = case_when(
      febre == 1 ~ "Yes", 
      febre == 2 ~ "No",
      febre == 9 ~ "Ignored",
      TRUE ~ "Blank"
    ),
    # cough
    tosse = case_when(
      tosse == 1 ~ "Yes", 
      tosse == 2 ~ "No",
      tosse == 9 ~ "Ignored",
      TRUE ~ "Blank"
    ),
    # throat
    garganta = case_when(
      garganta == 1 ~ "Yes", 
      garganta == 2 ~ "No",
      garganta == 9 ~ "Ignored",
      TRUE ~ "Blank"
    ),
    # dyspnea
    dispneia = case_when(
      dispneia == 1 ~ "Yes", 
      dispneia == 2 ~ "No",
      dispneia == 9 ~ "Ignored",
      TRUE ~ "Blank"
    ),
    # respiratory discomfort
    desc_resp = case_when(
      desc_resp == 1 ~ "Yes", 
      desc_resp == 2 ~ "No",
      desc_resp == 9 ~ "Ignored",
      TRUE ~ "Blank"
    ),
    # saturation
    saturacao = case_when(
      saturacao == 1 ~ "Yes", 
      saturacao == 2 ~ "No",
      saturacao == 9 ~ "Ignored",
      TRUE ~ "Blank"
    ),
    # diarrhea
    diarreia = case_when(
      diarreia == 1 ~ "Yes", 
      diarreia == 2 ~ "No",
      diarreia == 9 ~ "Ignored",
      TRUE ~ "Blank"
    )
  ) |> 
  # comorbities variables
  mutate(
    # heart disease
    cardiopatia = case_when(
      cardiopati == 1 ~ "Yes", 
      cardiopati == 2 ~ "No",
      cardiopati == 9 ~ "Ignored",
      TRUE ~ "Blank"
    ),
    # lung disease
    pneumopatia = case_when(
      pneumopati == 1 ~ "Yes", 
      pneumopati == 2 ~ "No",
      pneumopati == 9 ~ "Ignored",
      TRUE ~ "Blank"
    ),
    # kidney diseas
    renal = case_when(
      renal == 1 ~ "Yes", 
      renal == 2 ~ "No",
      renal == 9 ~ "Ignored",
      TRUE ~ "Blank"
    ),
    # obesity
    obesidade = case_when(
      obesidade == 1 ~ "Yes", 
      obesidade == 2 ~ "No",
      obesidade == 9 ~ "Ignored",
      TRUE ~ "Blank"
    )
  ) |> 
  # severe sars
  mutate(
    # evolution of case
    evolucao = case_when(
      evolucao == 1  ~ "Cure",
      evolucao %in% c(2, 3) ~ "Death",
      evolucao == 9 ~ "Ignored",
      TRUE ~ "Blank"
    )
  ) |> 
  # response variable - final classification
  mutate(
    class_caso = as.factor(
      case_when(
        classi_fin %in% c(1, 2, 3) ~ "Other confirmed agents",
        classi_fin == 5 ~ "COVID-19",
        TRUE ~ "Unspecified etiological agent"
      )
    ),
    # reference category: covid-19
    class_caso = fct_relevel(class_caso, "COVID-19")
  )


## Reordering variables ####
srag161718192021 <- srag161718192021 |> 
  mutate(
    # gestational stage
    class_gest_puerp = fct_relevel(
      as.factor(class_gest_puerp),
      "First trimester", "Second trimester", "Third trimester", "Postpartum", "Ignored"
    ),
    # race
    raca = fct_relevel(
      as.factor(raca),
      "White", "Non-white", "Blank", "Ignored", 
    ),
    # education level
    escolaridade = fct_relevel(
      as.factor(escolaridade),
      "Up to elementary", "High school", "Higher education", "Blank", "Ignored" 
    ),
    # influenza vaccination
    vacina = fct_relevel(
      as.factor(vacina),
      "Yes", "No", "Blank", "Ignored", 
    ),
    # covid-19 vaccination
    vacina_cov19 = fct_relevel(
      as.factor(vacina_cov19),
      "Yes", "No", "Blank", "Ignored", 
    ),
    # fever
    febre = fct_relevel(
      as.factor(febre),
      "Yes", "No", "Blank", "Ignored", 
    ), 
    # cough
    tosse = fct_relevel(
      as.factor(tosse),
      "Yes", "No", "Blank", "Ignored", 
    ),
    # throat
    garganta = fct_relevel(
      as.factor(garganta),
      "Yes", "No", "Blank", "Ignored", 
    ),
    # dyspnea
    dispneia = fct_relevel(
      as.factor(dispneia),
      "Yes", "No", "Blank", "Ignored", 
    ),
    # respiratory discomfort
    desc_resp = fct_relevel(
      as.factor(desc_resp),
      "Yes", "No", "Blank", "Ignored", 
    ),
    # saturation
    saturacao = fct_relevel(
      as.factor(saturacao),
      "Yes", "No", "Blank", "Ignored", 
    ),
    # diarrhea
    diarreia = fct_relevel(
      as.factor(diarreia),
      "Yes", "No", "Blank", "Ignored", 
    ),
    # heart disease
    cardiopatia = fct_relevel(
      as.factor(cardiopatia),
      "Yes", "No", "Blank", "Ignored", 
    ),
    # lung desease
    pneumopatia = fct_relevel(
      as.factor(pneumopatia),
      "Yes", "No", "Blank", "Ignored", 
    ),
    # kidney disease
    renal = fct_relevel(
      as.factor(renal),
      "Yes", "No", "Blank", "Ignored", 
    ),
    # obesity
    obesidade = fct_relevel(
      as.factor(obesidade),
      "Yes", "No", "Blank", "Ignored", 
    ),
    # evolution
    evolucao = fct_relevel(
      as.factor(evolucao),
      "Cure", "Death", "Blank", "Ignored", 
    )
  ) 


# Data export -------------------------------------------------------------

saveRDS(srag161718192021, "01.data/srag_16-21_[all].rds")


# Clean environment -------------------------------------------------------

rm(list = ls())


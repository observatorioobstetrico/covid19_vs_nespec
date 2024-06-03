
# Bibliotecas -------------------------------------------------------------

#manipulacao
library(dplyr)
library(stringr)
library(tidyr)
library(forcats)
library(janitor)


# Importacao dos dados ----------------------------------------------------

# 2016
srag16 <- readr::read_csv2("./01.dados/sivep-gripe/INFLUD16.csv", guess_max = 100000) |> 
  select(
    SG_UF, ID_MN_RESI, DT_NOTIFIC, DT_SIN_PRI, CS_SEXO, DT_NASC, NU_IDADE_N, CLASSI_FIN, CS_GESTANT, 
    PUERPERA, CS_RACA, CS_ESCOL_N, VACINA, FEBRE, TOSSE, GARGANTA, DISPNEIA, 
    DESC_RESP, SATURACAO, DIARREIA, CARDIOPATI, PNEUMOPATI, RENAL, OBESIDADE, 
    UTI, SUPORT_VEN, EVOLUCAO
  )

# 2017
srag17 <- readr::read_csv2("./01.dados/sivep-gripe/INFLUD17.csv", guess_max = 100000) |> 
  select(
    SG_UF, ID_MN_RESI, DT_NOTIFIC, DT_SIN_PRI, CS_SEXO, DT_NASC, NU_IDADE_N, CLASSI_FIN, CS_GESTANT, 
    PUERPERA, CS_RACA, CS_ESCOL_N, VACINA, FEBRE, TOSSE, GARGANTA, DISPNEIA, 
    DESC_RESP, SATURACAO, DIARREIA, CARDIOPATI, PNEUMOPATI, RENAL, OBESIDADE, 
    UTI, SUPORT_VEN, EVOLUCAO
  )

# 2018
srag18 <- readr::read_csv2("./01.dados/sivep-gripe/INFLUD18.csv", guess_max = 100000) |> 
  select(
    SG_UF, ID_MN_RESI, DT_NOTIFIC, DT_SIN_PRI, CS_SEXO, DT_NASC, NU_IDADE_N, CLASSI_FIN, CS_GESTANT, 
    PUERPERA, CS_RACA, CS_ESCOL_N, VACINA, FEBRE, TOSSE, GARGANTA, DISPNEIA, 
    DESC_RESP, SATURACAO, DIARREIA, CARDIOPATI, PNEUMOPATI, RENAL, OBESIDADE, 
    UTI, SUPORT_VEN, EVOLUCAO
  )

# 2019
srag19 <- readr::read_csv2("./01.dados/sivep-gripe/INFLUD19.csv", guess_max = 100000) |> 
  select(
    SG_UF, ID_MN_RESI, CO_MUN_RES, DT_SIN_PRI, CS_SEXO, NU_IDADE_N, CLASSI_FIN, 
    CS_GESTANT, PUERPERA, CS_RACA, CS_ESCOL_N, VACINA, FEBRE, TOSSE, GARGANTA, 
    DISPNEIA, DESC_RESP, SATURACAO, DIARREIA, CARDIOPATI, PNEUMOPATI, RENAL, 
    OBESIDADE, UTI, SUPORT_VEN, EVOLUCAO
  )

# 2020
srag20 <- readr::read_csv2("./01.dados/sivep-gripe/INFLUD20-01-05-2023.csv", guess_max = 1200000) |> 
  select(
    SG_UF, ID_MN_RESI, CO_MUN_RES, DT_SIN_PRI, CS_SEXO, NU_IDADE_N, CLASSI_FIN, 
    CS_GESTANT, PUERPERA, CS_RACA, CS_ESCOL_N, VACINA, FEBRE, TOSSE, GARGANTA, 
    DISPNEIA, DESC_RESP, SATURACAO, DIARREIA, DOR_ABD, FADIGA, PERD_OLFT, PERD_PALA, 
    DIABETES, CARDIOPATI, PNEUMOPATI, RENAL, OBESIDADE, UTI, SUPORT_VEN, EVOLUCAO
  )

# 2021
srag21 <- readr::read_csv2("./01.dados/sivep-gripe/INFLUD21-01-05-2023.csv", guess_max = 2000000) |> 
  select(
    SG_UF, ID_MN_RESI, CO_MUN_RES, DT_SIN_PRI, CS_SEXO, NU_IDADE_N, CLASSI_FIN, 
    CS_GESTANT, PUERPERA, CS_RACA, CS_ESCOL_N, VACINA, FEBRE, TOSSE, GARGANTA,
    DISPNEIA, DESC_RESP, SATURACAO, DIARREIA, DOR_ABD, FADIGA, PERD_OLFT, PERD_PALA, 
    DIABETES, CARDIOPATI, PNEUMOPATI, RENAL, OBESIDADE, VACINA_COV, DOSE_1_COV,
    UTI, SUPORT_VEN, EVOLUCAO
  )


# Manipulacao dos dados ---------------------------------------------------

## Uniao de bases ####

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


## Recategorizacao de variaveis ####
srag161718192021 <- srag161718192021 |> 
  clean_names() |> 
  filter(
    # data de sintomas de 01/16 a 11/21
    as.Date(dt_sin_pri, format = "%d/%m/%Y") < as.Date("01-12-2021", format = "%d-%m-%Y"),
    # sexo feminino
    cs_sexo == "F",
    # idades entre 10 e 55
    nu_idade_n > 9 & nu_idade_n < 56
  ) |> 
  # variaveis demograficas
  mutate(
    # ano
    ano = str_sub(dt_sin_pri, start = 7),
    # idade gestacional
    class_gest_puerp = case_when(
      cs_gestant == 1 ~ "1° trimestre", 
      cs_gestant == 2 ~ "2° trimestre",
      cs_gestant == 3 ~ "3° trimestre", 
      cs_gestant == 4 ~ "Ignorado",
      cs_gestant == 5 & puerpera == 1 ~ "Puerpério",
      cs_gestant == 9 & puerpera == 1 ~ "Puerpério",
      TRUE ~ NA_character_
    ),
    # raca
    raca = case_when(
      cs_raca == 1 ~ "Branca",
      cs_raca %in% c(2, 3, 4, 5) ~ "Não branca",
      cs_raca == 9 ~ "Ignorado",
      TRUE ~ "Em branco"
    ),
    # escolaridade
    escolaridade = case_when(
      cs_escol_n %in% c(0, 1, 2) ~ "Até fundamental",
      cs_escol_n == 3 ~ "Ensino médio", 
      cs_escol_n == 4 ~ "Ensino superior",
      cs_escol_n == 9 ~ "Ignorado",
      TRUE ~ "Em branco"
    ),
    # vacina de influenza
    vacina = case_when(
      vacina == 1  ~ "Sim", 
      vacina == 2 ~ "Não",
      vacina == 9 ~ "Ignorado",
      TRUE ~ "Em branco"
    ),
    # vacina de covid-19
    vacina_cov19 = case_when(
      vacina_cov == 1 | !is.na(dose_1_cov) ~ "Sim",
      vacina_cov == 2 ~ "Não",
      vacina_cov == 9 ~ "Ignorado",
      TRUE ~ "Em branco"
    )
  ) |> 
  # variaveis de sintomas
  mutate(
    # febre
    febre = case_when(
      febre == 1 ~ "Sim", 
      febre == 2 ~ "Não",
      febre == 9 ~ "Ignorado",
      TRUE ~ "Em branco"
    ),
    # tosse
    tosse = case_when(
      tosse == 1 ~ "Sim", 
      tosse == 2 ~ "Não",
      tosse == 9 ~ "Ignorado",
      TRUE ~ "Em branco"
    ),
    # garganta
    garganta = case_when(
      garganta == 1 ~ "Sim", 
      garganta == 2 ~ "Não",
      garganta == 9 ~ "Ignorado",
      TRUE ~ "Em branco"
    ),
    # dispneia
    dispneia = case_when(
      dispneia == 1 ~ "Sim", 
      dispneia == 2 ~ "Não",
      dispneia == 9 ~ "Ignorado",
      TRUE ~ "Em branco"
    ),
    # desconforto respiratorio
    desc_resp = case_when(
      desc_resp == 1 ~ "Sim", 
      desc_resp == 2 ~ "Não",
      desc_resp == 9 ~ "Ignorado",
      TRUE ~ "Em branco"
    ),
    # saturacao
    saturacao = case_when(
      saturacao == 1 ~ "Sim", 
      saturacao == 2 ~ "Não",
      saturacao == 9 ~ "Ignorado",
      TRUE ~ "Em branco"
    ),
    # diarreia
    diarreia = case_when(
      diarreia == 1 ~ "Sim", 
      diarreia == 2 ~ "Não",
      diarreia == 9 ~ "Ignorado",
      TRUE ~ "Em branco"
    )
  ) |> 
  # variaveis de comorbidades
  mutate(
    # cardiopatia
    cardiopatia = case_when(
      cardiopati == 1 ~ "Sim", 
      cardiopati == 2 ~ "Não",
      cardiopati == 9 ~ "Ignorado",
      TRUE ~ "Em branco"
    ),
    # pneuemonia
    pneumopatia = case_when(
      pneumopati == 1 ~ "Sim", 
      pneumopati == 2 ~ "Não",
      pneumopati == 9 ~ "Ignorado",
      TRUE ~ "Em branco"
    ),
    # doenca renal
    renal = case_when(
      renal == 1 ~ "Sim", 
      renal == 2 ~ "Não",
      renal == 9 ~ "Ignorado",
      TRUE ~ "Em branco"
    ),
    # obesidade
    obesidade = case_when(
      obesidade == 1 ~ "Sim", 
      obesidade == 2 ~ "Não",
      obesidade == 9 ~ "Ignorado",
      TRUE ~ "Em branco"
    )
  ) |> 
  # srag grave
  mutate(
    # evolucao do caso
    evolucao = case_when(
      evolucao == 1  ~ "Cura",
      evolucao %in% c(2, 3) ~ "Óbito",
      evolucao == 9 ~ "Ignorado",
      TRUE ~ "Em branco"
    )
  ) |> 
  # variavel resposta - classificacao final do caso
  mutate(
    # classificacao do caso
    class_caso = as.factor(
      case_when(
        classi_fin %in% c(1, 2, 3) ~ "Outros agentes confirmados",
        classi_fin == 5 ~ "COVID-19",
        TRUE ~ "Agente etiológico não especificado"
      )
    ),
    # categoria de referencia: covid-19
    class_caso = fct_relevel(class_caso, "COVID-19")
  )


## Reordenacao de variaveis ####
srag161718192021 <- srag161718192021 |> 
  mutate(
    # momento gestacional
    class_gest_puerp = fct_relevel(
      as.factor(class_gest_puerp),
      "1° trimestre", "2° trimestre", "3° trimestre", "Puerpério", "Ignorado"
    ),
    # raca
    raca = fct_relevel(
      as.factor(raca),
      "Branca", "Não branca", "Em branco", "Ignorado", 
    ),
    # escolaridade
    escolaridade = fct_relevel(
      as.factor(escolaridade),
      "Até fundamental", "Ensino médio", "Ensino superior", "Em branco", "Ignorado" 
    ),
    # vacina de influenza
    vacina = fct_relevel(
      as.factor(vacina),
      "Sim", "Não", "Em branco", "Ignorado", 
    ),
    # vacina de covid-19
    vacina_cov19 = fct_relevel(
      as.factor(vacina_cov19),
      "Sim", "Não", "Em branco", "Ignorado", 
    ),
    #  febre
    febre = fct_relevel(
      as.factor(febre),
      "Sim", "Não", "Em branco", "Ignorado", 
    ), 
    # tosse
    tosse = fct_relevel(
      as.factor(tosse),
      "Sim", "Não", "Em branco", "Ignorado", 
    ),
    # garganta
    garganta = fct_relevel(
      as.factor(garganta),
      "Sim", "Não", "Em branco", "Ignorado", 
    ),
    # dispneia
    dispneia = fct_relevel(
      as.factor(dispneia),
      "Sim", "Não", "Em branco", "Ignorado", 
    ),
    # desconforto respiratorio
    desc_resp = fct_relevel(
      as.factor(desc_resp),
      "Sim", "Não", "Em branco", "Ignorado", 
    ),
    # saturacao
    saturacao = fct_relevel(
      as.factor(saturacao),
      "Sim", "Não", "Em branco", "Ignorado", 
    ),
    # diarreia
    diarreia = fct_relevel(
      as.factor(diarreia),
      "Sim", "Não", "Em branco", "Ignorado", 
    ),
    # cardiopatia
    cardiopatia = fct_relevel(
      as.factor(cardiopatia),
      "Sim", "Não", "Em branco", "Ignorado", 
    ),
    # pneumopatia
    pneumopatia = fct_relevel(
      as.factor(pneumopatia),
      "Sim", "Não", "Em branco", "Ignorado", 
    ),
    # doenca renal
    renal = fct_relevel(
      as.factor(renal),
      "Sim", "Não", "Em branco", "Ignorado", 
    ),
    # obesidade
    obesidade = fct_relevel(
      as.factor(obesidade),
      "Sim", "Não", "Em branco", "Ignorado", 
    ),
    # evolucao
    evolucao = fct_relevel(
      as.factor(evolucao),
      "Cura", "Óbito", "Em branco", "Ignorado", 
    )
  ) 


# Exportacao dos dados ----------------------------------------------------

saveRDS(srag161718192021, "01.dados/srag_16-21_[all].rds")


# Limpa ambiente ----------------------------------------------------------

rm(list = ls())



# Bibliotecas -------------------------------------------------------------

#manipulacao
library(dplyr)
library(lubridate)
#modelo
library(tidymodels)
library(xgboost)
#interpretabilidade
library(shapviz)
#grafico
library(ggplot2)


# Importacao dos dados ----------------------------------------------------

srag1621 <- readRDS("01.dados/srag_16-21_[all].rds")


# Manipulacao dos dados ---------------------------------------------------

d_srag <- srag1621 |> 
  # casos indefinidos = na
  mutate(
    class_caso = as.factor(
      if_else(class_caso == "Agente etiológico não especificado", NA_character_, class_caso)
    )
  ) |> 
  select(
    class_gest_puerp, nu_idade_n, raca, escolaridade, vacina,
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


# Modelo XGBoost ----------------------------------------------------------

## Treino e teste ####
set.seed(2625)

# split; treino; teste
d_sp <- initial_split(d_srag); d_tr <- training(d_sp); d_ts <- testing(d_sp)


## Receita ####
xgb_rec <- recipe(class_caso ~ ., data = d_tr) |> 
  themis::step_smotenc(class_caso, over_ratio = 0.5) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

xgb_prep <- prep(xgb_rec) |> 
  juice() |> 
  select(-class_caso)


## Especificacao do modelo ####
xgb_espec <- boost_tree(
  trees = 1000,
  tree_depth = 9, min_n = 11,
  loss_reduction = 0.00157,     
  sample_size = 0.224, mtry = 16,   
  learn_rate = 0.0776,          
) |> 
  set_engine("xgboost") |> 
  set_mode("classification")


## Ajuste ####
xgb_aj <- workflow() |> 
  add_recipe(xgb_rec) |> 
  add_model(xgb_espec) |> 
  fit(d_tr)


# Interpretabilidade - SHAP -----------------------------------------------

shap <- shapviz(extract_fit_engine(xgb_aj), X_pred = data.matrix(xgb_prep))

## Grafico de resumo ####
g1 <- sv_importance(shap, kind = "beeswarm", show_numbers = TRUE, alpha = .3, max_display = 10) +
  scale_x_continuous(limits = c(-7, 7), breaks = seq(-7, 7, 2)) +
  scale_colour_gradient(
    low = "#0096ff", high = "#e0115f",
    breaks = c(0, 1), labels = c("Low", "High")
  ) +
  labs(x = "SHAP value (impact on model output)", colour = "Feature value") +
  theme_classic() +
  theme(
    axis.line.y = element_blank(),
    panel.grid.minor.y = element_line(),
    panel.grid.major.y = element_line(color = "#f8f9f9"),
    text = element_text(size = 20),
    legend.key.height = unit(4, 'cm'),
    legend.key.width = unit(.2, 'cm'),
    legend.title.position = "right",
    legend.title = element_text(angle = 90, hjust = 0.5)
  )

g2 <- g1 + 
  geom_label(
    aes(x, y, label = round(as.numeric(label), 2)), 
    data = layer_data(g1, 3), fontface = "bold"
  ) +
  scale_y_discrete(
    labels = c(
      "Respiratory discomfort_Yes",
      "Influenza vaccine_No",
      "Oxygen saturation < 95%_No",
      "Gestational moment_ 2nd quarter",
      "Cardiopathy_No",
      "Cough_Yes",
      "Obesity_Blank",
      "Age",
      "Diarrhea_Blank",
      "SARS notification in the\nadmission day"
    )
  )

# remove geom_text( )
g2$layers[[3]] <- NULL; g2

ggsave("03.resultados/modelo/tiff/g_res.tiff", width = 16, height = 10, dpi = 700)


## Grafico de importancia ####
sv_importance(shap, kind = "bar", show_numbers = TRUE, max_display = 10, fill = "#0096ff") +
  scale_x_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, 0.5)) +
  labs(x = "Mean of |SHAP value|") +
  theme_bw()

ggsave("03.resultados/modelo/g_imp.png", width = 16, height = 10)


# Limpa ambiente ----------------------------------------------------------

rm(list = ls())

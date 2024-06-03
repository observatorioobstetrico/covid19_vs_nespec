
# Bibliotecas -------------------------------------------------------------

#manipulacao
library(lubridate)
#modelo
library(tidymodels)
library(xgboost)
#tabela
library(gt)
#graficos
library(ggplot2)


# Importacao dos dados ----------------------------------------------------

srag1621 <- readRDS("01.dados/srag_16-21_[all].rds")


# Manipulacao dos dados ---------------------------------------------------

## Base de casos definidos ####
d_srag_def <- srag1621 |> 
  # casos indefinidos = na
  mutate(
    class_caso = as.factor(
      if_else(class_caso == "Agente etiológico não especificado", NA_character_, class_caso)
    )
  ) |>
  select(
    sg_uf, class_gest_puerp, nu_idade_n, raca, escolaridade, vacina,
    dt_sin_pri, febre, tosse, garganta, dispneia, desc_resp, saturacao, diarreia,
    cardiopatia, pneumopatia, renal, obesidade,
    class_caso
  ) |>  
  drop_na()

# numero de notificacoes de srag por dia
d_not <- d_srag_def |> 
  mutate(dt_sin_pri = as.Date(dt_sin_pri, "%d/%m/%Y")) |> 
  group_by(dt_sin_pri) |> 
  summarise(not_casos = n())

d_srag_def <- d_srag_def |> 
  mutate_at("dt_sin_pri", dmy) |> 
  left_join(d_not, by = "dt_sin_pri") |> 
  select(-c(dt_sin_pri, sg_uf))


## Base de casos indefinidos ####
d_srag_ind <- srag1621 |> 
  filter(ano > 2019 & class_caso == "Agente etiológico não especificado") |> 
  mutate(dt_sin_pri = as.Date(dt_sin_pri, "%d/%m/%Y")) |> 
  select(
    ano, sg_uf, class_gest_puerp, nu_idade_n, raca, escolaridade, vacina,
    dt_sin_pri, febre, tosse, garganta, dispneia, desc_resp, saturacao, diarreia,
    cardiopatia, pneumopatia, renal, obesidade,
    evolucao, class_caso
  ) |>  
  drop_na()

# numero de notificacoes de srag por dia
d_not <- d_srag_ind |> 
  mutate(dt_sin_pri = as.Date(dt_sin_pri, "%d/%m/%Y")) |> 
  group_by(dt_sin_pri) |> 
  summarise(not_casos = n())

d_srag_ind <- d_srag_ind |> 
  left_join(d_not, by = "dt_sin_pri") |> 
  relocate(ano, sg_uf, not_casos)


# XGBoost -----------------------------------------------------------------

## Treino e teste ####
set.seed(2625)

d_sp <- initial_split(d_srag_def)

# treino
d_tr <- training(d_sp)

d_tr <- recipe(class_caso ~ ., data = d_tr) |> 
  themis::step_smotenc(class_caso, over_ratio = 0.5) |> 
  prep() |> 
  bake(new_data = NULL)

# teste
d_ts <- testing(d_sp)


## Validacao cruzada ####
set.seed(2624)

xgb_folds <- vfold_cv(d_tr, v = 10, repeats = 3)


## Especificacao do modelo ####
xgb_espec <- boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     
  sample_size = tune(), mtry = tune(),        
  learn_rate = tune(),                        
) |> 
  set_engine("xgboost") |>  
  set_mode("classification")


## Grid search ####
xgb_grid <- grid_max_entropy(
  tree_depth(), min_n(),
  loss_reduction(), sample_size = sample_prop(),
  finalize(mtry(), d_tr),
  learn_rate(), size = 30
)


## Workflow ####
xgb_wf <- workflow() |> 
  add_formula(class_caso ~ .) |> 
  add_model(xgb_espec)


## Tune ####
doParallel::registerDoParallel(); set.seed(3533)

xgb_tun <- tune_grid(
  xgb_wf,
  resamples = xgb_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

#saveRDS(xgb_tun, "xgb_tun_xgb.rds")
xgb_tun <- readRDS("02.codigos/xgb_tun_xgb.rds")


## Melhores hiperparametros ####
xgb_hips <- select_best(xgb_tun, metric = "roc_auc"); xgb_hips

#mtry: 16; min_n: 11; tree_depth: 9; learn_rate: 0.0776; 
#loss_reduction: 0.00157; sample_size: 0.224


## Modelo final ####
xgb_wf_final <- xgb_wf |> finalize_workflow(xgb_hips)

xgb_final <- xgb_wf_final |> last_fit(d_sp)


## Analise de desempenho ####
# matriz de confusão
conf_mat(xgb_final |> collect_predictions(), class_caso, .pred_class) |> 
  autoplot("heatmap") +
    labs(x = "Verdadeiro", y = "Predito")

#ggsave("03.resultados/modelo/g_mzc.png", width = 16, height = 10)

# medidas de desempenho
summary(conf_mat(xgb_final |> collect_predictions(), class_caso, .pred_class)) |> 
  select(-.estimator) |> 
  rename(Medida = .metric, `Valor (c/ SMOTE)` = .estimate) |> 
  gt() |> 
    tab_style(
      style = cell_fill(color = "#ffffe0"),
      locations = cells_body(rows = c(1, 3, 4))
    ) |> 
    fmt_number(columns = 2, decimals = 4)

#gtsave(tbl_med, filename = "03.resultados/modelo/t_med_xgb.png")

# curva roc
xgb_final |> 
  collect_predictions() |> 
  roc_curve(class_caso, `.pred_COVID-19`) |> 
  autoplot() +
    labs(x = "1 - Especificidade", y = "Sensibilidade") +
    annotate("text", x = .37, y = .67, label = "AUC = 0.96")

#ggsave("03.resultados/modelo/g_roc.png", width = 16, height = 10)


## Predicao ####
# ajuste
xgb_aj <- xgb_wf_final |> fit(d_srag_def)

# predicao
xgb_pd <- predict(xgb_aj, d_srag_ind)


# Base com predicao -------------------------------------------------------

d_srag_com_pred <- d_srag_ind |> 
  mutate(class_caso_pred = xgb_pd$.pred_class)


# Exportacao dos dados ----------------------------------------------------

saveRDS(d_srag_com_pred, "01.dados/srag_16-21_[pred].rds")


# Limpa ambiente ----------------------------------------------------------

rm(list = ls())
  






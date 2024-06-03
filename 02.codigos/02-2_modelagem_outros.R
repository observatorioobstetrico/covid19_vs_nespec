
# Bibliotecas -------------------------------------------------------------

#manipulacao
library(dplyr)
library(lubridate)
#modelo
library(tidymodels)
library(kknn) #knn
library(glmnet) #lasso
library(discrim) #lda
library(sparsediscrim) #lda
library(kernlab) #svm
library(baguette) #bagging
#tabela
library(gt)


# Importacao dos dados ----------------------------------------------------

srag1621 <- readRDS("01.dados/srag_16-21_[all].rds")


# Manipulacao dos dados ---------------------------------------------------

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


# Teste, treino e validacao cruzada ---------------------------------------

## Treino e teste ####
set.seed(2625)

d_sp <- initial_split(d_srag_def)

# treino
d_tr <- training(d_sp)

# teste
d_ts <- testing(d_sp)


## Validacao cruzada ####
set.seed(2624); folds <- vfold_cv(d_tr, v = 10, repeats = 3)


# Analise discriminante linear --------------------------------------------

## Especificacao do modelo ####
adl_espec <- discrim_linear() |> 
  set_engine("MASS")


## Workflow ####
adl_wf <- workflow() |> 
  add_formula(class_caso ~ .) |> 
  add_model(adl_espec)


## Last fit ####
adl_final <- adl_wf |> last_fit(d_sp)


## Medidas de desempenho ####
adl_final |> collect_metrics() #auc: 0.919

summary(conf_mat(adl_final |> collect_predictions(), class_caso, .pred_class)) |> 
  dplyr::select(-.estimator) |> 
  rename(Medida = .metric, Valor = .estimate) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = "#ffffe0"),
    locations = cells_body(rows = c(1, 3, 4))
  ) |> 
  fmt_number(columns = 2, decimals = 4)


# Analise discriminante quadratica ----------------------------------------

## Especificacao do modelo ####
adl_quad_espec <- discrim_quad() |> 
  set_engine("sparsediscrim")


## Workflow ####
adl_quad_wf <- workflow() |> 
  add_formula(class_caso ~ .) |> 
  add_model(adl_quad_espec)


## Last fit ####
adl_quad_final <- adl_quad_wf |> last_fit(d_sp)


## Medidas de desempenho ####
adl_quad_final |> collect_metrics() #auc: 0.863

summary(conf_mat(adl_quad_final |> collect_predictions(), class_caso, .pred_class)) |> 
  dplyr::select(-.estimator) |> 
  rename(Medida = .metric, Valor = .estimate) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = "#ffffe0"),
    locations = cells_body(rows = c(1, 3, 4))
  ) |> 
  fmt_number(columns = 2, decimals = 4)


# Arvores de classificacao ------------------------------------------------

## Especificacao do modelo ####
tree_espec <- decision_tree(
  cost_complexity = tune(), 
  tree_depth = tune(), min_n = tune()
) |> 
  set_engine("rpart") |> 
  set_mode("classification")


## Grid search ####
tree_grid <- grid_regular(
  cost_complexity(), tree_depth(), min_n(), 
  levels = 4
)


## Workflow ####
tree_wf <- workflow() |> 
  add_formula(class_caso ~ .) |> 
  add_model(tree_espec)


## Tune ####
doParallel::registerDoParallel(); set.seed(3533)

tree_tun <- tune_grid(
  tree_wf,
  resamples = folds,
  grid = tree_grid,
  metrics = metric_set(roc_auc)
)


## Melhores hiperparametros ####
tree_hips <- select_best(tree_tun, metric = "roc_auc"); tree_hips

tree_wf_final <- tree_wf |> finalize_workflow(tree_hips)


## Last fit ####
tree_final <- tree_wf_final |> last_fit(d_sp)


## Medidas de desempenho ####
tree_final |> collect_metrics() #auc: 0.945

summary(conf_mat(tree_final |> collect_predictions(), class_caso, .pred_class)) |> 
  dplyr::select(-.estimator) |> 
  rename(Medida = .metric, Valor = .estimate) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = "#ffffe0"),
    locations = cells_body(rows = c(1, 3, 4))
  ) |> 
  fmt_number(columns = 2, decimals = 4)


# Bagging -----------------------------------------------------------------

## Especificacao do modelo ####
bag_espec <- bag_tree() |> 
  set_engine("rpart") |> 
  set_mode("classification")


## Workflow ####
bag_wf <- workflow() |> 
  add_formula(class_caso ~ .) |> 
  add_model(bag_espec)


## Last fit ####
bag_final <- bag_wf |> last_fit(d_sp)


## Medidas de desempenho ####
bag_final |> collect_metrics() #auc: 0.936

summary(conf_mat(bag_final |> collect_predictions(), class_caso, .pred_class)) |> 
  dplyr::select(-.estimator) |> 
  rename(Medida = .metric, Valor = .estimate) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = "#ffffe0"),
    locations = cells_body(rows = c(1, 3, 4))
  ) |> 
  fmt_number(columns = 2, decimals = 4)


# Florestas aleatorias ----------------------------------------------------

## Especificacao do modelo ####
rf_espec <- rand_forest(mtry = tune(), trees = 1000) |> 
  set_mode("classification") |> 
  set_engine("ranger")


## Workflow ####
rf_wf <- workflow() |> 
  add_formula(class_caso ~ .) |> 
  add_model(rf_espec)


## Grid search ####
rf_grid <- grid_regular(mtry(range = c(5, 15)))


## Tune ####
doParallel::registerDoParallel(); set.seed(3533)

rf_tun <- tune_grid(rf_wf, resamples = folds, grid = rf_grid)


## Melhores hiperparametros ####
rf_hips <- select_best(rf_tun, metric = "roc_auc"); rf_hips

rf_wf_final <- rf_wf |> finalize_workflow(rf_hips)


## Last fit ####
rf_final <- rf_wf_final |> last_fit(d_sp)


## Medidas de desempenho ####
rf_final |> collect_metrics() #auc: 0.954

summary(conf_mat(rf_final |> collect_predictions(), class_caso, .pred_class)) |> 
  dplyr::select(-.estimator) |> 
  rename(Medida = .metric, Valor = .estimate) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = "#ffffe0"),
    locations = cells_body(rows = c(1, 3, 4))
  ) |> 
  fmt_number(columns = 2, decimals = 4)


# KNN ---------------------------------------------------------------------

## Especificacao do modelo ####
knn_espec <- nearest_neighbor(neighbors = tune()) |> 
  set_engine("kknn") |> 
  set_mode("classification")


## Workflow ####
knn_wf <- workflow() |> 
  add_formula(class_caso ~ .) |> 
  add_model(knn_espec)


## Tune ####
doParallel::registerDoParallel(); set.seed(3533)

knn_tun <- tune_grid(knn_wf, resamples = folds)


## Melhores hiperparametros ####
knn_hips <- select_best(knn_tun, metric = "roc_auc"); knn_hips

knn_wf_final <- knn_wf |> finalize_workflow(knn_hips)


## Last fit ####
knn_final <- knn_wf_final |> last_fit(d_sp)


## Medidas de desempenho ####
knn_final |> collect_metrics() #auc: 0.901

summary(conf_mat(knn_final |> collect_predictions(), class_caso, .pred_class)) |> 
  dplyr::select(-.estimator) |> 
  rename(Medida = .metric, Valor = .estimate) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = "#ffffe0"),
    locations = cells_body(rows = c(1, 3, 4))
  ) |> 
  fmt_number(columns = 2, decimals = 4)


# Regressao logistica -----------------------------------------------------

## Especificacao do modelo ####
log_espec <- logistic_reg() |> 
  set_engine("glm") |> 
  set_mode("classification")


## Workflow ####
log_wf <- workflow() |> 
  add_formula(class_caso ~ .) |> 
  add_model(log_espec)


### Last fit ####
log_final <- log_wf |> last_fit(d_sp)


## Medidas de desempenho ####
log_final |> collect_metrics() #auc: 0.941

summary(conf_mat(log_final |> collect_predictions(), class_caso, .pred_class)) |> 
  dplyr::select(-.estimator) |> 
  rename(Medida = .metric, Valor = .estimate) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = "#ffffe0"),
    locations = cells_body(rows = c(1, 3, 4))
  ) |> 
  fmt_number(columns = 2, decimals = 4)


# Regressao logistica lasso -----------------------------------------------

## Especificacao do modelo ####
lasso_espec <- logistic_reg(penalty = tune()) |> 
  set_engine("glmnet") |> 
  set_mode("classification")


## Workflow ####
lasso_wf <- workflow() |> 
  add_formula(class_caso ~ .) |> 
  add_model(lasso_espec)


## Tune ####
doParallel::registerDoParallel(); set.seed(3533)

lasso_tun <- tune_grid(lasso_wf, resamples = folds)


## Melhores hiperparametros ####
lasso_hips <- select_best(lasso_tun, metric = "roc_auc"); lasso_hips

lasso_wf_final <- lasso_wf |> finalize_workflow(lasso_hips)


## Last fit ####
lasso_final <- lasso_wf_final |> last_fit(d_sp)


## Medidas de desempenho ####
lasso_final |> collect_metrics() #auc: 0.941

summary(conf_mat(lasso_final |> collect_predictions(), class_caso, .pred_class)) |> 
  dplyr::select(-.estimator) |> 
  rename(Medida = .metric, Valor = .estimate) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = "#ffffe0"),
    locations = cells_body(rows = c(1, 3, 4))
  ) |> 
  fmt_number(columns = 2, decimals = 4)


# SVM RBF -----------------------------------------------------------------

## Especificacao do modelo ####
svm_rbf_espec <- svm_rbf(cost = 5, rbf_sigma = 0.25) |> 
  set_mode("classification") |> 
  set_engine("kernlab")


## Workflow ####
svm_rbf_wf <- workflow() |> 
  add_formula(class_caso ~ .) |> 
  add_model(svm_rbf_espec)


## Last fit ####
svm_rbf_final <- svm_rbf_wf |> last_fit(d_sp)


## Medidas de desempenho ####
svm_rbf_final |> collect_metrics() #auc: 0.932

summary(conf_mat(svm_rbf_final |> collect_predictions(), class_caso, .pred_class)) |> 
  dplyr::select(-.estimator) |> 
  rename(Medida = .metric, Valor = .estimate) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = "#ffffe0"),
    locations = cells_body(rows = c(1, 3, 4))
  ) |> 
  fmt_number(columns = 2, decimals = 4)


# SVM linear --------------------------------------------------------------

## Especificacao do modelo ####
svm_linear_espec <- svm_linear() |> 
  set_mode("classification") |> 
  set_engine("kernlab")


## Workflow ####
svm_linear_wf <- workflow() |> 
  add_formula(class_caso ~ .) |> 
  add_model(svm_linear_espec)


## Last fit ####
svm_linear_final <- svm_linear_wf |> last_fit(d_sp)


## Medidas de desempenho ####
svm_linear_final |> collect_metrics() #auc: 0.939

summary(conf_mat(svm_linear_final |> collect_predictions(), class_caso, .pred_class)) |> 
  dplyr::select(-.estimator) |> 
  rename(Medida = .metric, Valor = .estimate) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = "#ffffe0"),
    locations = cells_body(rows = c(1, 3, 4))
  ) |> 
  fmt_number(columns = 2, decimals = 4)


# SVM polinomial ----------------------------------------------------------

## Especificacao do modelo ####
svm_poli_espec <- svm_poly(cost = 0.5, degree = 1) |> 
  set_mode("classification") |> 
  set_engine("kernlab")


## Workflow ####
svm_poli_wf <- workflow() |> 
  add_formula(class_caso ~ .) |> 
  add_model(svm_poli_espec)


## Last fit ####
svm_poli_final <- svm_poli_wf |> last_fit(d_sp)


## Medidas de desempenho ####
svm_poli_final |> collect_metrics() #auc: 0.939

summary(conf_mat(svm_poli_final |> collect_predictions(), class_caso, .pred_class)) |> 
  dplyr::select(-.estimator) |> 
  rename(Medida = .metric, Valor = .estimate) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = "#ffffe0"),
    locations = cells_body(rows = c(1, 3, 4))
  ) |> 
  fmt_number(columns = 2, decimals = 4)


# XGBoost -----------------------------------------------------------------

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
  resamples = folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

#saveRDS(xgb_tun, "xgb_tun_outros.rds")
#xgb_tun <- readRDS("02.codigos/xgb_tun_outros.rds")


## Melhores hiperparametros ####
xgb_hips <- select_best(xgb_tun, metric = "roc_auc"); xgb_hips

#mtry: 18; min_n: 9; tree_depth: 9; learn_rate: 0.0150; 
#loss_reduction: 0.000912; sample_size: 0.604


## Last fit ####
xgb_wf_final <- xgb_wf |> finalize_workflow(xgb_hips)

xgb_final <- xgb_wf_final |> last_fit(d_sp)


## Medidas de desempenho ####
xgb_final |> collect_metrics() #auc: 0.958

summary(conf_mat(xgb_final |> collect_predictions(), class_caso, .pred_class)) |> 
  dplyr::select(-.estimator) |> 
  rename(Medida = .metric, `Valor (s/ SMOTE)` = .estimate) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = "#ffffe0"),
    locations = cells_body(rows = c(1, 3, 4))
  ) |> 
  fmt_number(columns = 2, decimals = 4)

#gtsave(tbl_med, filename = "03.resultados/modelo/t_med_outros.png")


# Limpa ambiente ----------------------------------------------------------

rm(list = ls())





# Libraries ---------------------------------------------------------------

# If these packages are not installed on your machine, install them using the command lines:
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("tidymodels")
# install.packages("kknn")
# install.packages("glmnet")
# install.packages("discrim")
# install.packages("sparsediscrim")
# install.packages("kernlab")
# install.packages("baguette")
# install.packages("gt")

#manipulation
library(dplyr)
library(lubridate)
#model
library(tidymodels)
library(kknn) #knn
library(glmnet) #lasso
library(discrim) #lda
library(sparsediscrim) #lda
library(kernlab) #svm
library(baguette) #bagging
#table
library(gt)


# Data import -------------------------------------------------------------

srag1621 <- readRDS("01.data/srag_16-21_[all].rds")


# Data manipulation -------------------------------------------------------

d_srag_def <- srag1621 |> 
  # undefinied cases = na
  mutate(class_caso = as.factor(if_else(class_caso == "Unspecified etiological agent", NA_character_, class_caso))) |>
  dplyr::select(
    sg_uf, class_gest_puerp, nu_idade_n, raca, escolaridade, vacina,
    dt_sin_pri, febre, tosse, garganta, dispneia, desc_resp, saturacao, diarreia,
    cardiopatia, pneumopatia, renal, obesidade,
    class_caso
  ) |>  
  drop_na()

# sars notifications in the admission day
d_not <- d_srag_def |> 
  mutate(dt_sin_pri = as.Date(dt_sin_pri, "%d/%m/%Y")) |> 
  group_by(dt_sin_pri) |> 
  summarise(not_casos = n())

d_srag_def <- d_srag_def |> 
  mutate_at("dt_sin_pri", dmy) |> 
  left_join(d_not, by = "dt_sin_pri") |> 
  dplyr::select(-c(dt_sin_pri, sg_uf))


# Train, test and cross validation ----------------------------------------

# Train and test ####
set.seed(2625)

d_sp <- initial_split(d_srag_def)

# train
d_tr <- training(d_sp)

# test
d_ts <- testing(d_sp)


## Cross validation ####
set.seed(2624); folds <- vfold_cv(d_tr, v = 10, repeats = 3)


# Linear discriminant analysis --------------------------------------------

## Model specification ####
adl_espec <- discrim_linear() |> 
  set_engine("MASS")


## Workflow ####
adl_wf <- workflow() |> 
  add_formula(class_caso ~ .) |> 
  add_model(adl_espec)


## Last fit ####
adl_final <- adl_wf |> last_fit(d_sp)


## Performance metrics ####
summary(conf_mat(adl_final |> collect_predictions(), class_caso, .pred_class)) |> 
  dplyr::select(-.estimator) |> 
  rename(Medida = .metric, Valor = .estimate) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = "#ffffe0"),
    locations = cells_body(rows = c(1, 3, 4, 5, 6))
  ) |> 
  fmt_number(columns = 2, decimals = 4)


# Quadratic discriminant analysis -----------------------------------------

## Model specification ####
adl_quad_espec <- discrim_quad() |> 
  set_engine("sparsediscrim")


## Workflow ####
adl_quad_wf <- workflow() |> 
  add_formula(class_caso ~ .) |> 
  add_model(adl_quad_espec)


## Last fit ####
adl_quad_final <- adl_quad_wf |> last_fit(d_sp)


## Performance metrics ####
summary(conf_mat(adl_quad_final |> collect_predictions(), class_caso, .pred_class)) |> 
  dplyr::select(-.estimator) |> 
  rename(Medida = .metric, Valor = .estimate) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = "#ffffe0"),
    locations = cells_body(rows = c(1, 3, 4, 5, 6))
  ) |> 
  fmt_number(columns = 2, decimals = 4)


# Trees classification ----------------------------------------------------

## Model specification ####
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
set.seed(3533); doParallel::registerDoParallel()

tree_tun <- tune_grid(
  tree_wf,
  resamples = folds,
  grid = tree_grid,
  metrics = metric_set(roc_auc)
)


## Best hyperparameters ####
tree_hips <- select_best(tree_tun, metric = "roc_auc"); tree_hips

tree_wf_final <- tree_wf |> finalize_workflow(tree_hips)


## Last fit ####
tree_final <- tree_wf_final |> last_fit(d_sp)


## Performance metrics ####
summary(conf_mat(tree_final |> collect_predictions(), class_caso, .pred_class)) |> 
  dplyr::select(-.estimator) |> 
  rename(Medida = .metric, Valor = .estimate) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = "#ffffe0"),
    locations = cells_body(rows = c(1, 3, 4, 5, 6))
  ) |> 
  fmt_number(columns = 2, decimals = 4)


# Bagging -----------------------------------------------------------------

## Model specification ####
bag_espec <- bag_tree() |> 
  set_engine("rpart") |> 
  set_mode("classification")


## Workflow ####
bag_wf <- workflow() |> 
  add_formula(class_caso ~ .) |> 
  add_model(bag_espec)


## Last fit ####
bag_final <- bag_wf |> last_fit(d_sp)


## Performance metrics ####
summary(conf_mat(bag_final |> collect_predictions(), class_caso, .pred_class)) |> 
  dplyr::select(-.estimator) |> 
  rename(Medida = .metric, Valor = .estimate) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = "#ffffe0"),
    locations = cells_body(rows = c(1, 3, 4, 5, 6))
  ) |> 
  fmt_number(columns = 2, decimals = 4)


# Random forests ----------------------------------------------------------

## Model specification ####
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
set.seed(3533); doParallel::registerDoParallel()

rf_tun <- tune_grid(rf_wf, resamples = folds, grid = rf_grid)


## Best hyperparameters ####
rf_hips <- select_best(rf_tun, metric = "roc_auc"); rf_hips

rf_wf_final <- rf_wf |> finalize_workflow(rf_hips)


## Last fit ####
rf_final <- rf_wf_final |> last_fit(d_sp)


## Performance metrics ####
summary(conf_mat(rf_final |> collect_predictions(), class_caso, .pred_class)) |> 
  dplyr::select(-.estimator) |> 
  rename(Medida = .metric, Valor = .estimate) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = "#ffffe0"),
    locations = cells_body(rows = c(1, 3, 4, 5, 6))
  ) |> 
  fmt_number(columns = 2, decimals = 4)


# KNN ---------------------------------------------------------------------

## Model specification ####
knn_espec <- nearest_neighbor(neighbors = tune()) |> 
  set_engine("kknn") |> 
  set_mode("classification")


## Workflow ####
knn_wf <- workflow() |> 
  add_formula(class_caso ~ .) |> 
  add_model(knn_espec)


## Tune ####
set.seed(3533); doParallel::registerDoParallel()

knn_tun <- tune_grid(knn_wf, resamples = folds)


## Best hyperparameters ####
knn_hips <- select_best(knn_tun, metric = "roc_auc"); knn_hips

knn_wf_final <- knn_wf |> finalize_workflow(knn_hips)


## Last fit ####
knn_final <- knn_wf_final |> last_fit(d_sp)


## Performance metrics ####
summary(conf_mat(knn_final |> collect_predictions(), class_caso, .pred_class)) |> 
  dplyr::select(-.estimator) |> 
  rename(Medida = .metric, Valor = .estimate) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = "#ffffe0"),
    locations = cells_body(rows = c(1, 3, 4, 5, 6))
  ) |> 
  fmt_number(columns = 2, decimals = 4)


# Logistic regression -----------------------------------------------------

## Model specification ####
log_espec <- logistic_reg() |> 
  set_engine("glm") |> 
  set_mode("classification")


## Workflow ####
log_wf <- workflow() |> 
  add_formula(class_caso ~ .) |> 
  add_model(log_espec)


### Last fit ####
log_final <- log_wf |> last_fit(d_sp)


## Performance metrics ####
summary(conf_mat(log_final |> collect_predictions(), class_caso, .pred_class)) |> 
  dplyr::select(-.estimator) |> 
  rename(Medida = .metric, Valor = .estimate) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = "#ffffe0"),
    locations = cells_body(rows = c(1, 3, 4, 5, 6))
  ) |> 
  fmt_number(columns = 2, decimals = 4)


# Logistic lasso regression -----------------------------------------------

## Model specification ####
lasso_espec <- logistic_reg(penalty = tune()) |> 
  set_engine("glmnet") |> 
  set_mode("classification")


## Workflow ####
lasso_wf <- workflow() |> 
  add_formula(class_caso ~ .) |> 
  add_model(lasso_espec)


## Tune ####
set.seed(3533); doParallel::registerDoParallel()

lasso_tun <- tune_grid(lasso_wf, resamples = folds)


## Best hyperparameters ####
lasso_hips <- select_best(lasso_tun, metric = "roc_auc"); lasso_hips

lasso_wf_final <- lasso_wf |> finalize_workflow(lasso_hips)


## Last fit ####
lasso_final <- lasso_wf_final |> last_fit(d_sp)


## Performance metrics ####
summary(conf_mat(lasso_final |> collect_predictions(), class_caso, .pred_class)) |> 
  dplyr::select(-.estimator) |> 
  rename(Medida = .metric, Valor = .estimate) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = "#ffffe0"),
    locations = cells_body(rows = c(1, 3, 4, 5, 6))
  ) |> 
  fmt_number(columns = 2, decimals = 4)


# RBF SVM -----------------------------------------------------------------

## Model specification ####
svm_rbf_espec <- svm_rbf(cost = 5, rbf_sigma = 0.25) |> 
  set_mode("classification") |> 
  set_engine("kernlab")


## Workflow ####
svm_rbf_wf <- workflow() |> 
  add_formula(class_caso ~ .) |> 
  add_model(svm_rbf_espec)


## Last fit ####
svm_rbf_final <- svm_rbf_wf |> last_fit(d_sp)


## Performance metrics ####
summary(conf_mat(svm_rbf_final |> collect_predictions(), class_caso, .pred_class)) |> 
  dplyr::select(-.estimator) |> 
  rename(Medida = .metric, Valor = .estimate) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = "#ffffe0"),
    locations = cells_body(rows = c(1, 3, 4, 5, 6))
  ) |> 
  fmt_number(columns = 2, decimals = 4)


# Linear SVM --------------------------------------------------------------

## Model specification ####
svm_linear_espec <- svm_linear() |> 
  set_mode("classification") |> 
  set_engine("kernlab")


## Workflow ####
svm_linear_wf <- workflow() |> 
  add_formula(class_caso ~ .) |> 
  add_model(svm_linear_espec)


## Last fit ####
svm_linear_final <- svm_linear_wf |> last_fit(d_sp)


## Performance metrics ####
summary(conf_mat(svm_linear_final |> collect_predictions(), class_caso, .pred_class)) |> 
  dplyr::select(-.estimator) |> 
  rename(Medida = .metric, Valor = .estimate) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = "#ffffe0"),
    locations = cells_body(rows = c(1, 3, 4, 5, 6))
  ) |> 
  fmt_number(columns = 2, decimals = 4)


# Polynomial SVM ----------------------------------------------------------

## Model specification ####
svm_poli_espec <- svm_poly(cost = 0.5, degree = 1) |> 
  set_mode("classification") |> 
  set_engine("kernlab")


## Workflow ####
svm_poli_wf <- workflow() |> 
  add_formula(class_caso ~ .) |> 
  add_model(svm_poli_espec)


## Last fit ####
svm_poli_final <- svm_poli_wf |> last_fit(d_sp)


## Performance metrics ####
summary(conf_mat(svm_poli_final |> collect_predictions(), class_caso, .pred_class)) |> 
  dplyr::select(-.estimator) |> 
  rename(Medida = .metric, Valor = .estimate) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = "#ffffe0"),
    locations = cells_body(rows = c(1, 3, 4, 5, 6))
  ) |> 
  fmt_number(columns = 2, decimals = 4)


# Clean environment -------------------------------------------------------

rm(list = ls())




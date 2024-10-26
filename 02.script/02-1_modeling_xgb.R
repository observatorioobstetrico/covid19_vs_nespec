
# Libraries ---------------------------------------------------------------

# If these packages are not installed on your machine, install them using the command lines:
# install.packages("lubridate)
# install.packages("tidymodels")
# install.packages("xgboost")
# install.packages("gt")
# install.packages("ggplot2")

#manipulation
library(lubridate)
#model
library(tidymodels)
library(xgboost)
#table
library(gt)
#graphs
library(ggplot2)


# Data import -------------------------------------------------------------

srag1621 <- readRDS("01.data/srag_16-21_[all].rds")


# Data manipulation -------------------------------------------------------

## Dataset of defined cases ####
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


## Dataset of undefined cases ####
d_srag_ind <- srag1621 |> 
  filter(ano > 2019 & class_caso == "Unspecified etiological agent") |> 
  mutate(dt_sin_pri = as.Date(dt_sin_pri, "%d/%m/%Y")) |> 
  dplyr::select(
    ano, sg_uf, class_gest_puerp, nu_idade_n, raca, escolaridade, vacina,
    dt_sin_pri, febre, tosse, garganta, dispneia, desc_resp, saturacao, diarreia,
    cardiopatia, pneumopatia, renal, obesidade,
    evolucao, class_caso
  ) |>  
  drop_na()

# sars notifications in the admission day
d_not <- d_srag_ind |> 
  mutate(dt_sin_pri = as.Date(dt_sin_pri, "%d/%m/%Y")) |> 
  group_by(dt_sin_pri) |> 
  summarise(not_casos = n())

d_srag_ind <- d_srag_ind |> 
  left_join(d_not, by = "dt_sin_pri") |> 
  relocate(ano, sg_uf, not_casos)


# XGBoost -----------------------------------------------------------------

## Train and test ####
set.seed(2625)

d_sp <- initial_split(d_srag_def)

# train
d_tr <- training(d_sp)

d_tr <- recipe(class_caso ~ ., data = d_tr) |> 
  themis::step_smotenc(class_caso, over_ratio = 0.5) |> 
  prep() |> 
  bake(new_data = NULL)

# test
d_ts <- testing(d_sp)


## Cross validation ####
set.seed(2624); xgb_folds <- vfold_cv(d_tr, v = 10, repeats = 3)


## Model specification ####
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
set.seed(2688)

# doParallel::registerDoParallel()
# 
# xgb_tun <- tune_grid(
#   xgb_wf,
#   resamples = xgb_folds,
#   grid = xgb_grid,
#   control = control_grid(save_pred = TRUE)
# )
# 
# saveRDS(xgb_tun, "xgb_tun.rds")

xgb_tun <- readRDS("02.script/xgb_tun.rds")


## Best hyperparameters ####
xgb_hips <- select_best(xgb_tun, metric = "roc_auc"); xgb_hips
#mtry: 16; min_n: 11; tree_depth: 9; learn_rate: 0.0776; loss_reduction: 0.00157; sample_size: 0.224

tbl01 <- xgb_hips |>
  select(-.config) |> 
  mutate(trees = 1000) |> 
  relocate(trees) |>
  gt::gt() |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())
  ) |> 
  cols_align(align = "center", columns = c(2:6))


## Final model ####
xgb_wf_final <- xgb_wf |> finalize_workflow(xgb_hips)

xgb_final <- xgb_wf_final |> last_fit(d_sp)


## Performance metrics ####
summary(conf_mat(xgb_final |> collect_predictions(), class_caso, .pred_class)) |> 
  dplyr::select(-.estimator) |> 
  rename(Metric = .metric, `Value` = .estimate) |> 
  gt() |> 
    tab_style(
      style = cell_fill(color = "#ffffe0"),
      locations = cells_body(rows = c(1, 3, 4, 5, 6))
    ) |> 
    fmt_number(columns = 2, decimals = 4)


## Prediction ####
# fit
xgb_aj <- xgb_wf_final |> fit(d_srag_def)

# prediction
xgb_pd <- predict(xgb_aj, d_srag_ind)


# Base with prediction ----------------------------------------------------

d_srag_com_pred <- d_srag_ind |> 
  mutate(class_caso_pred = xgb_pd$.pred_class)


# Table and data export ---------------------------------------------------

# table
gt::gtsave(tbl01, filename = "03.results/supplementary/table01.png")

# data
saveRDS(d_srag_com_pred, "01.dados/srag_16-21_[pred].rds")


# Clean environment -------------------------------------------------------

rm(list = ls())
  






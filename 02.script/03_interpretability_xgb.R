
# Libraries ---------------------------------------------------------------

#if these packages are not installed on your machine, install them using the command lines:
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("tidymodels")
# install.packages("xgboost")
# install.packages("shapviz")
# install.packages("ggplot2")

#manipulation
library(dplyr)
library(lubridate)
#model
library(tidymodels)
library(xgboost)
#interpretability
library(shapviz)
#graphs
library(ggplot2)


# Data import -------------------------------------------------------------

srag1621 <- readRDS("01.data/srag_16-21_[all].rds")


# Data manipulation -------------------------------------------------------

d_srag <- srag1621 |> 
  # undefinied cases = na
  mutate(class_caso = as.factor(if_else(class_caso == "Unspecified etiological agent", NA_character_, class_caso))) |> 
  dplyr::select(
    class_gest_puerp, nu_idade_n, raca, escolaridade, vacina,
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


# XGBoost model -----------------------------------------------------------

## Train and test ####
# split; train; test
set.seed(2625); d_sp <- initial_split(d_srag); d_tr <- training(d_sp); d_ts <- testing(d_sp)


## Recipe ####
xgb_rec <- recipe(class_caso ~ ., data = d_tr) |> 
  themis::step_smotenc(class_caso, over_ratio = 0.5) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

xgb_prep <- prep(xgb_rec) |> 
  juice() |> 
  dplyr::select(-class_caso)


## Model specification ####
xgb_espec <- boost_tree(
  trees = 1000,
  tree_depth = 9, min_n = 11,
  loss_reduction = 0.00157,     
  sample_size = 0.224, mtry = 16,   
  learn_rate = 0.0776,          
  ) |> 
  set_engine("xgboost") |> 
  set_mode("classification")


## Fit ####
xgb_aj <- workflow() |> 
  add_recipe(xgb_rec) |> 
  add_model(xgb_espec) |> 
  fit(d_tr)


# Interpretability - SHAP -------------------------------------------------

shap <- shapviz(extract_fit_engine(xgb_aj), X_pred = data.matrix(xgb_prep))

# summary plot
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
  geom_label(aes(x, y, label = round(as.numeric(label), 2)), data = layer_data(g1, 3), fontface = "bold") +
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
      "SARS notifications in the\nadmission day"
    )
  )

# remove geom_text( )
g2$layers[[3]] <- NULL; g2


# Plot export -------------------------------------------------------------

ggsave("03.results/figs/figure02.png", width = 16, height = 10, dpi = 700)


# Clean environment -------------------------------------------------------

rm(list = ls())

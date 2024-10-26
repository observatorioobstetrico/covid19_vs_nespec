
# Libraries ---------------------------------------------------------------

# If these packages are not installed on your machine, install them using the command lines:
# install.packages("dplyr")
# install.packages("purrr")
# install.packages("tidyr")
# install.packages("forcats")
# install.packages("lubridate")
# install.packages("janitor")
# install.packages("geobr")
# install.packages("sf")
# install.packages("ggspatial")
# install.packages("ggplot2")
# install.packages("patchwork")
# install.packages("gtsummary")
# install.packages("gt")

#manipulation
library(dplyr)
library(purrr)
library(tidyr)
library(forcats)
library(lubridate)
library(janitor)
#maps
library(geobr)
library(sf)
library(ggspatial)
#graphs
library(ggplot2)
library(patchwork)
#table
library(gtsummary)
library(gt)


# Data import -------------------------------------------------------------

# confirmed sars
srag1621 <- readRDS("01.data/srag_16-21_[all].rds")

# predicted sars
srag1621_pred <- readRDS("01.data/srag_16-21_[pred].rds")

# sinasc - live biirth from 2020 to 2021
sinasc2021 <- fs::dir_ls("01.data/sinasc/", glob = "*.csv")

sinasc2021 <- map(sinasc2021, readr::read_csv2) |> 
  list_rbind() |> 
  clean_names() %>%
  # creates separate columns for code and state name
  separate(., col = uf, into = c("code_uf", "nome_uf"), sep = " ") |> 
  mutate_at("ano", as.character) |> 
  mutate_at("code_uf", as.numeric) |> 
  dplyr::select(-nome_uf) |> 
  pivot_wider(names_from = ano, values_from = total) |> 
  rename(nv_2020 = `2020`, nv_2021 = `2021`) |> 
  rowwise() |> 
  # calculates total of live births by state
  mutate(total_nv = sum(nv_2020, nv_2021))
  
# geobr - 2020
geobr <- read_state(code_state = "all", year = 2020, showProgress = FALSE)


# Data manipulation -------------------------------------------------------

## Real dataset of COVID-19 ####
d_srag_real <- srag1621 |> 
  filter(class_caso == "COVID-19") |> 
  mutate(class_caso = fct_recode(as.factor(class_caso), "COVID-19r"="COVID-19")) |>
  rename(class_cov = class_caso) |> 
  dplyr::select(
    ano, sg_uf, class_gest_puerp, nu_idade_n, raca, escolaridade, vacina,
    dt_sin_pri, febre, tosse, garganta, dispneia, desc_resp, saturacao, diarreia,
    cardiopatia, pneumopatia, renal, obesidade,
    evolucao, class_cov
  ) |>  
  drop_na()


## Predicted dataset of COVID-19 ####
d_srag_pred <- srag1621_pred |> 
  filter(class_caso_pred == "COVID-19") |> 
  mutate(class_caso_pred = fct_recode(class_caso_pred, "COVID-19p"="COVID-19")) |> 
  rename(class_cov = class_caso_pred) |> 
  dplyr::select(-c(not_casos, class_caso))


## Real and predicted dataset of COVID-19 ####
d_srag_covid19 <- d_srag_real |> 
  mutate_at("dt_sin_pri", dmy) |> 
  full_join(d_srag_pred) |> 
  left_join(geobr, by = c("sg_uf"="abbrev_state"))


# Maps ----------------------------------------------------------------------

### Pre-XGB ####
d_srag_casos_pre <- d_srag_covid19 |> 
  filter(class_cov == "COVID-19r") |> 
  group_by(code_state) |> 
  summarise(n_casos = n()) |> 
  left_join(sinasc2021 |> dplyr::select(code_uf, total_nv), by = c("code_state"="code_uf")) |> 
  mutate(taxa_casos_pre = (n_casos / total_nv) * 100000) |> 
  left_join(geobr, by = "code_state") |> 
  st_as_sf()

g01 <- ggplot(d_srag_casos_pre, aes(fill = taxa_casos_pre)) + 
  geom_sf(color = "#ffffff") +
  scale_fill_gradient(low = "#0096ff", high = "#e0115f", limits = c(0, 1500)) +
  geom_sf_text(aes(label = abbrev_state), size = 3, color = "#ffffff") +
  labs(title = "A", fill = "Rate of COVID-19 cases\nper 100,000 LB") +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    legend.key.height = unit(1.5, 'cm'), legend.key.width = unit(1, 'cm'),
    panel.background = element_blank(), plot.background = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.title = element_blank(), axis.text = element_blank(),
    axis.ticks = element_blank(), plot.title = element_text(face = "bold")
  ) +
  annotation_scale(location = "br") +
  annotation_north_arrow(
    location = "br", which_north = "true", 
    pad_x = unit(0.45, "in"), pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering
  )

### Post-XGB ####
d_srag_casos_pos <- d_srag_covid19 |> 
  group_by(code_state) |> 
  summarise(n_casos = n()) |> 
  left_join(sinasc2021 |> dplyr::select(code_uf, total_nv), by = c("code_state"="code_uf")) |> 
  mutate(taxa_casos_pos = (n_casos / total_nv) * 100000) |> 
  left_join(geobr, by = "code_state") |> 
  st_as_sf()

g02 <- ggplot(d_srag_casos_pos, aes(fill = taxa_casos_pos)) + 
  geom_sf(color = "#ffffff") +
  scale_fill_gradient(low = "#0096ff", high = "#e0115f", limits = c(0, 1500)) +
  geom_sf_text(aes(label = abbrev_state), size = 3, color = "#ffffff") +
  labs(title = "B", fill = "Rate of COVID-19 cases\nper 100,000 LB") +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    legend.key.height = unit(1.5, 'cm'), legend.key.width = unit(1, 'cm'),
    panel.background = element_blank(), plot.background = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.title = element_blank(), axis.text = element_blank(),
    axis.ticks = element_blank(), plot.title = element_text(face = "bold")
  ) +
  annotation_scale(location = "br") +
  annotation_north_arrow(
    location = "br", which_north = "true", 
    pad_x = unit(0.45, "in"), pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering
  )


# Tables ------------------------------------------------------------------

## Cases ####
tbl03 <- d_srag_casos_pre |> 
  as_tibble() |> 
  rename(n_casos_pre = n_casos) |> 
  left_join(d_srag_casos_pos |> rename(n_casos_pos = n_casos)) |> 
  dplyr::select(code_state, abbrev_state, name_state, geom, n_casos_pre, n_casos_pos, taxa_casos_pre, taxa_casos_pos) |>
  rowwise() |> 
  mutate(p_aum_casos = sprintf("%.1f", 100 * ((taxa_casos_pos - taxa_casos_pre) / taxa_casos_pre))) |> 
  ungroup() |> 
  mutate(taxa_casos_pre = sprintf("%.1f", taxa_casos_pre), taxa_casos_pos = sprintf("%.1f", taxa_casos_pos)) |> 
  dplyr::select(name_state, n_casos_pre, n_casos_pos, taxa_casos_pre, taxa_casos_pos, p_aum_casos) |> 
  # table
  gt::gt() |> 
  cols_label(
    name_state = "Brazilian state",
    n_casos_pre = "Number of COVID-19 cases (confirmed)",
    n_casos_pos = "Number of COVID-19 cases (confirmed + predicted)",
    taxa_casos_pre = "COVID-19 case rate (confirmed)",
    taxa_casos_pos = "COVID-19 case rate (confirmed + predicted)",
    p_aum_casos = "Case rate increase (percentage)"
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())
  ) |> 
  cols_align(align = "center", columns = c(2:6)) |>
  tab_footnote(
    footnote = "Rate by 100,000 live births.",
    locations = cells_column_labels(columns = p_aum_casos)
  )
  

## Death ####
tbl04 <- d_srag_obitos_pre |> 
  as_tibble() |> 
  rename(n_obitos_pre = n_obitos) |> 
  left_join(d_srag_obitos_pos |> rename(n_obitos_pos = n_obitos)) |> 
  dplyr::select(code_state, abbrev_state, name_state, geom, n_obitos_pre, n_obitos_pos, taxa_obitos_pre, taxa_obitos_pos) |>
  rowwise() |> 
  mutate(p_aum_obitos = sprintf("%.1f", 100 * ((taxa_obitos_pos - taxa_obitos_pre) / taxa_obitos_pre))) |> 
  ungroup() |> 
  mutate(taxa_obitos_pre = sprintf("%.1f", taxa_obitos_pre), taxa_obitos_pos = sprintf("%.1f", taxa_obitos_pos)) |>
  dplyr::select(name_state, n_obitos_pre, n_obitos_pos, taxa_obitos_pre, taxa_obitos_pos, p_aum_obitos) |> 
  # table
  gt::gt() |> 
  cols_label(
    name_state = "Brazilian state",
    n_obitos_pre = "Number of COVID-19 deaths (confirmed)",
    n_obitos_pos = "Number of COVID-19 deaths (confirmed + predicted)",
    taxa_obitos_pre = "MMR by COVID-19 (confirmed)",
    taxa_obitos_pos = "MMR by COVID-19 (confirmed + predicted)",
    p_aum_obitos = "MMR increase (percentage)"
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())
  ) |> 
  cols_align(align = "center", columns = c(2:6)) |>
  tab_footnote(
    footnote = "Maternal Mortality Ratio;",
    locations = cells_column_labels(columns = p_aum_obitos)
  ) |> 
  tab_footnote(
    footnote = "Rate by 100,000 live births.",
    locations = cells_column_labels(columns = p_aum_obitos)
  ) |> 
  tab_options(footnotes.multiline = FALSE)


# Maps and tables export --------------------------------------------------

# maps
g01 + g02 + plot_layout(guides = "collect") 
ggsave("03.results/figs/figure04.png", width = 16, height = 10, dpi = 700)

# tables
gt::gtsave(tbl03, filename = "03.results/supplementary/table03.png")
gt::gtsave(tbl04, filename = "03.results/supplementary/table04.png")


# Clean environment -------------------------------------------------------

rm(list = ls())



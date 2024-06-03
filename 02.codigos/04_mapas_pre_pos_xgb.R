
# Bibliotecas -------------------------------------------------------------

#manipulacao
library(dplyr)
library(purrr)
library(tidyr)
library(forcats)
library(lubridate)
library(janitor)
#mapas
library(geobr)
library(sf)
library(ggspatial)
#grafico
library(ggplot2)
library(patchwork)


# Importacao dos dados ----------------------------------------------------

# srag real
srag1621 <- readRDS("01.dados/srag_16-21_[all].rds")

# srag predita
srag1621_pred <- readRDS("01.dados/srag_16-21_[pred].rds")

# sinasc - nv de 2020 e 2021
sinasc2021 <- fs::dir_ls("01.dados/sinasc/", glob = "*.csv")

sinasc2021 <- map(sinasc2021, readr::read_csv2) |> 
  list_rbind() |> 
  clean_names() %>%
  # cria colunas separadas para codigo e nome da uf
  separate(., col = uf, into = c("code_uf", "nome_uf"), sep = " ") |> 
  mutate_at("ano", as.character) |> 
  mutate_at("code_uf", as.numeric) |> 
  select(-nome_uf) |> 
  pivot_wider(names_from = ano, values_from = total) |> 
  rename(nv_2020 = `2020`, nv_2021 = `2021`) |> 
  rowwise() |> 
  # calcula total de nv por uf
  mutate(total_nv = sum(nv_2020, nv_2021))
  
# geobr
geobr <- read_state(code_state = "all", year = 2020, showProgress = FALSE)


# Manipulacao dos dados ---------------------------------------------------

## Base real de COVID-19 ####
d_srag_real <- srag1621 |> 
  filter(class_caso == "COVID-19") |> 
  mutate(
    # covid-19 = covid-19 real
    class_caso = fct_recode(as.factor(class_caso), "COVID-19r" = "COVID-19")
  ) |>
  rename(class_cov = class_caso) |> 
  select(
    ano, sg_uf, class_gest_puerp, nu_idade_n, raca, escolaridade, vacina,
    dt_sin_pri, febre, tosse, garganta, dispneia, desc_resp, saturacao, diarreia,
    cardiopatia, pneumopatia, renal, obesidade,
    evolucao, class_cov
  ) |>  
  drop_na()


## Base predita de COVID-19 ####
d_srag_pred <- srag1621_pred |> 
  filter(class_caso_pred == "COVID-19") |> 
  mutate(
    # covid-19 = covid-19 predita
    class_caso_pred = fct_recode(class_caso_pred, "COVID-19p" = "COVID-19")
  ) |> 
  rename(class_cov = class_caso_pred) |> 
  select(-c(not_casos, class_caso))


## Base real e predita de COVID-19 ####
d_srag_covid19 <- d_srag_real |> 
  mutate_at("dt_sin_pri", dmy) |> 
  full_join(d_srag_pred) |> 
  left_join(geobr, by = c("sg_uf"="abbrev_state"))


# Mapas -------------------------------------------------------------------

## Casos ####

### Pre-XGB ####
d_srag_casos_pre <- d_srag_covid19 |> 
  filter(class_cov == "COVID-19r") |> 
  group_by(code_state) |> 
  summarise(n_casos = n()) |> 
  left_join(sinasc2021 |> select(code_uf, total_nv), by = c("code_state"="code_uf")) |> 
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

### Pos-XGB ####
d_srag_casos_pos <- d_srag_covid19 |> 
  group_by(code_state) |> 
  summarise(n_casos = n()) |> 
  left_join(sinasc2021 |> select(code_uf, total_nv), by = c("code_state"="code_uf")) |> 
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


## Obitos ####

### Pre-XGB ####
d_srag_obitos_pre <- d_srag_covid19 |> 
  filter(class_cov == "COVID-19r" & evolucao == "Óbito") |> 
  group_by(code_state) |> 
  summarise(n_obitos = n()) |> 
  left_join(sinasc2021 |> select(code_uf, total_nv), by = c("code_state"="code_uf")) |> 
  mutate(taxa_obitos_pre = (n_obitos / total_nv) * 100000) |> 
  left_join(geobr, by = "code_state") |> 
  st_as_sf()

g03 <- ggplot(d_srag_obitos_pre, aes(fill = taxa_obitos_pre)) + 
  geom_sf(color = "#ffffff") +
  scale_fill_gradient(low = "#0096ff", high = "#e0115f", limits = c(0, 110)) +
  geom_sf_text(aes(label = abbrev_state), size = 3, color = "#ffffff") +
  labs(title = "A", fill = "MMR per\n100,000 LB") +
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

### Pos-XGB ####
d_srag_obitos_pos <- d_srag_covid19 |> 
  filter(evolucao == "Óbito") |> 
  group_by(code_state) |> 
  summarise(n_obitos = n()) |> 
  left_join(sinasc2021 |> select(code_uf, total_nv), by = c("code_state"="code_uf")) |> 
  mutate(taxa_obitos_pos = (n_obitos / total_nv) * 100000) |> 
  left_join(geobr, by = "code_state") |> 
  st_as_sf()

g04 <- ggplot(d_srag_obitos_pos, aes(fill = taxa_obitos_pos)) + 
  geom_sf(color = "#ffffff") +
  scale_fill_gradient(low = "#0096ff", high = "#e0115f", limits = c(0, 110)) +
  geom_sf_text(aes(label = abbrev_state), size = 3, color = "#ffffff") +
  labs(title = "B", fill = "MMR per\n100,000 LB") +
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


# Exportacao dos mapas ----------------------------------------------------

# casos
g01 + g02 + plot_layout(guides = "collect") 
ggsave("03.resultados/mapas/tiff/g_cas.tiff", width = 16, height = 10, dpi = 700)

# obitos
g03 + g04 + plot_layout(guides = "collect")
ggsave("03.resultados/mapas/tiff/g_obi.tiff", width = 16, height = 10, dpi = 700)


# Tabelas -----------------------------------------------------------------

## Casos ####
d_srag_casos <- d_srag_casos_pre |> 
  as_tibble() |> 
  rename(n_casos_pre = n_casos) |> 
  left_join(d_srag_casos_pos |> rename(n_casos_pos = n_casos)) |> 
  select(
    code_state, abbrev_state, name_state, geom, 
    n_casos_pre, n_casos_pos, taxa_casos_pre, taxa_casos_pos
  ) |>
  rowwise() |> 
  mutate(p_aum_casos = sprintf("%.1f", 100 * ((taxa_casos_pos - taxa_casos_pre) / taxa_casos_pre)))
  

## Obitos ####
d_srag_obitos <- d_srag_obitos_pre |> 
  as_tibble() |> 
  rename(n_obitos_pre = n_obitos) |> 
  left_join(d_srag_obitos_pos |> rename(n_obitos_pos = n_obitos)) |> 
  select(
    code_state, abbrev_state, name_state, geom, 
    n_obitos_pre, n_obitos_pos, taxa_obitos_pre, taxa_obitos_pos
  ) |>
  rowwise() |> 
  mutate(p_aum_obitos = sprintf("%.1f", 100 * ((taxa_obitos_pos - taxa_obitos_pre) / taxa_obitos_pre)))


## Casos e obitos ####
d_srag_casos_obitos <- full_join(d_srag_casos, d_srag_obitos) |> 
  mutate(
    taxa_casos_pre = sprintf("%.1f", taxa_casos_pre), 
    taxa_casos_pos = sprintf("%.1f", taxa_casos_pos),
    taxa_obitos_pre = sprintf("%.1f", taxa_obitos_pre), 
    taxa_obitos_pos = sprintf("%.1f", taxa_obitos_pos)
  )

writexl::write_xlsx(d_srag_casos_obitos, "03.resultados/mapas/tab_porc_aum.xlsx")


# Limpa ambiente ----------------------------------------------------------

rm(list = ls())



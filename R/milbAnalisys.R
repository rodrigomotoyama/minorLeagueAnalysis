library(tidyverse)
library(dplyr)
library(readr)
library(janitor)
library(ggplot2)

path <- map(1:3, ~str_c("data/fangraphs-minor-league-leaders (", .x, ").csv"))
milb_data <- map(path, read_csv) %>%
  bind_cols() %>%
  clean_names() %>%
  select(-ends_with(c("1", "2")))
milb_data %>%
  filter(name %in% c("Bo Takahashi", "Daniel Missaki",
                     "Igor Januario", "Heitor Tokar",
                     "Eric Pardinho", "Andre Rienzo")) %>%
  select(season, name, team, age, ip, k_percent, bb_percent, era, fip, babip, sw_str_percent, w, l) %>%
  group_by(nome = name, season) %>%
  summarise(total_vitoria = sum(w), total_derrota = sum(l), "Innings arremessados" = sum(ip)) %>%
  ggplot() +
  geom_point(aes(x = total_derrota, y = total_vitoria,
                 size = `Innings arremessados`,
                 colour = nome))+
  xlab("Derrotas")+ylab("Vitorias")+
  geom_text(aes(x = total_derrota, y = total_vitoria, label=season))+
  ylim(0, 11)+
  xlim(0, 11)+
  theme(
    legend.position = "bottom",
    legend.box = "vertical")+
  ggtitle("Total de vitorias/derrotas por temporada")



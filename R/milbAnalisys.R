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
  filter(name %in% c("Bo Takahashi", "Daniel Missaki")) %>%
  select(season, name, team, age, ip, k_percent, bb_percent, era, fip, babip, sw_str_percent) %>%
  arrange(desc(k_percent)) %>%
  ggplot() +
  geom_point(aes(x = season, y = sw_str_percent, color = name))

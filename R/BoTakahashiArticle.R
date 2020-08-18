library(tidyverse)

library(janitor)


path <- map(1:3, ~str_c("data/fangraphs-minor-league-leaders (", .x, ").csv"))
milb_data <- map(path, read_csv) %>%
  reduce(full_join) %>%
  clean_names() %>%
  select(-ends_with(c("1", "2")))
milb_data %>%
  # filter(name %in% c("Bo Takahashi",
  #                    "Jacob deGrom",
  #                    "Sonny Gray")) %>%
  # select(season, name, team) %>%
  mutate(
    level = team %>%
      str_split(" \\(|\\)", simplify = T) %>%
      .[,2]
    # level = team %>%
    #   str_remove("* $")
  ) %>%
  # arrange(level) %>%
  select(name, season, team, level, ip, k_percent, bb_percent,
         avg, era, whip, fip, ld_percent, gb_percent,
         fb_percent, sw_str_percent) %>%
  group_by(level) %>%
  mutate(
    fip_botakahashi = if_else(name == "Bo Takahashi", avg(fip), 1)
  )
  # ggplot(aes(x = level, y = fip, fill = name))+
  # geom_bar(stat = "identity", position = "dodge")

  milb_data %>% glimpse()
 milb_data %>%
   filter(ip>10) %>%
  # filter(name %in% c("Bo Takahashi", "Daniel Missaki"
  #                    # "Jordan Yamamoto"
  #                    ))%>%
  mutate(
    level = team %>%
      str_split(" \\(|\\)", simplify = T) %>%
      .[,2]
    # level = team %>%
    #   str_remove("* $")
  ) %>%
  select(season, name1 = name, level, age, ip, k_percent, bb_percent,
         era, fip, x_fip, ld_percent, gb_percent, fb_percent) %>%
  group_by(level) %>%
  mutate(bb_percent_lvl = mean(bb_percent, na.rm = T),
         era_lvl = mean(era, na.rm = T),
         fb_percent_lvl = mean(fb_percent, na.rm = T),
         gb_percent_lvl = mean(gb_percent, na.rm = T),
         ld_percent_lvl = mean(ld_percent, na.rm = T),
         k_percent_lvl = mean(k_percent, na.rm = T),
         fip_lvl = mean(fip, na.rm = T),
         x_fip_lvl = mean(x_fip, na.rm = T),
  ) %>%
  ungroup() %>%
   filter(name1 == "Bo Takahashi") %>%
   group_by(level) %>%
   mutate_at(c('ip', 'k_percent', 'bb_percent', 'era',
             'fip', 'x_fip', 'ld_percent', 'gb_percent', 'fb_percent'),
             mean) %>%
  pivot_longer(cols = ip:fb_percent) %>%
  pivot_longer(cols = ends_with("lvl"),
               names_to = "name_season",
               values_to = "value_season") %>%
   filter(name1 == "Bo Takahashi",
          name == name_season %>%
            str_sub(end = -5)
          )%>%
  ggplot()+
  geom_col(aes(x = name, y = value, fill = level, colour = name1), color = 'black', position = "dodge") +
   geom_col(aes(x = name_season, y = value_season, fill = level), position = "dodge")+
   facet_wrap(~name, scales = "free")+
   scale_x_discrete(labels = c("Bo Takahashi", "Média liga"))

milb_data %>% mutate(
  level = team %>%
    str_split(" \\(|\\)", simplify = T) %>%
    .[,2]
  # level = team %>%
  #   str_remove("* $")
) %>%
  group_by(level) %>%
  mutate(bb_percent_lvl = mean(bb_percent),
         era_lvl = mean(era),
         fb_percent_lvl = mean(fb_percent),
         gb_percent_lvl = mean(gb_percent),
         ld_percent_lvl = mean(ld_percent),
         k_percent_lvl = mean(k_percent),
         fip_lvl = mean(fip),
         x_fip_lvl = mean(x_fip),
         )






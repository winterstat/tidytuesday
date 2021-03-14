# --------------------------------#
#### Tidy Tuesday: 2021 Week 11 ###
# --------------------------------#


# Set-up --------------------------------------------------------------------

## Load packages
library(tidyverse)
library(skimr)
library(hrbrthemes) # maybe a cool theme?
hrbrthemes::import_roboto_condensed()
library(scales)
library(ggsci)
library(ggtext)


# Data --------------------------------------------------------------------

## Get data
tuesdata <- tidytuesdayR::tt_load(2021, week = 11)
bechdel <- tuesdata$movies

skim(bechdel)

table(bechdel$clean_test) 
# Bechdel test options are:
# dubious = pass; 
# men = women only talk about men (so there are 2 women who talk);
# notalk = women don't talk (so there are 2 women);
# nowomen = total fail
# ok = pass

# so order = no women - no talk - men - dubious - ok
# From FiveThirtyEight: “dubious” means that some BechdelTest.com contributors were skeptical about whether the films in 
# question passed the test

# Bechdel test outcomes per director -----------------------------------------

bechdel %>% group_by(director) %>%
  summarize(n_movies = n()) %>% View(.)

# Female directors with > 2 movies: 
# Catherine Hardwicke (3)
# Julie Taymor (3)
# Nicole Holofcener (3)

bechdel %>% group_by(director) %>%
  mutate(n_movies = n()) %>%
  filter(n_movies > 7 & !is.na(director)) %>%
  mutate(dir_sex = "Male \n (> 7 movies)") -> male_director

bechdel %>% group_by(director) %>%
  mutate(n_movies = n()) %>%
  filter(director %in% c("Catherine Hardwicke", "Julie Taymor", "Nicole Holofcener")) %>%
  mutate(dir_sex = "Female \n (3 movies)") -> female_director

director <- bind_rows(female_director, male_director)

# Figure -----------------------------------------------------------------------

#Colors inspired by: show_col(pal_startrek("uniform")(7))
col_pal <- c("#910900","#CC0C00", "#ff6056", "#6ce2ff" ,"#00B5E2")

# Order the directors by how often they pass the Bechdel test
director %>% 
  group_by(dir_sex, director, clean_test) %>%
  summarize(n_bechdel = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = clean_test, values_from = n_bechdel, values_fill = 0) %>%
  mutate(row_sum = rowSums(select(., -c(1:2)))) %>%
  mutate_at(-c(1:2), ~ . / row_sum) %>%
  select(-row_sum) %>%
  pivot_longer(cols = ok:nowomen, names_to = "clean_test", values_to = "prop_score") %>%
  filter(clean_test == "ok") %>%
  mutate(dir = factor(director, levels = director[order(prop_score)])) %>%
  pull(dir) %>%
  levels -> dir_levels
 
# Create plot
director %>% 
  group_by(dir_sex, director, clean_test) %>%
  summarize(n_bechdel = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = clean_test, values_from = n_bechdel, values_fill = 0) %>%
  mutate(row_sum = rowSums(select(., -c(1:2)))) %>%
  mutate_at(-c(1:2), ~ . / row_sum) %>%
  select(-row_sum) %>%
  pivot_longer(cols = ok:nowomen, names_to = "clean_test", values_to = "prop_score") %>%
  mutate(clean_test = factor(clean_test, levels = c('nowomen', 'notalk', 'men', 'dubious', 'ok'),
                                                    labels = c("< two Women", "Women don't talk to each other", 
                                                               "Women only talk about men",  
                                                               "Dubious", "Pass")),
         director = factor(director, levels = dir_levels)) %>%
  ggplot(aes(x = director, y = prop_score, fill = clean_test)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = col_pal,
                    name = element_blank(),
                    guide = guide_legend(reverse = TRUE)) +
  scale_x_discrete(limits = rev) +
  coord_flip() +
  facet_grid(rows = vars(dir_sex), scales = "free", space = "free", as.table = FALSE) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    rect = element_rect(fill = "#F5F5F5"),
    panel.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
    plot.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
    legend.position = "bottom",
    legend.justification = "left",
    legend.text = element_text(size = 8),
    plot.caption = element_markdown(family = "Roboto Condensed", colour = "#7C878EFF"),
    plot.title = element_text(size = 16)
  ) +
  labs(
    x = "",
    y = "Proportion",
    caption = "@winterstat | #TidyTuesday | Data: [FiveThirtyEight]",
    title = "Which Directors Make Movies that Pass the Bechdel Test?",
    subtitle = "Comparing male directors with > 7 movies to the three female directors with the\nhighest number of movies (released prior to 2014) in the Bechdel test database."
  )

# Save plot
ggsave("TT_week11.png", type = 'cairo', width = 7, height = 8, dpi = 300, units = "in")


  
  

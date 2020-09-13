#### Tidy Tuesday: 2020 Week 37

## Load packages
#install.packages("friends")
library("friends")
library("tidyverse")
library("showtext")
library("ggtext")
library("ggnewscale")


## Set up fonts

# Specify dpi (otherwise text will be tiny!)
op = showtext_opts(dpi = 300)

# Get font for title
font_add_google("Permanent Marker", "permanent marker")
showtext_auto()

## Get data
friends <- friends
friends_info <- friends_info

## Search string for variations of "how you doin'?"
string_how <- "how you doin'|how ya doin'|how you doing|how ya doing"

## Find utterances that contain string
howya_friends <- friends[str_detect(friends$text, stringr::regex(string_how, ignore_case = TRUE)),]

## Get number of times string is said per episode
## If one utterance contains the string twice, it is counted as 2
doin_friends <-
  friends %>%
  mutate(doin = str_count(text, 
                          pattern = stringr::regex(string_how, 
                                                   ignore_case = TRUE))) %>%
  group_by(season, episode) %>%
  summarise(n_doin = sum(doin)) %>%
  ungroup() %>%
  mutate(episode_id = row_number())

## Get number of times string is said per season
doin_by_season <- 
  doin_friends %>%
  group_by(season) %>%
  mutate(n_doin_season = sum(n_doin)) %>%
  ungroup() %>%
  distinct(season, n_doin_season)

## Repeat but only for utterances said by Joey
doin_joey <-
  friends %>% filter(speaker == "Joey Tribbiani") %>%
  mutate(doin_j = str_count(text, 
                          pattern = stringr::regex(string_how, 
                                                   ignore_case = TRUE))) %>%
  group_by(season, episode) %>%
  summarise(n_doin_j = sum(doin_j)) %>%
  ungroup() %>%
  mutate(episode_id = row_number())

doin_joey_by_season <- 
  doin_joey %>%
  group_by(season) %>%
  mutate(n_doin_joey_season = sum(n_doin_j)) %>%
  ungroup() %>%
  distinct(season, n_doin_joey_season)

## Get episode titles for relevant episodes
epi_title202 <- friends_info %>% filter(season == 2 & episode == 2) %>% select(title)
epi_title413 <- friends_info %>% filter(season == 1 & episode == 13) %>% select(title)
epi_title819 <- friends_info %>% filter(season == 8 & episode == 19) %>% select(title)

## Get specific utterance for relevant episodes
## First time string is said
epi_utterance_recip101 <- friends%>% filter(season == 1 & episode == 1 & scene == 1 & utterance == howya_friends$utterance[1] + 1) %>% select(speaker)
epi_utterance101 <- glue::glue("{howya_friends$speaker[1]}: \"{str_split(howya_friends$text[1], ' Talk', simplify = TRUE)[1]}\" (to {epi_utterance_recip101})")

## Last time string is said
epi_utterance_recip1002 <- friends%>% filter(season == 10 & episode == 1 & scene == 3 & utterance == howya_friends$utterance[nrow(howya_friends)] + 1) %>% select(speaker)
epi_utterance1002 <- glue::glue("{howya_friends$speaker[nrow(howya_friends)]}: \"{str_split(howya_friends$text[nrow(howya_friends)], ' Talk', simplify = TRUE)[1]}\" (to {epi_utterance_recip1002})")

## Last time Joey says string
epi_utterance_text923 <- howya_friends %>% filter(season == 9 & episode == 23 & speaker == "Joey Tribbiani") %>% select(text)
epi_utterance_setting923 <- friends %>% filter(season == 9 & episode == 23 & scene == 4 & utterance == 1) %>% select(text)
epi_utterance923 <- glue::glue("{epi_utterance_setting923}\n Joey Tribbiani: \"{epi_utterance_text923}\"")

## Set up values for arrows in plot
arrows <- 
  tibble(
    x1 = c(-1.4,-2.5, -4.5, -8.5, -10.4),
    x2 = c(-1.15,-2.15,-4.2,-8.2, -10.15),
    y1 = c(1.8,2.8, 13.8, 19.8, 2.8), 
    y2 = c(1,2, 13, 19, 2)
  )

## This one separate because I want the curvature to be different
arrows2 <- 
  tibble(
    x1 = -9.5,
    x2 = -9.2,
    y1 = 22.2,
    y2 = 23
    )

## Create plot
g <- ggplot(doin_friends, aes(-season, episode)) + 
  geom_point(aes(size = n_doin, color = factor(n_doin)),
             show.legend = F) +
  scale_color_manual(
    values = c("#D6D6D6", "#52b788", "#52b788", "#52b788", "#52b788"),
    name = "n_doin"
  ) +
  geom_point(data = doin_joey, aes(size = n_doin_j, alpha = n_doin_j != 0), 
             color = "#B75281", show.legend = F) +
  # Scale alpha discrete is used to prevent overlapping points when string is said
  # but only by someone else and not Joey, it sets alpha = 0 when n_doin_j == 0
  scale_alpha_discrete(name = "n_j_doin",
                       guide = FALSE, range = c(0,1)) +
  geom_point(
    data = doin_by_season,
    aes(x = -season, y = 27, fill = n_doin_season),
    shape = 21,
    size = 12,
    stroke = 1,
    color = "#FFFFFF",
    show.legend = F
  ) +
  scale_fill_gradient(low = "#95d5b2", high = "#1b4332") +
  new_scale_fill() +
  geom_point(
    data = doin_joey_by_season,
    aes(x = -season, y = 30, fill = n_doin_joey_season),
    shape = 21,
    size = 12,
    stroke = 1,
    color = "#FFFFFF",
    show.legend = F
  ) +
  scale_fill_gradient(low = "#F4E6ED", high = "#B75281") +
  geom_text(
    data = doin_by_season,
    aes(x = -season, y = 27, label = n_doin_season),
    color = "white",
    size = 4,
    show.legend = F
  ) +
  geom_text(
    data = doin_joey_by_season,
    aes(x = -season, y = 30, label = n_doin_joey_season),
    color = "white",
    size = 4,
    show.legend = F
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "\"How you doin'?\"",
    subtitle = "The number of times the utterance \"How you doin'?\" was used by<br> <b><span style = 'color:#52b788;'>anyone</span></b> or by <b><span style = 'color:#B75281;'>Joey</span></b> over the 10 season run of FRIENDS",
    caption = "@winterstat | #TidyTuesday | Data: {friends} by Emil Hvitfeldt | Heavily inspired by: @MaiaPelletier"
  ) +
  coord_flip() +
  scale_x_continuous(
    breaks = seq(-10, -1),
    labels = paste("Season", seq(10, 1, -1))
  ) +
  scale_y_continuous(
    limits = c(1, 30)
  ) +
  annotate(
    "text", x = -2.5, y = 4, size = 2.8, color = "gray20", lineheight = .9, hjust = .1,
    label = glue::glue("S02E02: {epi_title202}")
  ) +
  annotate(
    "text", x = -4.5, y = 15, size = 2.8, color = "gray20", lineheight = .9, hjust = .1,
    label = glue::glue("S04E13: {epi_title413}")
  ) +
  annotate(
    "text", x = -8.5, y = 21, size = 2.8, color = "gray20", lineheight = .9, hjust = .1,
    label = glue::glue("S08E19: {epi_title819}")
  ) +
  annotate(
    "text", x = -1.4, y = 2.2, size = 2.8, color = "gray20",
    lineheight = .9, hjust = .01, fontface = 3,
    label = epi_utterance101
  ) +
  annotate(
    "text", x = -9.5, y = 20.5, size = 2.8, color = "gray20",
    lineheight = 1, hjust = .9, fontface = "italic",
    label = epi_utterance923
  ) +
  annotate(
    "text", x = -10.4, y = 3.2, size = 2.8, color = "gray20",
    lineheight = .9, hjust = .01, fontface = "italic",
    label = epi_utterance1002
  ) +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
    color = "gray20", curvature = -0.3
  ) +
  geom_curve(
    data = arrows2, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
    color = "gray20", curvature = 0.3
  ) +
  theme_classic() +
  theme(
    rect = element_rect(fill = "#F5F5F5"),
    panel.background = element_rect(fill = "#F5F5F5"),
    plot.title = element_markdown(family = "permanent marker", hjust = 0.5, size = 25),
    plot.subtitle = element_markdown(family = "sans",size = 12, hjust = 0.5, lineheight = 1.2),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 12, color = "gray20")
  )

## Save plot
ggsave("TT_week37.png", plot = g, type = 'cairo', width = 7, height = 8, dpi = 300, units = "in")


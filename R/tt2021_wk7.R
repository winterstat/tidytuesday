#### Tidy Tuesday: 2021 Week 7

## Resources used:
## Tutorial for making dumbbell plot: https://toebr.github.io/ggplot2_extended_dumbbell_plot_tutorial/

## Load packages
library(tidyverse)  # For everything lol
library(scales)     # For percentage and dollar x-axes
library("ggsci")    # For Star Trek color pallete
library(ggtext)     # For markdown text options
library(patchwork)  # To combine multiple plots

## Get data
tuesdata <- tidytuesdayR::tt_load(2021, week = 7)

## Extract retirement and wealth data
retirement <- tuesdata$retirement
wealth <-tuesdata$race_wealth

## Merge data
wealth_retire <- wealth %>%
  filter(type == "Average" & race != "Non-White") %>%
  select(!type) %>%
  full_join(retirement, by = c("year", "race"))

## Create pct_retire variable 
wealth_retire <- wealth_retire %>%
  mutate(pct_retire = retirement/wealth_family)

## Plot color prep
show_col(pal_startrek()(5))
col_race <- c("#5C88DAFF", "#CC0C00FF", "#FFCD00FF")
col_line <- "#7C878EFF"

## Plot 1: Average liquid retirement per year by race
wealth_retire %>% filter(year > 1985) %>%
  ggplot(aes(y = as.factor(year), x = retirement)) +
  geom_line(aes(group = year), size = 2, alpha = 0.5, color = col_line) +
  geom_point(aes(color = race), size = 4, alpha = 0.8, show.legend = FALSE) +
  scale_x_continuous(labels = scales::label_dollar()) +
  scale_color_manual(values = col_race, name = "") +
  labs(title = "Average liquid retirement", x = "", y = "") +
  theme_classic() +
  theme(
    rect = element_rect(fill = "#F5F5F5"),
    panel.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
    plot.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
    plot.title = ggtext::element_markdown(size = 14, family = "serif"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(family = "sans", size = 10, lineheight = 1.2),
    panel.grid.minor=element_blank(),
    panel.grid.major=element_blank(),
    strip.text = element_markdown(size = 12, face = "bold"),
    strip.background = element_blank(),
    axis.line.x = element_line(size = 0.5, colour = "gray20"),
    axis.text.x = element_text(size = 12, color = "gray20"),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 12, color = "gray20")
  ) -> p_left

## Plot 2: Liquid retirement as a percentage of overall wealth per year by race
wealth_retire %>% filter(year > 1985) %>%
  ggplot(aes(y = as.factor(year), x = pct_retire)) +
  geom_line(aes(group = year), size = 2, alpha = 0.5, color = col_line) +
  geom_point(aes(color = race), size = 4, alpha = 0.8, show.legend = FALSE) +
  scale_x_continuous(labels = scales::label_percent(), limits = c(0,0.25)) +
  scale_color_manual(values = col_race, name = "") +
  labs(title = "Liquid retirement as a percentage of wealth",
    x = "", y = "") +
  theme_classic() +
  theme(
    rect = element_rect(fill = "#F5F5F5"),
    panel.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
    plot.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
    plot.title = ggtext::element_markdown(size = 14, family = "serif"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(family = "sans", size = 10, lineheight = 1.2),
    panel.grid.minor=element_blank(),
    panel.grid.major=element_blank(),
    strip.text = element_markdown(size = 12, face = "bold"),
    strip.background = element_blank(),
    axis.line.x = element_line(size = 0.5, colour = "gray20"),
    axis.text.x = element_text(size = 12, color = "gray20"),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 12, color = "gray20")
  ) -> p_right


## Combine plots
plot <- p_left + p_right

## Add title, explanation, caption, and several theme elements
plot_final <- plot +
  plot_annotation(
  title = "Family retirement savings by race in the USA",
  subtitle = "Average family liquid retirement (normalized to 2016 dollars) is largest for <b><span style = 'color:#FFCD00FF;'>White</span></b> families<br> 
and the difference between <b><span style = 'color:#FFCD00FF;'>White</span></b> families and <b><span style = 'color:#5C88DAFF;'>Black</span></b> and <b><span style = 'color:#CC0C00FF;'>Hispanic</span></b> families has grown over time. <br>
However, looking at family retirement savings as a percentage of family wealth, it becomes clear <br>
that since 1998, <b><span style = 'color:#5C88DAFF;'>Black</span></b> families have consistently put a larger percentage of their wealth towards <br>
retirement savings compared to <b><span style = 'color:#FFCD00FF;'>White</span></b> and <b><span style = 'color:#CC0C00FF;'>Hispanic</span></b> families.<br>",
  caption = "@winterstat | #TidyTuesday | Data: [Urban Institute and US Census]",
  theme = theme(plot.title = ggtext::element_markdown(size = 20, family = "serif"),
  plot.subtitle = element_markdown(family = "sans", size = 10, lineheight = 1.3),
  plot.caption = element_markdown(colour = col_line))
) &
  theme(rect = element_rect(fill = "#F5F5F5"),
        panel.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
        plot.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"))

## Look at plot
plot_final

## Save plot
ggsave("TT_2021_week7.png", plot = plot_final, type = 'cairo', width = 9, height = 5.5, dpi = 300, units = "in", bg = "#F5F5F5")

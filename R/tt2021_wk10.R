# --------------------------------#
#### Tidy Tuesday: 2021 Week 10 ###
# --------------------------------#

# Bubble plot (plot 2) inspired by: @margaretsiple
# https://github.com/mcsiple/tidytuesday/blob/master/2021/5_2021_plastics.R

# Set-up --------------------------------------------------------------------

## Load packages
library(tidyverse)
library(skimr)
library(waffle) # waffle plots!
library(hrbrthemes) # maybe a cool theme?
hrbrthemes::import_roboto_condensed()
library(gggibbous) # moon plots!
library(patchwork)
library(scales)
library(ggsci)
library(ggtext)


# Data --------------------------------------------------------------------

## Get data
tuesdata <- tidytuesdayR::tt_load(2021, week = 10)
youtube <- tuesdata$youtube

skim(youtube)

# Noticed that Hyundai is misspelled (as Hynudai), so fixing that first
youtube <- youtube %>% 
  mutate(brand = if_else(brand == "Hynudai", "Hyundai", brand))

# Theme distribution over time  ---------------------------------------------

show_col(pal_startrek()(7))

theme_time <- youtube %>% group_by(year) %>%
  summarise(across(where(is.logical), ~sum(.x == T, na.rm = T))) %>%
  pivot_longer(cols = funny:use_sex, names_to = "theme", 
               values_to = "counts", 
               names_transform = list(theme = as.factor),
               values_transform = list(counts = as.numeric))

# Look at how often each theme occurs across years
theme_time %>% group_by(theme) %>%
  summarize(total = sum(counts)) %>%
  arrange(desc(total))

# Order themes by overall popularity
theme_levels <- levels(theme_time$theme)[c(4,6,1,3,2,7,5)]

theme_time <- theme_time %>% 
  mutate(theme = factor(theme, levels = theme_levels,
                        labels = c("Funny", "Shows Product Quickly", "Animals", 
                                   "Danger", "Celebrity", "Uses Sex", "Patriotic"
                        )))

# Reorder the data frame to match the order of the factor, so that geom_waffle will also use this order
theme_time <- theme_time %>% group_by(year) %>%
  arrange(theme, .by_group = TRUE)


## Create plot across all years in two rows

# Using an alternative order together with as.table = FALSE in facet_wrap()
# results in a full lower row and an empty spot in the upper row (to me, this 
# makes more sense with years).

yr_order <- 2000:2020
o <- c(11:21, 1:10)

p1 <- theme_time %>% 
    ggplot(aes(fill = theme, values = counts)) +
  geom_waffle(n_rows = 5, color = "white", flip = TRUE) +
  facet_wrap(~factor(year, levels = yr_order[o], ordered = TRUE), 
             nrow = 2, strip.position = "bottom", as.table = FALSE) +
  scale_fill_jco(name = NULL) +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 5, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  coord_equal() +
  labs(
    title = "Themes in Super Bowl ads over time",
    subtitle = "Ten most advertised companies included. Each block represents one ad.",
    x = "Year",
    y = "Count"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(rect = element_rect(fill = "#F5F5F5"),
        panel.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
        plot.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
        panel.grid = element_blank(), 
        axis.title.y = element_text(hjust = 0),
        axis.ticks.y = element_line(),
        legend.position = "right") +
  guides(fill = guide_legend(reverse = FALSE))
  

# Dotplot using gibbous? ---------------------------------------------------

p2 <- youtube %>%
  group_by(brand) %>%
  summarize_at(vars(funny:use_sex), .funs = ~ sum(.x, na.rm = TRUE)) %>%
  mutate(row_sum = rowSums(select(., -1))) %>%
  mutate_at(-1, ~ . / row_sum) %>%
  select(-row_sum) %>%
  pivot_longer(cols = funny:use_sex, names_to = "theme", values_to = "prop_use") %>%
  mutate(theme = factor(theme, levels = c(theme_levels),
                        labels = c("Funny", "Shows Product Quickly", "Animals", 
                                   "Danger", "Celebrity", "Uses Sex", "Patriotic"
                        ))) %>%
  ggplot(aes(x = theme, y = brand, colour = theme)) +
  geom_point(aes(alpha = prop_use), size = 8, show.legend = FALSE) +
  scale_alpha(range = c(0.4, 1)) +
  geom_moon(aes(ratio = prop_use), size = 8, color = NA) +
  scale_colour_jco(name = NULL) + 
  scale_y_discrete(limits = rev) +
  xlab("Theme") +
  ylab("") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    rect = element_rect(fill = "#F5F5F5"),
    panel.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
    plot.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
    legend.position = "none"
  ) +
  labs(
    title = "What type of ads do these companies create?",
    subtitle = "The white moon shape represents proportions of each ad theme by company."
  )

# Combine plots ------------------------------------------------------------

plot <- p1 + p2 + plot_layout(heights = c(1,2))

plot_final <- plot + plot_annotation(
  caption = "@winterstat | #TidyTuesday | Data: [FiveThirtyEight]",
  theme = theme(plot.caption = element_markdown(family = "Roboto Condensed", colour = "#7C878EFF"))) &
    theme(rect = element_rect(fill = "#F5F5F5"),
          panel.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
          plot.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"))

plot_final

## Save plot
ggsave("TT_2021_week10.png", plot = plot_final, type = 'cairo', width = 9, height = 7, dpi = 300, units = "in", bg = "#F5F5F5")





# Not used, but still cool:
# Which companies make which kinds of ads? ---------------------------------

theme_comp <- youtube %>% group_by(brand) %>%
  summarise(across(where(is.logical), ~sum(.x == T, na.rm = T))) %>%
  pivot_longer(cols = funny:use_sex, names_to = "theme", 
               values_to = "counts", 
               names_transform = list(theme = as.factor),
               values_transform = list(counts = as.numeric)) %>%
  mutate(theme = factor(theme, levels = c('', theme_levels),
                        labels = c("","Funny", "Shows Product Quickly", "Animals", 
                                   "Danger", "Celebrity", "Uses Sex", "Patriotic"
                        )))

# Add empty category so that inner part of circle diagram is not used
# If you don't do this, then the first category (funny) will be on the inner most
# spot and will always look like a dot, and not convey how often it's used compared
# to other themes.
theme_comp_empty <- theme_comp %>%
  filter(theme == "Funny") %>%
  group_by(brand) %>%
  summarise(counts = 0) %>%
  mutate(theme = factor("", levels = c('', theme_levels),
                        labels = c("","Funny", "Shows Product Quickly", "Animals", 
                                   "Danger", "Celebrity", "Uses Sex", "Patriotic"
                        )))

theme_comp <- bind_rows(theme_comp, theme_comp_empty)

theme_comp %>%
  group_by(brand, theme) %>%
  summarize(totvalue = sum(counts, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(brand) %>%
  mutate(prop_val = totvalue / sum(totvalue)) %>%
  View(.)


show_col(pal_jco("default")(10))

cust_pal <- c(NA, "#0073C2FF", "#EFC000fF", "#868686FF", "#CD534CFF", "#7AA6DCFF", "#003C67FF", "#8F7700FF")

## Long format plot
p3 <- theme_comp %>%
  group_by(brand, theme) %>%
  summarize(totvalue = sum(counts, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(brand) %>%
  mutate(prop_val = totvalue / sum(totvalue)) %>%
  ggplot(aes(fill = theme, colour = theme)) +
  scale_colour_manual(name = "Themes", values = cust_pal) +
  scale_x_discrete(drop=FALSE) + 
  geom_segment(aes(
    x = theme, xend = theme,
    y = 0.02, yend = prop_val + 0.02
  ),
  lwd = 2, lineend = "round"
  ) +
  ylim(c(0, 1.2)) +
  xlab("Type of Ad") +
  ylab("") +
  coord_polar(theta = "y") +
  facet_wrap(~brand, ncol = 2) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    rect = element_rect(fill = "#F5F5F5"),
    panel.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
    plot.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
    axis.title.y = element_text(hjust = 1, size = 8),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "right",
    legend.justification = c(0, 1)
  )

## Wide format plot
p3b <- theme_comp %>%
  group_by(brand, theme) %>%
  summarize(totvalue = sum(counts, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(brand) %>%
  mutate(prop_val = totvalue / sum(totvalue)) %>%
  ggplot(aes(fill = theme, colour = theme)) +
  scale_colour_manual(name = "Themes", values = cust_pal) +
  scale_x_discrete(drop=FALSE) + 
  geom_segment(aes(
    x = theme, xend = theme,
    y = 0.02, yend = prop_val + 0.02
  ),
  lwd = 2, lineend = "round"
  ) +
  ylim(c(0, 1.2)) +
  xlab("Type of Ad") +
  ylab("") +
  coord_polar(theta = "y") +
  facet_wrap(~brand, nrow = 2) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    rect = element_rect(fill = "#F5F5F5"),
    panel.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
    plot.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
    axis.title.y = element_text(hjust = 1, size = 8),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "right",
    legend.justification = c(0, 1)
  )


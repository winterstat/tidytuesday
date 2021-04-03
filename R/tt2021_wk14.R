# --------------------------------#
#### Tidy Tuesday: 2021 Week 14 ###
# --------------------------------#


# Set-up --------------------------------------------------------------------

## Load packages
library(tidyverse)
library(scales)
library(ggtext)
library(showtext)


font_add_google("Open Sans", "open")
font_add_google("Roboto Condensed", "roboto")
font_add_google("Zilla Slab Highlight", "zilla")

## Automatically use showtext to render text
showtext_auto()

# Data --------------------------------------------------------------------

## Get data
tuesdata <- tidytuesdayR::tt_load(2021, week = 14)

allCategories <- tuesdata$allCategories
allShades <- tuesdata$allShades

# Add hue and saturation to allCategories, have to remove "name" collumn to make
# join work
allCategories <- allShades %>%
  select(!name) %>%
  left_join(allCategories, .) %>%
  separate_rows(categories)

# Find 10 most popular categories
top10 <- allCategories %>% 
  mutate(categories = str_to_title(categories)) %>%
  count(categories,sort=TRUE) %>% 
  slice(1:10) %>% 
  .$categories

label_cols <- allCategories %>%
  mutate(lightness_cat = cut_interval(lightness, 10)) %>%
  group_by(lightness_cat) %>%
  slice(1) %>%
  mutate(hex_cat = hex, 
         name_cat = str_to_title(name)) %>%
  select(hex_cat, name_cat, lightness_cat)

plot_dat <- allCategories %>%
  mutate(lightness_cat = cut_interval(lightness, 10)) %>%
  mutate(categories = str_to_title(categories)) %>%
  filter(categories %in% top10) %>%
  group_by(lightness_cat, categories) %>%
  summarize(n_cat = n()) %>%
  left_join(., label_cols) %>%
  ungroup() %>%
  mutate(xlabels = paste0("<span style = 'color: ", hex_cat,
                          ";'>", lightness_cat, 
                          "</span>"),
         xlabels = fct_reorder(xlabels, as.numeric(lightness_cat)),
         categories = factor(categories, levels = c("Drink", "Wood", "Location","Food", "Descriptor",
                                                    "Skin", "Misc", "Rock", "Color", "Gem")))


# Create Plot -----------------------------------------------------

# Only run this line when you're saving it!
# Specify dpi (otherwise text will be tiny!)
op = showtext_opts(dpi = 300)

ggplot(plot_dat, aes(x = xlabels, y = n_cat, group = categories, fill = categories)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::label_percent(), expand = expansion(mult = c(0, .05),
                                                                          add = c(0, 0))) + 
  scale_fill_brewer(palette = "Spectral") +
  labs(x = "",
       y = "",
       title = "Bias in Foundation Names",
       subtitle = "Representation of different name categories across foundation shade levels",
       caption = "@winterstat | #TidyTuesday | Data: [The Pudding]") + 
  coord_flip() +
  theme_minimal() + 
  theme(text = element_text(family = "roboto"),
    rect = element_rect(fill = "#9EA0A5"),
        panel.background = element_rect(fill = "#9EA0A5", color = "#9EA0A5"),
        plot.background = element_rect(fill = "#9EA0A5", color = "#9EA0A5"),
        legend.position = "bottom",
        legend.justification = "center",
    legend.text = element_text(size = 9),
    plot.title = element_text(color = "white", face = "bold", 
                              size = 22, hjust = .5, family = "open"),
    plot.subtitle = element_text(color = "white", 
                                 size = 14, hjust = .5, family = "open"),
    plot.caption = element_text(color = "white", family = "open", size = 8),
        axis.text.y = element_markdown(size = 14, face = "bold", family = "zilla"),
        axis.text.x = element_text(color = "#ffffff"),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE, title = NULL))

# Save plot
ggsave("TT_week14.png", type = 'cairo', width = 10, height = 5, dpi = 300, units = "in", bg = "#9EA0A5")


  
  

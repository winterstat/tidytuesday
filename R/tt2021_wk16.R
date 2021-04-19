# --------------------------------#
#### Tidy Tuesday: 2021 Week 16 ###
# --------------------------------#

# Citation: Blevins, Cameron; Helbock, Richard W., 2021, 
# "US Post Offices", https://doi.org/10.7910/DVN/NUKCNA, 
# Harvard Dataverse, V1, UNF:6:8ROmiI5/4qA8jHrt62PpyA== [fileUNF]

# Set-up --------------------------------------------------------------------

## Load packages
library(tidyverse)
library(scales)
library(ggtext)
library(showtext)
library(ggsci)
library(waffle)


font_add_google("Open Sans", "open")
font_add_google("Roboto Condensed", "roboto")

## Automatically use showtext to render text
showtext_auto()

# Data --------------------------------------------------------------------

## Get data
tuesdata <- tidytuesdayR::tt_load(2021, week = 16)

post_offices <- tuesdata$post_offices
rm(tuesdata)

post_offices %>% filter(established > 1630) %>% 
  mutate(decade = cut(established, breaks = seq(from = 1630, to = 2000, by = 10))) %>%
  group_by(decade) %>%
  summarize(n_est = n()) -> decade_established

post_offices %>% filter(discontinued > 1630 & !is.na(discontinued) & discontinued < 2001) %>% 
  mutate(decade = cut(discontinued, breaks = seq(from = 1630, to = 2000, by = 10))) %>%
  group_by(decade) %>%
  summarize(n_disc = n()) -> decade_discontinued

decade_est_disc <- full_join(decade_established, decade_discontinued)

decade_labels <- paste0(seq(1630, 1990, by = 10), "s")
decade_est_disc$decade <- factor(decade_est_disc$decade, levels = levels(decade_est_disc$decade),
                                 labels = decade_labels)

decade_est_disc <- decade_est_disc %>%
  pivot_longer(cols = n_est:n_disc, names_to = "status", names_prefix = "n_", values_to = "value") %>%
  mutate_if(is.numeric,  ~replace(., is.na(.), 0)) %>%
  mutate(value10 = value / 10,
    value100 = value / 100,
    value_comb = if_else(as.numeric(decade) < 17, value, value100),
    status = factor(status, levels = c("est", "disc"), labels = c("Established", "Discontinued")))

# Create Plot -----------------------------------------------------

# USPS colors: blue, red
palette <- c("#004B87", "#DA291C")

# Reverse order of years:
o <- c(28:37, 18:27, 1:17)

# Only run this line when you're saving it!
# Specify dpi (otherwise text will be tiny!)
op = showtext_opts(dpi = 300)

ggplot(decade_est_disc, aes(fill = status, values = value)) +
  geom_waffle(n_rows = 10, color = "white", flip = TRUE, make_proportional = TRUE, show.legend = FALSE) +
  coord_equal() +
  facet_wrap(~factor(decade, 
                     levels = levels(decade_est_disc$decade)[o], 
                     ordered = TRUE), 
             as.table = FALSE, nrow = 3, ncol = 10, 
             strip.position = "bottom") +
  scale_fill_manual(name = NULL, values = palette) +
  scale_x_discrete() + 
  labs(
    title = "US Post Offices per Decade (1630s-1990s)",
    subtitle = "Proportional Breakdown of 
    <b><span style = 'color:#004B87;'>Established</span></b> and 
    <b><span style = 'color:#DA291C;'>Discontinued</span></b> 
    Post Offices<br>",
    caption = "winterstat | #TidyTuesday | Data: [Blevins & Helbock, 2021]",
    x = "",
    y = ""
  ) +
  theme_minimal(base_family = "roboto") +
  theme(rect = element_rect(fill = "#F5F5F5"),
        panel.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
        plot.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
        panel.grid = element_blank(),
        plot.title = element_text(family = "open", face = "bold"),
        plot.subtitle = element_markdown(family = "roboto"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

# Save plot
ggsave("TT_week16.png", type = 'cairo', width = 10, height = 5, dpi = 300, units = "in", bg = "#F5F5F5")


  
  

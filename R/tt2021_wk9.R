# --------------------------------#
#### Tidy Tuesday: 2021 Week 9 ####
# --------------------------------#


# Set-up --------------------------------------------------------------------

## Load packages
library(tidyverse)
library(cowplot)
library(scales)
library(ggsci)
library(ggtext)
library(showtext)

# Specify dpi (otherwise text will be tiny!)
op = showtext_opts(dpi = 300)

## Loading Google fonts (https://fonts.google.com/)
##  font-family: 'Quicksand', used for everything except title and percentages;
font_add_google("Reenie Beanie", "reenie")
##  font-family: 'Big Shoulders', used for percentages
#font_add_google("Big Shoulders Display", "shoulders")
## font-family: 'Mono', used for title

## Automatically use showtext to render text
showtext_auto()

# Data --------------------------------------------------------------------

## Get data
tuesdata <- tidytuesdayR::tt_load(2021, week = 9)
employed <- tuesdata$employed

## Do some cleaning
employed <- employed %>%
  mutate(minor_occupation = str_replace(minor_occupation, "-", ""))

## Extract just 2019 and 2020 and remove a few strange industry categories
employed1920 <- employed %>% 
  filter(year == 2020 | year == 2019) %>%
  filter(!(industry %in% c("White", "Women", "Asian", "Men")))

## Do some not so nice acrobatics to get the data how I want. Can
## probably do this in a smarter way, but it worked for now.
employed1920_gender <- employed1920 %>% 
  drop_na() %>%
  filter(race_gender %in% c("Men", "Women")) %>%
  pivot_wider(names_from = year, values_from = c(industry_total, employ_n)) %>%
  group_by(minor_occupation, race_gender) %>%
  summarise(industry_total_2019 = max(industry_total_2019),
            industry_total_2020 = max(industry_total_2020),
            employ_n_2020 = sum(employ_n_2020),
            employ_n_2019 = sum(employ_n_2019)) %>%
  pivot_wider(names_from = race_gender, values_from = c(industry_total_2019, industry_total_2020,
                                                        employ_n_2019, employ_n_2020)) %>%
  mutate(minor_occupation = str_replace(minor_occupation, " occupations", ""),
         employ_n_2019 = sum(employ_n_2019_Men, employ_n_2019_Women),
         employ_n_2020 = sum(employ_n_2020_Men, employ_n_2020_Women),
         employ_n_fem_2019 = employ_n_2019_Women / employ_n_2019,
         employ_n_fem_2020 = employ_n_2020_Women / employ_n_2020,
         maj_fem_2019 = if_else(employ_n_fem_2019 < .5, 0, 1),
         employ_n_diff = (employ_n_2020 - employ_n_2019),
         employ_n_diff_p = (employ_n_2020 - employ_n_2019)/employ_n_2019)

# Plot --------------------------------------------------------------------

## Pick colors
show_col(pal_rickandmorty("schwifty")(12))
plot_pal <- c("#B7E4F9FF", "#FB6467FF")

## Create plot
g <- ggplot(employed1920_gender, 
            aes(x = employ_n_fem_2019, 
                y = employ_n_diff_p, 
                label = minor_occupation)) +
  ## Add points for different fields, color depends on if field is majority-female
  geom_point(aes(fill = factor(maj_fem_2019),
                 color = factor(maj_fem_2019)),
             show.legend = FALSE, size = 2.5) +
  ## Add lines at 0% growth and 50% female-share of jobs
  geom_hline(yintercept = 0, color = "#FAFAFA") +
  geom_vline(xintercept = 0.5, color = "#FAFAFA") +
  ## Add labels to field-points. ggrepel ensures that labels don't overlap
  ggrepel::geom_text_repel(point.padding = 0.5, 
                           min.segment.length = 0, 
                           seed = 79, box.padding = 0.5,
                           arrow = arrow(length = unit(0.015, "npc"))) +
  ## Set color/fill for points
  scale_color_manual(values = plot_pal) +
  scale_fill_manual(values = plot_pal) +
  ## Set scale labels to be percentages
  scale_x_continuous(labels = label_percent(), limits = c(0, 1)) +
  scale_y_continuous(labels = label_percent()) +
  ## Title etc.
  labs(title = "Change in employment in the USA from 2019 to 2020",
       subtitle = "Comparing majority <b><span style = 'color:#B7E4F9FF;'>male</span></b> to majority <b><span style = 'color:#FB6467FF;'>female</span></b> fields",
       caption = "@winterstat | #TidyTuesday | Data: {Employment} by BLS",
       x = "Percentage of jobs occupied by women in 2019",
       y = "Percentage change in employment from 2019 to 2020") +
  ## Add annotation about insane growth transport field
  annotate(
    "text", x = 0, y = .09, size = 6, color = "#FAFAFA", lineheight = .9, hjust = .1,
    label = "Did we really order that much online?", family = "reenie"
  ) +
  ## Add rectangle to highlight no growth for majority-female fields
  geom_rect(aes(xmin = .501, xmax = Inf, ymin = 0.0001, ymax = Inf), fill = "#FB6467FF", alpha = .1) +
  ## Add annotation to highlight no growth for majoriy-female fields
  annotate(
    "text", x = .8, y = .068, size = 9, color = "#FAFAFA", lineheight = 1, hjust = .5,
    label = "None of the (just!) three\nmajority-female fields\nexperienced growth in 2020!",
    family = "reenie"
  ) +
  ## Set theme elements
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.background = element_rect(fill = "#b0b0b0"),
        panel.background = element_rect(fill = "#b0b0b0"),
        plot.title = element_text(size = 24, family = "serif", color = "#FAFAFA", face = "bold",
                                      margin = unit(c(2, 0, 2, 0), "pt")),
        plot.subtitle = element_markdown(family = "sans", size = 12, lineheight = 1.3),
        plot.caption = element_text(colour = "#FAFAFA"),
        axis.title = element_text(size = 14, color = "#FAFAFA"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text = element_text(size = 12, color = "#FAFAFA"))

## Look at plot
g

## Save plot using cowplot funcion because ggsave() wasn't rendering text_markdown elements
## correctly.
save_plot("TT_2021_week9.png", plot = g, base_width = 9, base_height = 5.5)

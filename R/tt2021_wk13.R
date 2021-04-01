# --------------------------------#
#### Tidy Tuesday: 2021 Week 13 ###
# --------------------------------#

# Inspired by: -------------------------------------------------------------

# Person: Tessa Eagle, @tessuheagle
# Code: https://github.com/tessaeagle/TidyTuesday/blob/master/2021/3_2_21.R

# Set-up --------------------------------------------------------------------

## Load packages
library(tidyverse)
#remotes::install_github("davidsjoberg/ggstream")
library(ggstream)
library(scales)
library(patchwork)
hrbrthemes::import_roboto_condensed()


# Data --------------------------------------------------------------------

## Get data
tuesdata <- tidytuesdayR::tt_load(2021, week = 13)
unvotes <- tuesdata$unvotes
roll_calls <- tuesdata$roll_calls
issues <- tuesdata$issues

# Extract relevant info about the Netherlands and what session (year) issues
# were voted on
netherlands <- unvotes %>% filter(country == "Netherlands")
netherlands <- roll_calls %>% select(rcid, session, short) %>%
  left_join(netherlands, ., by = "rcid")

netherlands <- left_join(netherlands, issues, by = "rcid")

netherlands <- netherlands %>% 
  mutate(year = session + 1945,
         vote = factor(vote, levels = c("no", "abstain", "yes"),
                       labels = c("No", "Abstain", "Yes")))

NL_prop <- netherlands %>% group_by(year, vote) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

# Extract relevant info about the United States and what session (year) issues
# were voted on
us <- unvotes %>% filter(country == "United States")
us <- roll_calls %>% select(rcid, session, short) %>%
  left_join(us, ., by = "rcid")

us <- left_join(us, issues, by = "rcid")

us <- us %>% 
  mutate(year = session + 1945,
         vote = factor(vote, levels = c("no", "abstain", "yes"),
                       labels = c("No", "Abstain", "Yes")))

US_prop <- us %>% group_by(year, vote) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

# Set up vertical lines
lines <- NL_prop %>%
  filter(year %in% seq(1950, 2010, by = 10))


# Create Plot -----------------------------------------------------

# Set up color palettes
dutch_red <- "#AE1C28"
dutch_white <- "#FFFFFF"
dutch_blue <- "#21468B"

nl_palette = c(`No` = dutch_red,
                   `Abstain` = dutch_white,
                   `Yes` = dutch_blue)

us_red <- "#B22234"
us_white <- "#FFFFFF"
us_blue <- "#3C3B6E"

us_palette = c(`No` = us_red,
                  `Abstain` = us_white,
                  `Yes` = us_blue)

label_palette = c(`No` = "#FFFFFF",
                  `Abstain` = "#000000",
                  `Yes` = "#FFFFFF")

# Create stream plots for the Netherlands (p1) and the US (p2)
p1 <- ggplot(NL_prop, aes(year, prop, fill = vote))+
  geom_stream(color = "#c6c9cf", extra_span = .15, true_range = "none") +
  geom_vline(data = lines, aes(xintercept = year), 
             linetype = "dotted", color = "#c6c9cf", size = .7) +
  scale_colour_manual(values = label_palette) +
  geom_stream_label(aes(label = toupper(vote), colour = vote), 
                    size = 4, type = "mirror", extra_span = .15,
                    family = "Roboto Condensed") +
  scale_fill_manual(values = nl_palette) +
  scale_x_continuous(position = "top", 
                     breaks = seq(1950, 2010, by = 10)) +
  theme(
    panel.background = element_rect(fill = "#9EA0A5"),
    plot.background = element_rect(fill = "#9EA0A5"),
    text = element_text(family = "Roboto Condensed"),
    plot.title = element_text(color = "white", face = "bold", 
                              size = 16, hjust = .5),
    plot.subtitle = element_text(color = "white", 
                                 size = 11, hjust = .5),
    plot.caption = element_text(color = "white", size = 9),
    axis.text.x = element_text(color = "white", size = 10),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    panel.grid  = element_blank(),
    legend.position = "none"
  ) +
  labs(
    y = "",
    x = "",
    title = "Netherlands"
  )

p2 <- ggplot(US_prop, aes(year, prop, fill = vote))+
  geom_stream(color = "#c6c9cf", extra_span = .15, true_range = "none")+
  geom_vline(data = lines, aes(xintercept = year), 
             linetype = "dotted", color = "#c6c9cf", size = .7)+
  geom_stream_label(aes(label = toupper(vote), colour = vote), 
                    size = 4, type = "mirror", extra_span = .15,
                    family = "Roboto Condensed") +
  scale_colour_manual(values = label_palette) +
  scale_fill_manual(values = us_palette) +
  scale_x_continuous(position = "top", 
                     breaks = seq(1950, 2010, by = 10)) +
  theme(
    panel.background = element_rect(fill = "#9EA0A5"),
    plot.background = element_rect(fill = "#9EA0A5"),
    text = element_text(family = "Roboto Condensed"),
    plot.title = element_text(color = "white", face = "bold", 
                              size = 16, hjust = .5),
    plot.subtitle = element_text(color = "white", 
                                 size = 11, hjust = .5),
    plot.caption = element_text(color = "white", size = 9),
    axis.text.x = element_text(color = "white", size = 10),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    panel.grid  = element_blank(),
    legend.position = "none"
  )+
  labs(
    y = "",
    x = "",
    title = "United States"
  )

# Combines plots, add titles etc., and adjust formatting
plot <- p1 + p2

plot_final <- plot + plot_annotation(
  title = "Different Attitudes",
  subtitle = "Proportion of votes cast on issues brought up during UN sessions (1946-2019)",
  caption = "@winterstat | #TidyTuesday | Data: [Harvard's Dataverse]") &
  theme(rect = element_rect(fill = "#9EA0A5"),
        panel.background = element_rect(fill = "#9EA0A5", color = "#9EA0A5"),
        plot.background = element_rect(fill = "#9EA0A5", color = "#9EA0A5"),
        text = element_text(family = "Roboto Condensed"),
        plot.title = element_text(color = "white", face = "bold", size = 22, hjust = .5),
        plot.subtitle = element_text(color = "white", size = 11, hjust = .5),
        plot.caption = element_text(color = "white", size = 9),
  )

# Check if it looks alright
plot_final

# Save plot
ggsave("TT_week13.png", plot = plot_final, type = 'cairo', width = 10, height = 5, dpi = 300, units = "in", bg = "#9EA0A5")


  
  

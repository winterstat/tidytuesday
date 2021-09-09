# --------------------------------#
#### Tidy Tuesday: 2021 Week 37 ###
# --------------------------------#


# Set-up --------------------------------------------------------------------

## Load packages
library(tidyverse)
library(lubridate) # For time in seconds
library(ggtext) # For markdown
library(patchwork)  # To combine multiple plots
library(showtext) # For custom fonts

# Font for most plot elements:
font_add_google("Roboto Condensed", "roboto") # caption

# Font for title (locally saved):
font_add(family = "f1_wide", regular = "./Users/sonja/Library/Fonts/Formula1-Wide.otf")

## Automatically use showtext to render text
showtext_auto()

# Data --------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 37)

constructors <- tuesdata$constructors
constructors_standings <- tuesdata$constructor_standings
constructors_results <- tuesdata$constructor_results

pit_stops <- tuesdata$pit_stops

drivers <- tuesdata$drivers

race_info <- tuesdata$races
race_results <- tuesdata$results

rm(tuesdata)

# Wrangle the Data --------------------------------------------------------

# Add info about drivers
pit_stops <- drivers %>%
  dplyr::select(driverId, surname, nationality) %>%
  right_join(pit_stops, by = "driverId")

# Add info about the constructor
pit_stops <- race_results %>%
  select(raceId, driverId, constructorId) %>%
  right_join(pit_stops, by = c("raceId", "driverId"))

# Add more constructor info
pit_stops <- constructors %>%
  select(constructorId, name) %>%
  right_join(pit_stops, by = "constructorId")

# Add more info about the race, and rename the duplicate name variables
pit_stops <- race_info %>%
  select(raceId, year, name) %>%
  right_join(pit_stops, by = "raceId") %>%
  rename(grandprix = name.x) %>%
  rename(teamname = name.y)

# Explore Data ------------------------------------------------------------

# What teams are included in the data?
pit_stops %>%
  group_by(teamname, year) %>%
  tally()

# Remove teams that no longer exist:
# Caterham, Virgin, Marussia, Manor Marussia, Lotus, Lotus F1, HRT
dead_teams <- c("Caterham", "Virgin", "Marussia", "Manor Marussia", "Lotus", "Lotus F1", "HRT")

pit_stops <- pit_stops %>%
  filter(!teamname %in% dead_teams)

# Need to ensure that some teams are linked, because even though names have changed,
# they are the same team.
# Renault = Alpine
# Force India = Racing Point = Aston Martin
# Toro Rosso = Alpha Tauri
# Sauber = Alpha Romeo

pit_stops %>%
  group_by(teamname) %>%
  summarize(cid = min(constructorId)) %>%
  mutate(betterconstructorId = c(1, 2, 3, 4, 5, 4, 6, 7, 8, 4, 9, 3, 1, 2, 10)) -> betterId
  
pit_stops <- betterId %>%
  select(-cid) %>%
  right_join(pit_stops, by = "teamname")

team_labels <- pit_stops %>% group_by(betterconstructorId) %>%
  summarize(team_labels = paste(unique(teamname), collapse = ', '))

# Get the chronological order of the grand prix's during the 2020 season
grandprix_order <- race_info %>%
  filter(year == 2020) %>%
  select(round, name) %>%
  mutate(name = str_replace(name, pattern = " Grand Prix", replacement = ""))

# Get the shortest pitstop time for each race
min_pittime <- pit_stops %>%
  filter(year == 2020) %>%
  group_by(grandprix) %>%
  summarize(min_all = min(milliseconds))

# Data for Plot -----------------------------------------------------------

pit_stop_plot <- pit_stops %>%
  filter(year == 2020) %>%
  right_join(min_pittime, by = "grandprix") %>% 
  group_by(grandprix, teamname) %>%
  summarize(min_pit = min(milliseconds),
            dif_pit = min_pit - min_all,
            dif_pit2 = lubridate::milliseconds(x = dif_pit)) %>%
  mutate(grandprix = str_replace(grandprix, pattern = " Grand Prix", replacement = ""),
         grandprix = factor(grandprix, levels = grandprix_order$name)) %>%
  distinct()

# Extract fastest constructor at each race
fastest_constructor <- pit_stop_plot %>%
  filter(dif_pit == 0) 

# Look at how much slower Leclerc's super slow pit stop was
pit_stop_plot %>%
  filter(teamname == "Ferrari" & grandprix == "Styrian") %>%
  View()

# Plot --------------------------------------------------------------------

# Create color palette for the teams. Order is:
# Alfa Romeo, Alpha Tauri, Ferrari, Haas, McLaren, Mercedes, 
# Racing Point, Red Bull, Renault, Williams
team_colors <- c("#960000", "#C8C8C8", "#C00000", "#787878", "#FF8700", "#00D2BE", 
                 "#F596C8", "#0600EF", "#FFF500", "#0082FA")

# Additional plot colors
col_line <- "#7C878EFF"
f1_red <- "#FF1E00"
f1_black <- "#15151E"
f1_grey <- "#B8B8BB"

# Long annotations
sub_text <- "Which constructor completed the fastest pit stop at each grand prix<br>during the 2020 season? And how much slower were the other constructors?"
ferrari_text <- "Leclerc and Vettel\ncollided on lap 1,\nretiring Vettel and\nleading to the slowest\npit stop for Leclerc\n(> 18s slower than the\nfastest stop)"

# Create plot (code is so ugly...)
plot <- ggplot(pit_stop_plot, aes(x = dif_pit2, y = grandprix, colour = teamname, fill= teamname)) +
  geom_line(aes(group = grandprix), size = 3, alpha = 0.5, color = col_line) +
  geom_point(size = 3) +
  geom_point(data = fastest_constructor, aes(x = -0.5, y = grandprix, 
                                             colour = teamname, fill = teamname, size = 5), 
             show.legend = FALSE) +
  geom_text(data = fastest_constructor, aes( x = -0.8, y = grandprix, label = teamname),
            hjust = 0, size = 3, color = f1_grey) +
  scale_x_reverse(breaks = 0:7) +
  scale_y_discrete(limits = rev(levels(pit_stop_plot$grandprix))) +
  coord_cartesian(xlim = c(7, -2), clip = "off") +
  scale_color_manual("", values = team_colors) +
  scale_fill_manual("", values = team_colors) +
  annotate("text", x = 8, y = 18, label = "Grand Prix:", color = f1_grey, fontface = 2, family = "roboto") +
  annotate("text", x = 6.3, y = 13.75, label = ferrari_text, color = f1_grey, size = 2.5, family = "roboto") +
  geom_curve(
    aes(x = 6.8, y = 14, xend = 7.1, yend = 15.75),
    arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
    color = f1_red, curvature = -0.3
  ) +
  labs(title = "F1 2020", 
       subtitle = sub_text,
       caption = "@winterstat | #TidyTuesday | Data: {Ergast API} via Sara Stoudt",
       x = "Difference from fastest (sec)", y = "") +
  theme_minimal() +
  theme(text = element_text(family = "roboto"),
    legend.position = "bottom",
        legend.justification = "left",
        legend.text = element_text(color = f1_grey),
        plot.margin = margin(t = 0.25, l = .1, r = .1, unit = "inch"),
        rect = element_rect(fill = f1_black),
        panel.background = element_rect(fill = f1_black, color = f1_black),
        plot.background = element_rect(fill = f1_black, color = f1_black),
        plot.title = ggtext::element_markdown(size = 30, family = "f1_wide", color = f1_red),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(size = 10, lineheight = 1.2, color = f1_grey, margin = margin(0, 0, 20, 0, "pt")),
    plot.caption = element_markdown(size = 8, color = f1_grey),
    panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        strip.text = element_markdown(size = 12, face = "bold", color = f1_grey),
        strip.background = element_blank(),
        axis.line.x = element_line(size = 0.5, colour = f1_grey),
        axis.title.x = element_text(size = 10, color = f1_grey),
        axis.text.x = element_text(size = 10, color = f1_grey),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 10, color = f1_grey)
  )
  

# Only run this line when you're saving it!
# Specify dpi (otherwise text will be tiny!)
op = showtext_opts(dpi = 300)

# Save plot
ggsave("TT2021_week37.png", plot = plot, dpi = 300, width = 10, height = 7, units = "in", bg = f1_black)

  

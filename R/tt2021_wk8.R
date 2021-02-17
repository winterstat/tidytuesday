## This plot is part of the #DuBoisChallenge. 
## I am recreating the plot for Challenge 03.
## Additional info can be found here:
## https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge


## I have to give 100% credit to Jack Davison  for the super smart method for creating empty space
## https://github.com/jack-davison/TidyTuesday/blob/master/R/2021_02_16_dubois.R

# Set-up --------------------------------------------------------------------

## Load packages
library(tidyverse)
library(cowplot)
library(scales)
library(showtext)

# Specify dpi (otherwise text will be tiny!)
op = showtext_opts(dpi = 300)

## Loading Google fonts (https://fonts.google.com/)
##  font-family: 'Quicksand', used for everything except title and percentages;
font_add_google("Quicksand", "quicksand")
##  font-family: 'Big Shoulders', used for percentages
font_add_google("Big Shoulders Display", "shoulders")
## font-family: 'Mono', used for title

## Automatically use showtext to render text
showtext_auto()

# Data --------------------------------------------------------------------

## Get data
tuesdata <- tidytuesdayR::tt_load(2021, week = 8)

## Doing Challenge 3
occupation <- tuesdata$occupation

## Clean/Prep data
occup <- occupation %>%
  janitor::clean_names() %>% #make var names all lowercase
  mutate(plot_perc = percentage / sum(percentage)) %>%
  mutate(plot_perc = (plot_perc * 61)/100) %>% #create white space in plot
  bind_rows(tibble(group = c("Negroes", "Whites"), #add rows for these empty spaces
                   occupation = c("empty", "empty"),
                   percentage = c(0,0),
                   plot_perc = c(0.195, 0.195))) %>%
  mutate(occupation = factor(occupation, #makeoccupation variable into a factor
                             levels = c("Agriculture, Fisheries and Mining",
                                        "Domestic and Personal Service",
                                        "Manufacturing and Mechanical Industries",
                                        "Trade and Transportation",
                                        "Professions",
                                        "Empty"))) %>%
  # In next line, I try to create a variable that will denote the size of the percentages in the plot
  mutate(perc_size = if_else(occupation == "Professions" & group == "Negroes", 1,2)) %>%
  arrange(desc(group), occupation) %>%
  mutate(ypos = cumsum(plot_perc) - 0.5 * plot_perc) %>%
  arrange(occupation)

# There is probably a better way  to do this, but I ended up 
#just manually adjusting the perc_size values:
occup$perc_size <- c(3, 3, 2, 3, 3, 3, 3, 2, 3, 1, NA, NA)


## This data is for the legend, which is added separately
lab_dat <- occup %>%
  select(occupation) %>%
  distinct() %>%
  drop_na() %>%
  mutate(x = c(-1.5, 1.5, -1.5, 1.5, 1.5), #for location of the labels of the legend
         y = c(4, 5, 2, 1, 3),
         occupation = case_when(
           occupation == "Agriculture, Fisheries and Mining" ~ "AGRICULTURE, FISHERIES\nAND MINING.",
           occupation == "Domestic and Personal Service" ~ "DOMESTIC AND\nPERSONAL SERVICE.",
           occupation == "Manufacturing and Mechanical Industries" ~ "MANUFACTURING AND\nMECHANICAL INDUSTRIES.",
           occupation == "Trade and Transportation" ~ "TRADE AND\nTRANSPORTATION.",
           occupation == "Professions" ~ "PROFESSIONS."
         ))

# Visualisations ------------------------------------------------------------

## The set-up is ALLL Jack Davison! I only adjusted the fonts, colors, and other esthethics. 
# First the pie.
# We need to use group = group so the two racial groups are considered separately
# We use color = occupation == "empty" so that only the "true" wedges have borders, not the invisible ones
# na.translate gets rid of the empty wedges


## Colors used by Sonja:
# Black (title): #000000
# Brown (lines, small text): #654321
# Crimson (agriculture): #dc143c
# Gold (domestic): #ffd700
# Blue (manufacturing): #5a6796
# Linen (trade): #ece5d5
# Tan (professions): #d2b48c
# Background: #faf0e6

pie <- ggplot(occup, aes(y = "", x = plot_perc, fill = occupation, group = group)) +
  # With size = 0.15 the lines are not as noticable
  geom_col(aes(color = occupation == "empty"), size=0.15) +
  coord_polar(start = pi - (55*pi)/180) +
  scale_fill_manual(values = c("#dc143c", "#ffd700", "#5a6796", "#ece5d5", "#d2b48c"), na.translate = F) +
  # Lines are brown, not black. NA is for the empty areas
  scale_color_manual(values = c("#654321", NA)) +
  guides(color = guide_none()) +
  labs(title = "OCCUPATIONS OF NEGROES AND WHITES IN GEORGIA.") +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#faf0e6", color = "#faf0e6"),
        plot.background = element_rect(fill = "#faf0e6", color = "#faf0e6"),
        text = element_text(family = "mono"),
        plot.title = element_text(hjust = .5, face = "bold"), 
        plot.margin = unit(rep(.5,4), "cm")) +
  # perc_size adjusts the size of the percentages so they all fit in their pie slices
  geom_text(aes(x = ypos, size = perc_size,
                label = ifelse(occupation == "empty", NA, glue::glue("{round(percentage,1)}%"))),
            nudge_y = .35, family = "shoulders", colour = "#654321") +
  # set up the scale for the percentage sizes
  scale_size(breaks = c(1,2,3), range = c(1, 3), guide = F) +
  annotate(geom = "text", x = 0.65, y = 1.5, label = "NEGROES.", family = "quicksand", colour = "#654321") +
  annotate(geom = "text", x = 0.15, y = 1.5, label = "WHITES.", family = "quicksand", colour = "#654321") 

    
## Also Jack Davison! (Just adjusted colors, location, fonts, size)
# The legend is plotted separately - it's so different from a ggplot2 legend its just easier to cowplot them together

legend <- ggplot(lab_dat, aes(x,y, fill = occupation)) +
  # For geom_point, size has a different meaning so we need stroke to set the line width
  geom_point(shape = 21, size = 10, stroke = 0.15) +
  geom_text(data = lab_dat %>% filter(x == -1.5),
            aes(label = occupation),
            nudge_x = .5, family = "quicksand", size = 1.9, colour = "#654321") +
  geom_text(data = lab_dat %>% filter(x == 1.5),
            aes(label = occupation),
            nudge_x = -.4, family = "quicksand", size = 1.9, colour = "#654321") +
  scale_fill_manual(values = c("#dc143c", "#ffd700", "#5a6796", "#d2b48c", "#ece5d5"), na.translate = F) +
  scale_color_manual(values = c("#654321", NA)) +
  theme_void() +
  theme(text = element_text(family = "quicksand"),
        legend.position = "none", aspect.ratio = .3) +
  coord_cartesian(clip = "off")

## Assemble the plot
## Original size was 22 x 28 in
ggdraw(plot = pie) +
  draw_plot(legend, 
            scale = .7)
ggsave("TT2021_week8.png", type = 'cairo', width = 6, height = 6.66, dpi = 300, units = "in", bg = "#faf0e6")

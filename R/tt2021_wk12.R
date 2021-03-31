# --------------------------------#
#### Tidy Tuesday: 2021 Week 12 ###
# --------------------------------#

# Inspired by: -------------------------------------------------------------

# Person: @ijeamaka_a
# Code: https://github.com/Ijeamakaanyene/tidytuesday/blob/master/scripts/2021_06_dubois_data.Rmd

# And:

# Person: @CharlieGallaghr
# Code: https://github.com/charlie-gallagher/tidy-tuesday/blob/master/dubois/dubois.R

# Set-up --------------------------------------------------------------------

## Load packages
library(tidyverse)
library(skimr)
library(glue)
library(scales)
library(ggsci)
library(ggtext)
library(showtext)

## Loading Google fonts (https://fonts.google.com/)
##  font-family: 'Quicksand', used for title and caption;
font_add_google("Quicksand", "quicksand")
## font-family: 'Mono', used for geom_text/game names + user numbers
font_add_google("Roboto Mono", "roboto")

## Automatically use showtext to render text
showtext_auto()

# Data --------------------------------------------------------------------

## Get data
tuesdata <- tidytuesdayR::tt_load(2021, week = 12)
games <- tuesdata$games

skim(games)

## Pick games my husband has played
funs_games <- c("Borderlands: The Pre-Sequel", "Fallout 4",
                "Dead Island Definitive Edition", "Dying Light",
                "Hotline Miami", "Goat Simulator")

## Select only the first month of each game's life
release_games <- games %>% 
  filter(gamename %in% funs_games) %>%
  filter(is.na(gain))


# Recreate DuBois Challenge 07 with this data ---------------------------------


# Set up the color palette
hotline <- "#eaafa6"
deadisl <- "#9da0b0"
goatsim <- "#c4a58f"
dyingli <- "#ecb025"
borderl <- "#d8c7b3"
fallout <- "#dc354a"

dubois_palette = c(`Hotline Miami` = hotline,
                   `Dead Island Definitive Edition` = deadisl,
                   `Goat Simulator` = goatsim,
                   `Dying Light` = dyingli,
                   `Borderlands: The Pre-Sequel` = borderl,
                   `Fallout 4` = fallout)

# Make sure the order of the games is correct (from lowest to highest peak number)
release_games$gamename <- factor(release_games$gamename,
                                 levels = c("Hotline Miami",
                                            "Dead Island Definitive Edition",
                                            "Goat Simulator",
                                            "Dying Light",
                                            "Borderlands: The Pre-Sequel",
                                            "Fallout 4"))



# How many users make up 1 full circle in the plot (wanted Fallout 4 to take up 1.5 revs)
rev_val <- 471955 / 1.5

# Create spiral data (thank you Charlie Gallagher)
games_spiral <- release_games %>% 
  group_by_all() %>% 
  summarize(
    rev = 1:ceiling(peak / rev_val)
  ) %>% group_by_all() %>% 
  summarize(
    x = c(0, peak)
  ) %>% 
  ungroup() %>% 
  mutate(
    x = case_when(
      x == 0 ~ 0,
      rev == 1 & x < rev_val ~ x,
      rev == 1 & x > rev_val ~ rev_val,
      rev == 2 & x > rev_val ~ x - rev_val
    ),
    y = case_when(
      rev == 1 & x == 0 ~ 2,
      rev == 1 & x != 0 ~ 2 - x / rev_val,
      rev == 2 & x == 0 ~ 1,
      rev == 2 & x != 0 ~ 1 - x / rev_val
    )
  )

# Charlie used some code to offset the y-values, so the lines don't overlap.
# I kind of hacked my way there by using the years from the DuBois data even
# though my data has no relationship to it. Not pretty, but it works.
year <- c(rep(c(1875, 1880, 1885, 1890, 1895), each = 2),
          rep(1900, 4))

hack_y <- games_spiral$y + (1887 - year) / 20

# This is fine, but I still need to offset each y by a little
games_spiral <- games_spiral %>% 
  mutate(
    y = hack_y
  )

# Create the legend/labels

# Create padding between game name and number of users, using an em-dash (\U2013)
text_width <- nchar(levels(games_spiral$gamename)) + c(3,4,4,5,5,6)
pad_width <- 36 - text_width

text_padded <- str_pad(levels(release_games$gamename), 
                       width = nchar(levels(games_spiral$gamename)) + pad_width, 
                       pad = "\U2013", side = "right")

# Create label data frame
game_label <- games_spiral %>% 
  filter(x == 0, rev == 1) %>% 
  mutate(
    text = paste0(text_padded, peak)
  )


# Create Plot -----------------------------------------------------

# Only run this line when you're saving it!
# Specify dpi (otherwise text will be tiny!)
op = showtext_opts(dpi = 300)

p1 <- ggplot() + 
  geom_line(data = games_spiral, aes(x = x, y = y, 
                                     group = interaction(gamename, rev),
                                     color = factor(gamename)), size = 4) + 
  geom_text(data = game_label, aes(x = rev_val - 1000, y = y, label = text),
          hjust = 1, size = 1.9, family = "roboto", color = "#654321") + 
  labs(
    title = "ASSESSED PEAK NUMBER OF GAMERS\nDURING MONTH OF RELEASE.",
    caption = "@winterstat | #TidyTuesday | Inspiration: W. E. B. Du Bois | Data: [Steam]"
  ) + 
  coord_polar() + 
  scale_y_continuous(expand = c(0,0), limits = c(-3, 2.61)) +
  scale_color_manual(values = dubois_palette) +
  guides(color = FALSE) +
  theme_void() + 
  theme(
    text = element_text(color = "#654321"),
    plot.margin = margin(5, 0, 10, 0),
    plot.background = element_rect(color = NA, fill = "#faf0e6"),
    plot.title = element_text(family = "quicksand", size = 15, hjust = 0.5),
    plot.caption = element_text(family = 'quicksand', size = 6)
  )

# Save plot
ggsave("TT_week12.png", type = 'cairo', width = 5, height = 5.55, dpi = 300, units = "in", bg = "#faf0e6")


  
  

# --------------------------------#
#### Tidy Tuesday: 2021 Week 16 ###
# --------------------------------#


# Set-up --------------------------------------------------------------------

## Load packages
library(tidyverse)
library(scales)
library(ggtext)
library(showtext)
library(ggsci)
library(circlize)

font_add_google("Bebas Neue", "bebas")

## Automatically use showtext to render text
showtext_auto()

# Data --------------------------------------------------------------------

## Get data
tuesdata <- tidytuesdayR::tt_load(2021, week = 17)

netflix <- tuesdata$netflix_titles
rm(tuesdata)

# I'm going to look at LGBTQ movies on Netflix
netflix_lgbtq <- netflix %>% filter(str_detect(listed_in, "LGBTQ Movies")) %>%
  separate(listed_in, into = c("genre1", "genre2", "genre3"),
           sep = ", ", fill = "right") %>%
  separate(date_added, into = c(NA, "added_year"), sep = ", ") %>%
  mutate(country = if_else(str_detect(title, "Wish You"), "South Korea", country), # Fill in missing country
         added_year = as.numeric(added_year)) %>%
  pivot_longer(genre1:genre3, names_to = "genre_num", values_to = "genre")

# Set up data for a chord diagram
chord_lgbtq <- netflix_lgbtq %>% 
  filter(!is.na(genre) & genre != "LGBTQ Movies") %>%
  mutate(genre = factor(genre),
         added_year = as_factor(added_year)) %>% 
  group_by(genre) %>%
  mutate(count_genre = n()) %>%
  filter(count_genre > 1) %>% 
  ungroup() %>% 
  group_by(genre, added_year) %>% 
  count() %>% 
  ungroup()



# Create Plot -----------------------------------------------------

# LGBTQ 9-color flag (Baker, 2017): 
# lavender, hot pink, red, orange, yellow, green, turquoise, indigo and violet
# palette <- c("#CE66FF", "#FF69B6", "#FF0018", "#FFA52C", "#FFFF41", "#008018", "#00C0C0", "#400098", "#86007D")
grid.col = c(`Comedies` = "#FF69B6",   
             `Cult Movies` = "#FF0018",
             `Documentaries` = "#FFA52C", 
             `Dramas` = "#FFFF41", 
             `Independent Movies` = "#008018",
             `International Movies` = "#00C0C0", 
             `Music & Musicals` = "#400098", 
             `Romantic Movies` = "#86007D",
             `2015` = "gray", `2016` = "gray", `2017` = "grey", `2018` = "grey", 
             `2019` = "grey", `2020` = "grey", `2021` = "grey")

# Only run this line when you're saving it!
# Specify dpi (otherwise text will be tiny!)
op = showtext_opts(dpi = 300)

png("TT_week17.png", width = 8, height = 8, unit = "in", type = "cairo", res = 300)

par(bg = "#000000")
circos.par(canvas.xlim=c(-1.1,1.1),canvas.ylim=c(-1.2,1.1), start.degree = 0)
chordDiagram(chord_lgbtq,
             order = c("Romantic Movies", "Music & Musicals", "International Movies", 
                       "Independent Movies", "Dramas", "Documentaries", "Cult Movies",
                       "Comedies", "2015", "2016", "2017", "2018", "2019", "2020", "2021"),
             link.sort = FALSE, 
             link.decreasing = TRUE, 
             grid.col = grid.col, 
             transparency = 0.1, 
             annotationTrack = "grid",
             preAllocateTracks = list(track.height = .1))
for(si in get.all.sector.index()) {
  xlim = get.cell.meta.data("xlim", sector.index = si, track.index = 1)
  ylim = get.cell.meta.data("ylim", sector.index = si, track.index = 1)
  circos.text(mean(xlim), ylim[1], labels = si, sector.index = si, 
              track.index = 1, 
              facing = "clockwise", 
              cex=0.8, 
              adj=c(0,.5),
              niceFacing = TRUE,
              col = "#ffffff")
}
title(main = "\nWhen were different types of LGBTQ Movies added to Netflix?",
      sub = "@winterstat | #TidyTuesday | Data: [Kaggle/Shivam Bansal]",
      family = "bebas", cex.main = 2, col.main = "#E50914",
      col.sub = "#E50914", cex.sub = 2, outer = FALSE)
text(x = 0.75, y = -1.26, "@winterstat | #TidyTuesday | Data: [Kaggle/Shivam Bansal]",
     col = "#E50914", family = "bebas", cex = .8)

circos.clear()
dev.off()



  
  

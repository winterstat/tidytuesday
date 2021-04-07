# --------------------------------#
#### Tidy Tuesday: 2021 Week 15 ###
# --------------------------------#


# Set-up --------------------------------------------------------------------

## Load packages
library(tidyverse)
library(scales)
library(ggtext)
library(packcircles)
library(showtext)

font_add_google("Open Sans", "open")
font_add_google("Roboto Condensed", "roboto")

## Automatically use showtext to render text
showtext_auto()

# Data --------------------------------------------------------------------

## Get data
tuesdata <- tidytuesdayR::tt_load(2021, week = 15)

forest <- tuesdata$forest
forest_area <- tuesdata$forest_area
brazil <- tuesdata$brazil_loss
soybean_use <- tuesdata$soybean_use
vegetable_oil <- tuesdata$vegetable_oil

# Find the top producers of vegetable oil in 2014
top_producers <- vegetable_oil %>%
  filter(!is.na(code)) %>%
  group_by(entity, year) %>%
  summarize(production_total = sum(production, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year == max(year)) %>%
  arrange(-production_total) %>%
  slice(2:10) %>%
  .$entity

top_producers <- sort(top_producers)
  
# Get percentage of oil production for each type of oil
# and select only top 9 producers in 2014
top_oil <- vegetable_oil %>% group_by(entity, year) %>%
  summarize(production_total = sum(production, na.rm = TRUE)) %>%
  left_join(vegetable_oil, .) %>%
  arrange(entity, crop_oil) %>%
  mutate(production_perc  = production / production_total,
         production_perc = if_else(is.na(production_perc), 0, production_perc),
         crop_oil = factor(crop_oil))%>%
  filter(year == 2014 & entity %in% top_producers)


# Create the packing data frame for each country
packings <- lapply(
  1:length(top_producers), 
  function(i) { 
    producer <- top_producers[i]
    x <- top_oil %>% filter(entity == producer)
    circleProgressiveLayout(x$production_perc*100) 
  })

# Combine packings
packings <- do.call(rbind, packings)

# Create the circle layout to plot with geom_polygon
dat.gg <- circleLayoutVertices(packings, npoints=50)

# Add back info about the entity and crop oil
dat.gg$entity <- rep(top_producers, each = 13 * (50+1))
dat.gg$entity <- factor(dat.gg$entity)
dat.gg$crop_oil <- rep(rep(unique(top_oil$crop_oil), each = 50+1),9)

# Create labels for most-produced oil per country
circle_labels <- cbind(top_oil, packings)
circle_labels <- circle_labels %>%
  mutate(entity = factor(entity)) %>%
  group_by(entity) %>%
  filter(production == max(production, na.rm = TRUE))
  

# Create Plot -----------------------------------------------------

palette <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99',
             '#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a',
             '#ffff99','#b15928', '#666666')

# Only run this line when you're saving it!
# Specify dpi (otherwise text will be tiny!)
op = showtext_opts(dpi = 300)

dat.gg %>%
  ggplot() +
  geom_polygon(aes(x, y, group= crop_oil, 
                   fill=as.factor(crop_oil)), 
               colour="black", alpha=.8) +
  scale_size_continuous(range=c(1,2)) +
  theme_void() +
  facet_wrap(vars(entity)) +
  geom_text(data = circle_labels, 
            aes(x, y, label=crop_oil), 
            family = "open", size = 2.6) +
  scale_fill_manual(values = palette) +
  labs(
    title = "Breakdown of Vegetable Oil Production\nin Top Nine Producing Countries in 2014",
    subtitle = "The most-produced  oil is labeled for each country.\n",
    caption = "\n@winterstat | #TidyTuesday | Data: [Our World in Data]"
  ) +
  theme(
    aspect.ratio = 1,
    panel.background = element_rect(fill = "#bdcbbc", color = "#bdcbbc"),
    plot.background = element_rect(fill = "#bdcbbc", color = "#bdcbbc"),
    legend.position="bottom",
    legend.justification = "center",
    strip.text = element_text(family = "roboto", face = "bold", 
                              color = "#242d23", size = 12),
    plot.title = element_text(family = "roboto", face = "bold", 
                              color = "#242d23", size = 20, hjust =  .5),
    plot.subtitle = element_text(family = "open", 
                                 color = "#242d23", size = 14, hjust = .5),
    plot.caption = element_text(family = "open", 
                                color = "#242d23", size = 8),
    legend.text = element_text(family = "roboto", 
                               color = "#242d23", size = 10)) +
  guides(fill = guide_legend(nrow = 3, reverse = TRUE, title = NULL))



# Save plot
ggsave("TT_week15.png", type = 'cairo', width = 6.8, height = 9, dpi = 300, units = "in", bg = "#bdcbbc")


  
  

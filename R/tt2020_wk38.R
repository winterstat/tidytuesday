#install.packages("tidytuesdayR")
#install.packages("statebins")

library(statebins) # maybe make plot with this
library(tidyverse) # data cleaning
library(ggplot2)   # plot
library(ggthemes)  # theme_map
library(viridis)   # color palette
library(gganimate) # animate the plot
library(transformr)# needed for animation of polygons

## Get the data
tuesdata <- tidytuesdayR::tt_load(2020, week = 38)

kids <- tuesdata$kids
rm(tuesdata)

## Extract spending on elementary and secondary education
kids_PK12ed <- kids %>% filter(variable == "PK12ed")

## Create basic plot
p <- ggplot(data = kids_PK12ed, aes(state = state, fill = inf_adj_perchild, group = year)) +
  geom_statebins() +
  labs(title = "Public spending on elementary and secondary\neducation (adjusted for inflation)",
       subtitle = 'Year: {closest_state}',
       caption = "@winterstat | #TidyTuesday | Data: [tidykids] by Joshua Rosenberg",
       fill = "$1000s spent\n (per child)") +
  scale_fill_viridis(option = "D", guide = guide_colourbar(barwidth = 10)) +
  theme_statebins() +
  theme(
    plot.margin = margin(5,5,10,5),
    plot.title = element_text(size = 20, lineheight = 1.2, margin=margin(5,0,5,0)),
    plot.subtitle = element_text(size = 18, vjust = 1),
    plot.caption = element_text(size = 12, hjust = 1),
    legend.title = element_text(size = 14, hjust = 1),
    legend.text = element_text(size = 12),
    legend.margin = margin(5,0,5,0)
  )

## Add some more formatting and specify the animation
p_anim <- p + 
  theme(
    legend.position = "bottom",
    legend.justification = c(1,1)
  ) +
  transition_states(year, transition_length = 5, state_length = 30) +
  enter_fade() +
  exit_fade()

## Render the animation
animate(p_anim, start_pause = 5, end_pause = 30, fps = 5,
        type = "cairo", height = 600, width =800)

## Save the animation
anim_save("TT2020_wk38.gif")

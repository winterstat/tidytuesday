#install.packages("tidytuesdayR")
#devtools::install_github("hrbrmstr/streamgraph")

library(tidyverse)
library(streamgraph)
library(htmlwidgets)

# Get the Data

tuesdata <- tidytuesdayR::tt_load(2020, week = 39)

climbers <- tuesdata$members
expeditions <- tuesdata$expeditions
peaks <- tuesdata$peaks

# Find the climbers that died
climber_died <- climbers %>%
  select(peak_id, peak_name, year, season, sex, age, citizenship, 
         expedition_role, hired, solo, oxygen_used, died, death_cause, 
         death_height_metres) %>%
  filter(died == TRUE)

# Figure out what the top 10 deadliest mountains are
climber_died %>%
  group_by(peak_name) %>%
  summarize(num_die = sum(died)) %>%
  arrange(desc(num_die)) %>%
  slice(1:10) -> mountain_died

# Get basic data for plot
climber_died %>% 
  select(peak_name, year, season, died) %>%
  filter(peak_name %in% mountain_died$peak_name[1:10]) %>%
  group_by(year, season) %>%
  summarize(num_die= sum(died)) -> plot_died

# Find deadliest year
plot_died %>% ungroup() %>%
  group_by(year) %>%
  summarize(num_die = sum(num_die)) %>%
  arrange(desc(num_die)) %>%
  slice(1:10)

# Find deadliest mountain (Everest)
mountain_died$peak_name[1]

# Find first death on top 10 deadliest mountains
climber_died %>%
  filter(peak_name %in% mountain_died$peak_name[1:10] & year == 1905) %>%
  select(peak_name)

# Get full list of years and seasons for plot
seasons <- climber_died %>% select(season) %>% unique(.)
years <- climber_died %>%
  filter(peak_name %in% mountain_died$peak_name[1:10]) %>%
  select(year) %>% range(.)
years <- seq(years[1], years[2], 1)

time <- expand.grid(years, pull(seasons, season), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
colnames(time) <- c("year", "season")

# Merge plot data with full list of years and seasons
plot_data <- left_join(time, plot_died, by = c("year", "season"))

# Replace NA with 0
plot_data %>% replace_na(list(num_die = 0)) %>%
  arrange(year, season) -> plot_data

# Create plot of deaths across seasons
# Use annotation in the plot but also to create titles and caption. 
# Just increase top and bottom margin to create space
# beyond the axes.
pp <- streamgraph(plot_data, key = "season",value = "num_die", date = "year", offset="zero", interpolate="cardinal",
                  height="500px", width="800px", interactive = FALSE, top = 80, bottom = 80) %>%
  sg_annotate(label = "Deaths across seasons on the ten deadliest peaks in the Himalayas",x = "1905-01-01", y = 35, color = "black", size = 22) %>%
  sg_annotate(label = "Nepal reopens borders in 1969",x = "1944-01-01", y = 9, color = "black", size = 12) %>%
  sg_annotate(label = "with a list of ‘permitted peaks’",x = "1944-01-01", y = 8, color = "black", size = 12) %>%
  sg_annotate(label = "Deadliest year: 2014 (30 dead)",x = "1989-01-01", y = 29, color = "black", size = 12) %>%
  sg_annotate(label = "Seasons",x = "1905-01-01", y = 30, color = "black", size = 14) %>%
  sg_annotate(label = "Winter",x = "1908-01-01", y = 28.5, color = "#bbdef0", size = 14) %>%
  sg_annotate(label = "Spring",x = "1908-01-01", y = 27, color = "#90be6d", size = 14) %>%
  sg_annotate(label = "Summer",x = "1908-01-01", y = 25.5, color = "#ff6c52", size = 14) %>%
  sg_annotate(label = "Fall",x = "1908-01-01", y = 24, color = "#f08700", size = 14) %>%
  sg_annotate(label = "1905 Kanchenjunga expedition:", x = "1905-01-01", y = 8, color = "black", size = 12) %>%
  sg_annotate(label = "Alexis Pache and 4 porters", x = "1905-01-01", y = 7, color = "black", size = 12) %>%
  sg_annotate(label = "died in an avalanche", x = "1905-01-01", y = 6, color = "black", size = 12) %>%
  sg_annotate(label = "@winterstat | #TidyTuesday | Data: {The Himalayan Database} by Alex Cookson", x = "1960-01-01", y = -6, color = "grey", size = 11) %>%
  sg_axis_x(5, "year", "%Y") %>%
  sg_fill_manual(values = c('#f08700', '#90be6d', '#ff6c52', '#bbdef0'))

# Render plot in Rstudio Viewer
pp

# Save the plot as an HTML page
saveWidget(pp, file="tt2020_wk39.html")

# Install phantom:
#webshot::install_phantomjs()

# Make a webshot in pdf (use pdf viewer to export as high quality .jpg later)
webshot::webshot("tt2020_wk39.html", "tt2020_wk39.pdf", delay = 0.2)


# --------------------------------#
#### Tidy Tuesday: 2021 Week 18 ###
# --------------------------------#


# Set-up --------------------------------------------------------------------

## Load packages
library(tidyverse)
library(scales)
library(ggtext)
library(showtext)
library(ggsci)

library(syuzhet)

font_add_google("Rozha One", "rozha")
font_add_google("Montserrat", "mont")

## Automatically use showtext to render text
showtext_auto()

# Data --------------------------------------------------------------------

## Get data
tuesdata <- tidytuesdayR::tt_load(2021, week = 18)

ceo <- tuesdata$departures
rm(tuesdata)

# Looking at sentiments expressed in the notes included 
# about the CEOs' dismissals
sentiment_analysis <- get_nrc_sentiment(ceo$notes)

# Combine with ceo data
ceo <- bind_cols(ceo, sentiment_analysis)

# Set up some nicer labels for the plot
departure_codes <- c("Death", "Illness", "Bad Performance", "Legal Violations",
                     "Retired", "New Opportunity", "Other", "Missing", "Error")

sentiments <- c("Anger", "Disgust", "Fear", "Sadness", "Surprise", "Anticipation", "Joy", "Trust")



# Look at if certain types of CEO dismassals are associated with above or below
# average sentiment scores
sentiment_plot <- ceo %>% 
  mutate(departure_code = factor(departure_code, levels = 1:9, labels = departure_codes)) %>%
  filter(departure_code %in% departure_codes[1:6]) %>%
  group_by(departure_code) %>%
  summarize_at(vars(anger:positive), ~sum(., na.rm = TRUE)) %>%
  pivot_longer(cols = anger:positive, names_to = "sentiment", values_to = "count") %>%
  filter(!(sentiment %in% c("negative", "positive"))) %>%
  group_by(departure_code) %>%
  mutate(all_sentiment = sum(count)) %>%
  ungroup() %>%
  mutate(prop_sentiment = count / all_sentiment) %>%
  group_by(sentiment) %>%
  mutate(avg_prop_sentiment = mean(prop_sentiment),
         dev_avg_prop_sentiment = prop_sentiment - avg_prop_sentiment,
         sentiment = factor(sentiment, levels = tolower(sentiments), labels = sentiments))

  
  




# Create Plot -----------------------------------------------------

# Setup color palette, some colors taken from the Inside Out movie (Pixar)
color_pal <- c('Anger' = "#B3240B",
               'Disgust' = "#74BB43",
               'Fear' = "#BC84DC",
               'Sadness' = "#0494EC",
               'Surprise' = "#52b672",
               "Anticipation" = "#fca103",
               'Joy' = "#f8df3e",
               'Trust' = "#cb48a0")

# Only run this line when you're saving it!
# Specify dpi (otherwise text will be tiny!)
op = showtext_opts(dpi = 300)


ggplot(sentiment_plot, aes(y = factor(departure_code), group = sentiment, fill = sentiment)) +
  geom_bar(aes(x = dev_avg_prop_sentiment), 
           stat = "identity", position = "dodge", show.legend = FALSE) +
  geom_vline(xintercept = 0) +
  facet_wrap(vars(sentiment), nrow = 2) +
  scale_fill_manual(values = color_pal) +
  labs(title = "How do we feel about CEO dismissals?",
       subtitle = "How the reason for a CEO's dismissal affects which sentiments are expressed\nabove or below average in messaging about the dismissal\n",
       caption = "@winterstat | #TidyTuesday | Data: [Data is Plural/Gentry et al.]",
       x = "",
       y = "") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#f9f4ec", color = "#f9f4ec"),
    plot.title.position = "plot", 
    plot.title = element_text(family = "rozha", size = 24),
    plot.subtitle = element_text(family = "mont", size = 14),
    plot.caption = element_text(family = "mont", size = 7),
    strip.text = element_text(family = "rozha", size = 16),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = "mont", size = 10),
    axis.title.y = element_blank(),
    panel.grid = element_blank(),
    panel.spacing.y = unit(0.75, "lines")
  ) +
  NULL

# Save plot
ggsave("TT_week18.png", type = 'cairo', width = 10, height = 6, dpi = 300, units = "in", bg = "#f9f4ec")

  
  

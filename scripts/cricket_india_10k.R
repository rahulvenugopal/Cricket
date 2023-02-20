# Fun number crunching for indian batsmen who scored 10k plus
# Idea triggered by CA sir's post on eJC about coefficient of variation

# author @ Rahul Venugopal on 20th February 2023

# Loading libraries
library(cricketdata)
library(tidyverse)
library(ggtext)
library(colorspace)
library(ragg)
library(cowplot)
library(png)

# https://stats.espncricinfo.com/ci/engine/stats/index.html?class=2;team=6;template=results;type=batting
# Player
sachin <- fetch_player_data(35320, "ODI") %>% 
  select(Date, Runs) %>%
  filter(!is.na(Runs))
sachin$player <- 'Sachin'

kohli <- fetch_player_data(253802, "ODI") %>% 
  select(Date, Runs) %>%
  filter(!is.na(Runs))
kohli$player <- 'Kohli'

ganguly <- fetch_player_data(28779, "ODI") %>% 
  select(Date, Runs) %>%
  filter(!is.na(Runs))
ganguly$player <- 'Ganguly'

dravid <- fetch_player_data(28114, "ODI") %>% 
  select(Date, Runs) %>%
  filter(!is.na(Runs))
dravid$player <- 'Dravid'

dhoni <- fetch_player_data(28081, "ODI") %>% 
  select(Date, Runs) %>%
  filter(!is.na(Runs))
dhoni$player <- 'Dhoni'

# Bind and get master datasheet -------------------------------------------
data <- rbind(sachin, kohli, ganguly, dravid, dhoni)

# Visualisations ----------------------------------------------------------
data$player <- as.factor(data$player)

# picked up minimlist image from below link and made a collage
# https://www.scrolldroll.com/cricketers-minimal-posters/

# color palette from https://www.schemecolor.com/author/keshavnaidu

banner <- readPNG('images/Banner.png')

pal <- rev(c("#E2B07E", "#e2b07e", "#0A8043", "#0D8F4F", "#4CB050"))

# custom function to set location of total matches text
add_sample <- function(x){
  return(c(y = 201, 
           label = length(x)))
}

co_of_var <- function(x){
  return(c(y = 0,
             label = 100*sd(x)/mean(x)))
}

# Viz
plot_runs <- data %>% 
  group_by(player) %>% 
  ggplot(aes(x = player, y = Runs)) + 
  ggdist::stat_halfeye(
    aes(color = player, alpha = 0.3,
        fill = player),
    adjust = .5, 
    width = .5, 
    .width = 0,
    justification = -.4, 
    point_color = NA) + 
  
  geom_point(
    aes(color = player, alpha = 0.5),
    fill = "white",
    shape = 21,
    stroke = .4,
    size = 1,
    position = position_jitter(seed = 1, width = .12)
  ) + 
  
  geom_point(
    aes(fill = player),
    color = "transparent",
    shape = 21,
    stroke = .4,
    size = 1,
    alpha = .3,
    position = position_jitter(seed = 1, width = .12)
  ) + 
  
  geom_boxplot(
    aes(color = player,
        fill = player, alpha = 0.1),
    width = 0.25, 
    outlier.shape = NA
  ) +
  
  stat_summary(
    geom = "text",
    fun.data = co_of_var,
    aes(label = round(..label.., 2),
        color = player),
    family = "Roboto Mono",
    fontface = "bold",
    size = 4.5,
    hjust = 1.20
  ) +
  
  stat_summary(
    geom = "text",
    fun.data = add_sample,
    aes(label = paste("Total matches =", ..label..),
        color = player),
    family = "Roboto Condensed",
    size = 5,
    hjust = 0
  ) +

  coord_flip(clip = "off") +
  
  scale_y_continuous(
    limits = c(0, 225),
    breaks = seq(0, 200, by = 25)
  ) +
  
  scale_color_manual(values = pal, guide = "none") +
  
  scale_fill_manual(values = pal, guide = "none") +
  
  labs(
    x = NULL,
    y = "Runs scored",
    title = "Descriptive stats visualisation of top 5 batsmen from India (in ODI) to cross 10,000 runs",
    subtitle = "Coefficient of variation is written next to name of the batsman. Smaller the value, greater the consistency",
    caption = "Visualisation by Rahul Venugopal (code adapted from Cédric Scherer's palmer penguins viz)"
  ) +
  
  theme_minimal(base_family = "Zilla Slab", base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(family = "Roboto Mono", size = 16),
    axis.text.y = element_text(
      color = darken(pal, .1, space = "HLS"), 
      size = 16
    ),
    axis.title.x = element_text(margin = margin(t = 10), size = 16),
    plot.title = element_text(face = "bold", size = 21),
    plot.subtitle = element_text(color = "grey40"),
    plot.title.position = "plot",
    legend.position = "none",
    plot.caption = element_text(color = "grey40",
                                    hjust = 1,
                                    lineheight = 1.2),
    plot.margin = margin(15, 15, 10, 15)
  )

ggdraw(plot_runs) +
  draw_image(banner, x = 0.415,y = 0.45,
             scale = 0.15) + 
  
  # annotate the dots (Run from a match)
  annotate(geom = "text", x = 0.6, y = 0.725,
           label = "Runs scored from a single match",
           hjust = "left",
           size = 6,
           color = "grey40") + 
  
  annotate(geom = "curve",
           x = 0.6, y = 0.725,
           xend = 0.555, yend = 0.68,
           curvature = .3,
           color = "#E2B07E",
           alpha = 0.75,
           size = 0.8,
           arrow = arrow(length = unit(2, "mm")))

ggsave("CricInfo.png", bg = "white",
       width = 14, height = 10, dpi = 600)
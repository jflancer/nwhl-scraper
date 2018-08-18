library(tidyverse)
source('~/Documents/R/RinkFunction.R')

pbp_data <- read_csv("Dropbox/nwhl/nwhl_pbp_1718.csv")

rink <- fun.draw_rink() + coord_fixed()

gamedata <- filter(pbp_data, game_id == 18507502, event_type %in% c("Shot","Goal")) %>%
  mutate(x = 50-x_coord,
         x = ifelse(period == 2, -x, x)*2,
         y = y_coord-50,
         period = as.factor(period))

rink +
  geom_point(data = gamedata,
             aes(x,y,fill = event_type), size = 4, shape = 21, color = "black", alpha = 0.7) +
  annotate("text", x = -50, y = 50, label = gamedata$away_team[1], size = 5) +
  annotate("text", x = 50, y = 50, label = gamedata$home_team[1], size = 5) +
  ggtitle(paste(gamedata$game_date[1],
                 "\n", gamedata$away_score[nrow(gamedata)],
                 " - ", gamedata$home_score[nrow(gamedata)])) +
  theme(plot.title = element_text(hjust = 0.5))

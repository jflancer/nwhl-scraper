
# NWHL GAME AND SCHEDULE SCRAPER
# CREATED BY JAKE FLANCER
# August 17, 2018

#Load Packages
library(rjson)
library(tidyverse)
library(Rcrawler)

#Season IDs
seasons <- data.frame(id = c("407749","327125","327151"), 
                      Season = c("20172018","20162017","20152016"))

#General Funcions----

#Used to convert player ids to names
convert_ids <- function(column, player_df){
  column <- player_df[match(column, player_df$id, nomatch = column),
                      c('Player')]
}

#Convert column to numeric
convert_numeric <- function(column){
  as.numeric(as.character(column))
}

#Game Scrape Functions----

#Formats roster as dataframe
roster_info <- function(roster_json = NA, game_id = NA){
  if(is.na(roster_json)) {
    roster_json <- fromJSON(file =  paste("https://www.nwhl.zone/game/get_play_by_plays?id=", game_id, sep = ''))$roster_player
  }
  
  roster_data <- lapply(roster_json, unlist)
  roster_data <- lapply(roster_data, FUN = function(x){ data.frame(t(x)) })
  roster_df <- do.call("bind_rows", roster_data)
  return(roster_df)
}

#Formats team info as dataframe
team_info <- function(team_json = NA, gameid = NA){
  if(is.na(team_json)) {
    team_json <- fromJSON(file = paste("https://www.nwhl.zone/game/get_play_by_plays?id=", game_id, sep = ''))$team_instance
  }
  
  team_data <- lapply(team_json, unlist)
  columns <- names(team_data[[1]])
  roster_df <- data.frame(matrix(unlist(team_data),
                                 byrow = T,
                                 nrow = length(team_data)))
  colnames(roster_df) <- columns
  return(roster_df)
}

#Formats game info as dataframe
game_info <- function(game_json = NA, gameid = NA){
  if(is.na(game_json)) {
    game_json <- fromJSON(file = paste("https://www.nwhl.zone/game/get_play_by_plays?id=", game_id, sep = ''))$game
  }
  game_vect <- unlist(game_json)
  game_df <- data.frame(t(game_vect))
}

#Scrapes game given id
complete_game_scrape <- function(game_id){
  
  # Gets json file
  game_url <- paste("https://www.nwhl.zone/game/get_play_by_plays?id=", game_id, sep = '')
  pbp_json <- fromJSON(file = game_url)
  
  #Gets extraneous data from other functions
  game_data <- game_info(game_json = pbp_json$game)
  team_data <- team_info(team_json = pbp_json$team_instance)
  roster_data <- roster_info(roster_json = pbp_json$roster_player)
  
  #Formats other data
  team_ids <- select(team_data, id, abbrev)
  
  players <- roster_data %>%
    select(id, first_name, last_name) %>%
    mutate(Player = paste(first_name,last_name)) %>%
    select(-first_name, -last_name)
  
  #extracts play by play
  plays <- pbp_json$plays
  
  #return na if play by play not found
  if(length(plays) == 0){ 
    print(paste("NO PLAY BY PLAY DATA AVAILABLE FOR GAMEID",game_id))
    return(NA)
    }
  
  #This essentially converts fromJSON list to a dataframe
  plays_reduced <- lapply(plays, unlist)
  play_data <- lapply(plays_reduced, FUN = function(x){ data.frame(t(x)) })
  play_uncleaned <- do.call("bind_rows", play_data)
  
  #This prepares everything
  play_prep <- play_uncleaned %>%
    #Selects relevant columns
    select(game_id, play_index, clock_time, min, sec, time_interval,  play_actions.subseason_id, created_at,
           home_team_score, away_team_score, team_id,
           play_type,play_summary.x_coord, play_summary.y_coord,
           primary_player_id,
           play_summary.loser_id, play_summary.goalie_id, 
           play_actions.away_team_goalie, play_actions.home_team_goalie) %>%
    #note: ifelse statements are to create NA column if the column wasn't found
    # common reasons:
        #shutout (no goals scored so no assists or plus/minus)
        #no penalties so no penalty data
        #no empty net goals, etc.
    mutate(play_summary.assist_1_id = ifelse(rep("play_summary.assist_1_id" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                             play_uncleaned$play_summary.assist_1_id, NA),
           play_summary.assist_2_id = ifelse(rep("play_summary.assist_2_id" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                             play_uncleaned$play_summary.assist_2_id, NA),
           play_summary.served_by_id = ifelse(rep("play_summary.served_by_id" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                              play_uncleaned$play_summary.served_by_id, NA),
           play_summary.penalty_type = ifelse(rep("play_summary.penalty_type" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                              play_uncleaned$play_summary.penalty_type, NA),
           play_summary.infraction_type = ifelse(rep("play_summary.infraction_type" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                                 play_uncleaned$play_summary.infraction_type, NA),
           play_summary.penalty_minutes = case_when(play_summary.penalty_type == "Minor" ~ 2,
                                                    play_summary.penalty_type == "Major" ~ 4,
                                                    play_summary.penalty_type == "1" ~ 2,
                                                    TRUE ~ 0),
           #PLAYER 1 is faceoff win, goal scorer/shot taker, shot blocker, penalty taker, turnover doer
           event_player_1 = primary_player_id,
           #PLAYER 2 is faceoff loss, assist1, penalty server, goalie on shots on goal
           event_player_2 = case_when(play_type == "Faceoff" ~ ifelse(!is.na(play_summary.loser_id),play_summary.loser_id, NA_character_),
                                      play_type == "Shot" ~ ifelse(!is.na(play_summary.goalie_id),play_summary.goalie_id, NA_character_),
                                      play_type == "Penalty" ~ ifelse(!is.na(play_summary.served_by_id),play_summary.served_by_id,NA_character_),
                                      play_type == "Goal" ~ ifelse(!is.na(play_summary.assist_1_id),play_summary.assist_1_id,NA_character_),
                                      TRUE ~ NA_character_
                                      ),
           #PLAYER 3 is secondary assist
           event_player_3 = play_summary.assist_2_id,
           #As of now the only detail is penalty info
           event_detail = ifelse(play_type == "Penalty",
                                 paste(play_summary.penalty_type, play_summary.infraction_type, play_summary.penalty_minutes),
                                 NA),
           #create background columns
           home_team = game_data$home_team, #these are currently ids
           away_team = game_data$away_team,
           game_date = as.character(as.POSIXct(created_at, "%Y-%m-%d")),
           game_seconds = 1200*(as.numeric(time_interval)-1) + ifelse(as.numeric(min) == 20,0,(19 - as.numeric(min))*60) + ifelse(as.numeric(sec) == 0, 0,60-as.numeric(sec)),
           #same as before, this deals with columns not found
           minus_player_1 = ifelse(rep("play_actions.minus_player_1" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                   play_uncleaned$play_actions.minus_player_1, NA),
           plus_player_1 = ifelse(rep("play_actions.plus_player_1" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                  play_uncleaned$play_actions.plus_player_1, NA),
           minus_player_2 = ifelse(rep("play_actions.minus_player_2" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                   play_uncleaned$play_actions.minus_player_2, NA),
           plus_player_2 = ifelse(rep("play_actions.plus_player_2" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                  play_uncleaned$play_actions.plus_player_2, NA),
           minus_player_3 = ifelse(rep("play_actions.minus_player_3" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                   play_uncleaned$play_actions.minus_player_3, NA),
           plus_player_3 = ifelse(rep("play_actions.plus_player_3" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                  play_uncleaned$play_actions.plus_player_3, NA),
           minus_player_4 = ifelse(rep("play_actions.minus_player_4" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                   play_uncleaned$play_actions.minus_player_4, NA),
           plus_player_4 = ifelse(rep("play_actions.plus_player_4" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                  play_uncleaned$play_actions.plus_player_4, NA),
           minus_player_5 = ifelse(rep("play_actions.minus_player_5" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                   play_uncleaned$play_actions.minus_player_5, NA),
           plus_player_5 = ifelse(rep("play_actions.plus_player_5" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                  play_uncleaned$play_actions.plus_player_5, NA),
           minus_player_6 = ifelse(rep("play_actions.minus_player_6" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                   play_uncleaned$play_actions.minus_player_6, NA),
           plus_player_6 = ifelse(rep("play_actions.plus_player_6" %in% colnames(play_uncleaned), nrow(play_uncleaned)),
                                  play_uncleaned$play_actions.plus_player_6, NA)
           )  %>%
    #Adds interval for events
    arrange(game_seconds) %>%
    mutate(event_interval = ifelse(!is.na(lag(game_seconds)), game_seconds - lag(game_seconds), 0)) %>%
    #Remove columns not needed any more
    select(-play_summary.loser_id, -play_summary.assist_1_id, -play_summary.goalie_id, -play_summary.served_by_id,
           -play_summary.assist_2_id, - play_summary.penalty_type, -play_summary.infraction_type) %>%
    #converts team and season ids to values
    left_join(team_ids, by = c("home_team"="id")) %>%
    left_join(team_ids, by = c("away_team"="id")) %>%
    left_join(team_ids, by = c("team_id"="id")) %>%
    left_join(seasons, by = c("play_actions.subseason_id" = "id")) %>%
    select(-home_team, -away_team, -team_id, -play_actions.subseason_id, -created_at, -clock_time) %>%
    #cleaning column names
    rename(home_team = abbrev.x,
           away_team = abbrev.y,
           event_team = abbrev,
           period = time_interval,
           event_type = play_type,
           x_coord = play_summary.x_coord,
           y_coord = play_summary.y_coord,
           away_goalie = play_actions.away_team_goalie,
           home_goalie = play_actions.home_team_goalie,
           penalty_length = play_summary.penalty_minutes
           ) %>%
    #putting everything in a logical order
    select(Season, game_id, game_date, home_team, away_team,
           play_index, period, min, sec, game_seconds, event_interval,
           event_type, event_detail, x_coord, y_coord, event_team,
           event_player_1, event_player_2, event_player_3,
           home_goalie, away_goalie, plus_player_1:plus_player_5,plus_player_6,
           minus_player_1:minus_player_5,minus_player_6,
           penalty_length
           )
    
  #Create Score Columns
  play_prep$home_score <- 0
  play_prep$away_score <- 0
  
  #gets running count for scores
  play_prep$home_score <- 1*(play_prep$home_team == play_prep$event_team)*(play_prep$event_type == "Goal")
  play_prep$home_score <- cumsum(play_prep$home_score)
  play_prep$away_score <- 1*(play_prep$away_team == play_prep$event_team)*(play_prep$event_type == "Goal")
  play_prep$away_score <- cumsum(play_prep$away_score)
  
  #Converts to numeric columns
  play_prep$penalty_length <- convert_numeric(play_prep$penalty_length)
  play_prep$period <- convert_numeric(play_prep$period)
  
  #Gets home skater strength----
  
  #This takes events that could impact strength
  home_state_changes <- play_prep %>% 
    filter((event_type == "Goal" & event_team == away_team) |
             (event_type == "Penalty" & event_team == home_team)) %>% 
    select(event_type,game_seconds,penalty_length) %>%
    mutate(event_type = ifelse(event_type == "Penalty",1,2),
           prev.event = lag(event_type),
           prev.time = lag(game_seconds),
           prev.length = lag(penalty_length))
  
  #Ok this determines if a player should be added or subtracted from the home team count
  home_pen_mat <- apply(home_state_changes,
                        1,
                         FUN = function(x) {
      #Creates a -1 for duration of penalty and 0s surrounding it
      if(x[1] == 1 & x[2]+x[3]*60 < (max(play_prep$period)*1200-1)){
          c( rep( 0, length( 0:x[2] ) + 1),
          rep( -1, x[3]*60),
          rep(0, length((x[2]+x[3]*60 + 1):(max(play_prep$period)*1200-1))) 
          )
        #Creates a -1 for duration of penalty and 0s before (for end of game penalties)
        } else if(x[1] == 1 & x[2]+x[3]*60 >= (max(play_prep$period)*1200-1)) {
          c( rep( 0, length( 0:x[2] ) + 1),
             rep(-1, max(play_prep$period)*1200-1-x[2] )
          )
        #Creates a +1 from time power play goal is scored to end of penalty to handle skater coming back on
        } else if( x[1] == 2 & (x[2] %in% ifelse(!is.na(x[5]) & !is.na(x[6]) & x[2] != x[5], x[5]:(x[5]+x[6]*60),-1 )) ) {
          c( rep( 0, length( 0:(x[2]) ) + 1),
            rep( 1, length( (x[2]+1):(x[6]*60-(x[2]-x[5])))),
            rep(0, length((x[6]*60-(x[2]-x[5])):(max(play_prep$period)*1200-1)))
          )
        # Creates all zeros if event doesnt effect strength
        } else {
          rep(0, length(0:(max(play_prep$period)*1200)))
        }
      })
  
  #creates vector for skaters
  home_skaters <- 5 + apply(home_pen_mat, 1, sum)
  
  #Gets away skater strength----
  
  #**See above comments for explanation
  
  away_state_changes <- play_prep %>% 
    filter((event_type == "Goal" & event_team == home_team) |
             (event_type == "Penalty" & event_team == away_team)) %>% 
    select(event_type,game_seconds,penalty_length) %>%
    mutate(event_type = ifelse(event_type == "Penalty",1,2),
           prev.event = lag(event_type),
           prev.time = lag(game_seconds),
           prev.length = lag(penalty_length))
  
  away_pen_mat <- apply(away_state_changes,
                        1,
                        FUN = function(x) {
                          if(x[1] == 1 & x[2]+x[3]*60 < (max(play_prep$period)*1200-1)){
                            c( rep( 0, length( 0:x[2] ) + 1),
                               rep( -1, x[3]*60),
                               rep(0, length((x[2]+x[3]*60 + 1):(max(play_prep$period)*1200-1))) 
                            )
                          } else if(x[1] == 1 & x[2]+x[3]*60 >= (max(play_prep$period)*1200-1)) {
                            c( rep( 0, length( 0:x[2] ) + 1),
                               rep(-1, max(play_prep$period)*1200-1-x[2] )
                            )
                          } else if( x[1] == 2 & x[2] %in% ifelse(!is.na(x[5]) & !is.na(x[6]) & x[2] != x[5], x[5]:(x[5]+x[6]*60),-1 ) ) {
                            c( rep( 0, length( 0:(x[2]) )+1),
                               rep( 1, length( (x[2]+1):(x[6]*60-(x[2]-x[5])))),
                               rep(0, length((x[6]*60-(x[2]-x[5])+1):(max(play_prep$period)*1200-1)))
                            )
                          } else {
                            rep(0, length(0:(max(play_prep$period)*1200)))
                          }
                        })
  
  away_skaters <- 5 + apply(away_pen_mat, 1, sum)
  
  #----
  
  #Adds skater strength to pbp data
  play_df <- play_prep %>%
    left_join(data.frame(game_seconds = 0:(max(play_prep$period)*1200),
              home_skaters = home_skaters,
              away_skaters = away_skaters))

  #deals with extra skater for pulled goalie
  play_df$home_skaters <- ifelse(play_df$home_goalie == "", play_df$home_skaters+1, play_df$home_skaters)
  play_df$away_skaters <- ifelse(play_df$away_goalie == "", play_df$away_skaters+1, play_df$away_skaters)
  #deals with more than 2 penalties
  play_df$home_skaters <- ifelse(play_df$home_skaters < 3,3, play_df$home_skaters)
  play_df$away_skaters <- ifelse(play_df$away_skaters < 3,3, play_df$away_skaters)
  
  #Converts player ids to player names
  play_df[,17:33] <-
    play_df[,17:33] %>% sapply(convert_ids, player_df = players)
  
  #final sorting of columns and removing unnecessary rows
  play_df <- play_df %>%
    select(-penalty_length) %>%
    select(game_id:away_team, home_score, away_score, play_index:away_skaters) %>%
    filter(event_type != '')
  
  return(play_df)
}

#Compiles scraped games into one dataframe given vector of game ids
compile_games <- function(game_ids) {
  game_list <- lapply(game_ids, complete_game_scrape)
  game_list <- game_list[which(!is.na(game_list))]
  game_data <- do.call("bind_rows", game_list)
}

#Schedule Scrape Function----

#Pass in season as 2yrs and teams as vector of team abbrev, default is all teams (ie league schedule)
schedule_scrape <- function(Season = "20172018", teams = NA){
  all_teams <- c("BOS", "BUF","CTW","MET")
  if(is.na(teams)){
    teams <- all_teams
  }
  #team ids change every season lol
  the1718ids <- 2840761:2840764
  the1617ids <- c(2150454, 2150455, 2150457, 2150459)
  the1516ids <- 2150711:2150714
  
  #Gets relevant team ids
  team_ids <- if(Season == "20152016") {
    the1516ids[match(teams,all_teams)]
  } else if(Season == "20162017") {
    the1617ids[match(teams,all_teams)]
  } else {
    the1718ids[match(teams,all_teams)]
  }
  #Gets season id
  seasonid <- as.character(seasons$id[which(seasons$Season == Season)])

  #Creates urls for each team schedule page
  team_urls <- paste("https://www.nwhl.zone/schedule/team_instance/",
                     team_ids,
                     "?subseason=",
                     seasonid,
                     sep = "")
  
  # Creates empty vector
  all_games <- c()
  #Iterates through each team
  for(i in team_urls){
    #Pulls urls for each game
    Rcrawler(i, urlregexfilter ="/game/show/", MaxDepth=1)
    #Takes out game id
    game_ids <- strsplit(INDEX$Url, split = "\\?|\\/")
    game_ids <- lapply(game_ids, function(x){x[6]})
    game_ids <- unlist(game_ids)
    #adds to vector
    all_games <- c(all_games,game_ids)
  }
  #since duplicate games
  all_games <- unique(all_games)

  # Rcrawler Creates dumb folders in working directory- this deletes them
  deleted <- dir(pattern = "nwhl.zone")
  unlink(deleted, recursive = T)
  
  return(all_games)
}

##############
#RUNNING THE SCRAPER

# Uncomment code to scrape entire season
# I'd recommend saving the game ids as that takes a little longer to run
#pbp_ids <- schedule_scrape(Season = "20172018")
pbp_ids <- read_csv("/Users/Jake/Dropbox/nwhl/nwhl_gameids1718.csv")$x
pbp_df <- compile_games(pbp_ids)

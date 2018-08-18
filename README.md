# nwhl-scraper
Scrape NWHL Schedule and Play by Play Data in R


nwhl_scraper.R 
---------------
Functions:

roster_info - scrape roster information given game id into dataframe

team_info - scrape information on teams within a given game id

game_info - scrape background information on a game given game id

complete_game_scrape - this is the main game scrape function. Given a game id this will return a dataframe of cleaned pbp data.

compile_games - this takes in a vector of game ids and returns a dataframe of all play by play files merged together

schedule_scrape - given a season and optional team vector returns all game ids for the field

nwhl_gameids####.csv
--------------------

These are csv files with the gameids for each game of the season in the year named in file

nwhl_pbp_1718.csv
-----------------

If unable to run scraper and/or don't want to, this file has all of the 1718 season pbp data loaded.

# Load necessary libraries
library(dplyr)
library(tidyverse)
library(ffscrapr)

# Create connection objects for each season
league_ids <- c(591530379404427264, 650064757319118848, 785068521058115584, 917507729030352896)
seasons <- 2020:2023

connections <- mapply(function(id, season) ff_connect(platform = "sleeper", league_id = id, season = season), league_ids, seasons, SIMPLIFY = FALSE)

# Fetch common template and player data
conn_temp <- ff_template(scoring_type = "ppr", roster_type = "1qb")
players <- sleeper_players()
position_start <- ff_starter_positions(conn_temp)

# Function to fetch and process starter data for a season
fetch_starters <- function(conn, season, weeks) {
  starters <- ff_starters(conn, week = weeks)
  starters$season <- season
  return(starters)
}

# Combine starter data for all seasons
starters <- purrr::map2_df(connections, seasons, ~fetch_starters(.x, .y, weeks = ifelse(.y == 2020, 1:17, 1:18)))

starters$player_name <- ifelse(is.na(starters$player_name), starters$player_id, starters$player_name)

print(starters)

# Load necessary libraries
library(ffscrapr)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(qs)

# Set constants
SEASONS <- 2020:2023
SLEEPER_LEAGUE_IDS <- c(591530379404427264, 650064757319118848, 785068521058115584, 917507729030352896)
ffscrapr::clear_cache()
# Function to manage IO operations
manage_io_operations <- function(conn, f, data_type, overwrite = FALSE, ...) {
  season <- conn$season
  message(sprintf('Scraping %s scores for season = %s.', data_type, conn$season))
  
  res <- f(conn, ...)
  Sys.sleep(runif(1, 1, 3))
  res
}

# Helper function to add season column
.add_season_col <- function(df, conn) {
  df |> 
    dplyr::mutate(
      season = as.integer(conn$season),
      .before = 1
    )
}

# Lookup table for cleaning user names
lookup <- c("nhosta" = "Hosta",
            "bellist" = "Tom",
            "tbellis" = "Tom",
            "pdpablo" = "Patrick",
            "zacgeoffray" = "Zac",
            "naaderbanki" = "Naad",
            "ttsao" = "Tommy",
            "elrandal" = "Randal",
            "Alanasty" = "JP",
            "JohnWickMD" = "Aviel",
            "slobonmynoblin" = "Logan",
            "asmontalvo" = "Montel",
            "cpmadden1" = "Conor",
            "patrickliou" = "Pat Liou",
            "JayPeeA" = "JP")

# Function to clean user names using lookup
.clean_user_name <- function(x) {
  if (x %in% names(lookup)) {
    return(lookup[x])
  } else {
    return(gsub('\\s.*$', '', x))
  }
}

# Function to scrape franchises
scrape_franchises <- function(conn) {
  ffscrapr::ff_franchises(conn) |> 
    .add_season_col(conn)
}

# Function to scrape schedules
scrape_schedules <- function(conn) {
  ffscrapr::ff_schedule(conn) |> 
    .add_season_col(conn)
}

# Function to scrape weekly player scores
scrape_weekly_player_scores <- function(conn) {
  max_week <- ifelse(conn$season == lubridate::year(Sys.Date()), 14, 18)
  ffscrapr::ff_starters(conn, weeks = 1:max_week) |> 
    .add_season_col(conn)
}

# Main function to scrape data
ff_data <- purrr::map2(
  SEASONS, SLEEPER_LEAGUE_IDS,
  \(season, league_id) {
    overwrite <- ifelse(season == max(SEASONS), TRUE, FALSE)
    
    conn <- ffscrapr::sleeper_connect(
      season = season,
      league_id = league_id
    )
    
    franchises <- manage_io_operations(
      conn, 
      data_type = 'franchises',
      f = scrape_franchises,
      overwrite = overwrite
    )
    
    franchises <- franchises |> 
      dplyr::mutate(
        user_name = sapply(user_name, .clean_user_name)
      )
    
    schedules <- manage_io_operations(
      conn, 
      data_type = 'schedule',
      f = scrape_schedules,
      overwrite = overwrite
    )
    
    player_scores <- manage_io_operations(
      conn, 
      data_type = 'player',
      f = scrape_weekly_player_scores,
      overwrite = overwrite
    )
    
    list(
      'franchises' = franchises,
      'schedules' = schedules,
      'player_scores' = player_scores
    )
  }
)

# Helper function to combine data frames
map_dfr_ff_data <- function(ff_data, name) {
  purrr::map_dfr(
    ff_data,
    \(.x) .x[[name]]
  ) 
}

# Combine data frames for each type
franchises <- map_dfr_ff_data(ff_data, 'franchises')
schedules <- map_dfr_ff_data(ff_data, 'schedules')
weekly_player_scores <- map_dfr_ff_data(ff_data, 'player_scores')




# Connection --------------------------------------------------------------------
conn1 = ff_connect(platform = "sleeper", league_id = 591530379404427264, season = 2020)
conn2 = ff_connect(platform = "sleeper", league_id = 650064757319118848, season = 2021)
conn3 = ff_connect(platform = "sleeper", league_id = 785068521058115584, season = 2022)
conn4 = ff_connect(platform = "sleeper", league_id = 917507729030352896, season = 2023)

conn_temp <-  ff_template(scoring_type = "ppr",roster_type = "1qb")

players <- sleeper_players()
position_start <- ff_starter_positions(conn_temp)

season_details <- list(
  list(conn = conn1, weeks = 1:17, season = 2020),
  list(conn = conn2, weeks = 1:18, season = 2021),
  list(conn = conn3, weeks = 1:18, season = 2022),
  list(conn = conn4, weeks = 1:18, season = 2023)
)


# Initialize an empty list to store starters data for each season
starters_list <- list()

# Loop over the season_details list to fetch and process starters data
for (i in seq_along(season_details)) {
  details <- season_details[[i]]
  starters_temp <- ff_starters(details$conn, week = details$weeks)
  starters_temp$season <- details$season
  starters_list[[i]] <- starters_temp
}

# Combine all starters data into a single dataframe
starters <- do.call(rbind, starters_list)

# Optionally, clean up the list of temporary dataframes if you want to free up memory
rm(starters_list)

starters$player_name <- ifelse(is.na(starters$player_name), starters$player_id, starters$player_name) 

scoring1 <- ff_scoringhistory(conn1, season = 2020)
scoring2 <- ff_scoringhistory(conn2, season = 2021)
scoring3 <- ff_scoringhistory(conn3, season = 2022)
scoring4 <- ff_scoringhistory(conn4, season = 2023)

scoring <- rbind(scoring1,scoring2,scoring3,scoring4) |>
  filter(pos != "LB" & pos != "DB" &  pos != "DL") |>
  select(season, week, sleeper_id, player_name, points, team)

starters_scoring <- starters |>
  left_join(scoring, by = c("season", "week", "player_id" = "sleeper_id", "player_name"))

starters_scoring$team.x <- ifelse(is.na(starters_scoring$team.x), starters_scoring$team.y, starters_scoring$team.x) 

starters_final <- starters_scoring |>
  select(-team.y) |>
  rename(team = team.x) |>
  select(season, week, franchise_id, franchise_name, player_id, player_name, pos, team, points, starter_status)

rm(scoring1,scoring2,scoring3,scoring4,starters_scoring)


# Calculate weekly projected scores
weekly_projected_scores <- starters_final |> 
  dplyr::filter(starter_status == 'starter') |> 
  dplyr::group_by(
    season,
    week,
    franchise_id
  ) |> 
  dplyr::ungroup()

# Calculate weekly team scores
weekly_team_scores <- schedules |> 
  dplyr::select(
    season,
    week,
    franchise_id,
    user_score = franchise_score,
    opponent_id,
    opponent_score,
    result
  ) |> 
  dplyr::inner_join(
    franchises |> 
      dplyr::select(
        season,
        franchise_id,
        user_name
      ),
    by = dplyr::join_by(season, franchise_id)
  ) |> 
  dplyr::inner_join(
    franchises |> 
      dplyr::select(
        season,
        opponent_id = franchise_id,
        opponent_user_name = user_name
      ),
    by = dplyr::join_by(season, opponent_id)
  ) 
  dplyr
  
weekly_team_scores

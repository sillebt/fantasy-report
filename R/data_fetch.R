# =============================================================================
# Data Fetching Functions
# =============================================================================
# Functions for connecting to Sleeper API and fetching fantasy football data.
# Handles starters, franchises, schedules, scoring history, and transactions.
# =============================================================================

# -----------------------------------------------------------------------------
# Required Libraries
# -----------------------------------------------------------------------------

library(ffscrapr)
library(sleeperapi)
library(dplyr)
library(purrr)
library(tidyr)

# Load configuration
source("R/config.R")

# -----------------------------------------------------------------------------
# Connection Management
# -----------------------------------------------------------------------------

#' Create connection object for a specific season
#' @param season Integer. The season year
#' @return ff_connect object for API calls
create_connection <- function(season) {
  league_id <- get_league_id(season)
  ff_connect(
    platform  = "sleeper",
    league_id = as.numeric(league_id),
    season    = season
  )
}

#' Create connections for all active seasons
#' @param seasons Integer vector. Seasons to connect to (default: SEASONS from config)
#' @return Named list of ff_connect objects
create_all_connections <- function(seasons = SEASONS) {
  connections <- lapply(seasons, create_connection)
  names(connections) <- as.character(seasons)
  connections
}

# -----------------------------------------------------------------------------
# Starters Data
# -----------------------------------------------------------------------------

#' Fetch starter data for a single season
#' @param conn ff_connect object
#' @param season Integer. The season year
#' @return Data frame of starters with season column
fetch_starters_single <- function(conn, season) {
  weeks <- get_weeks_for_season(season)
  starters <- ff_starters(conn, week = weeks)
  starters$season <- season

  # Fill missing player names with player_id
  starters$player_name <- ifelse(
    is.na(starters$player_name),
    starters$player_id,
    starters$player_name
  )

  starters
}

#' Fetch starters for all seasons
#' @param connections Named list of ff_connect objects
#' @return Combined data frame of all starters
fetch_all_starters <- function(connections) {
  starters_list <- purrr::imap(connections, function(conn, season) {
    message(sprintf("Fetching starters for %s...", season))
    fetch_starters_single(conn, as.integer(season))
  })

  do.call(rbind, starters_list)
}

# -----------------------------------------------------------------------------
# Franchise Data
# -----------------------------------------------------------------------------

#' Fetch franchise data for a single season
#' @param conn ff_connect object
#' @param season Integer. The season year
#' @return Data frame of franchises with season column
fetch_franchises_single <- function(conn, season) {
  franchises <- ff_franchises(conn)
  franchises$season <- season
  franchises
}

#' Fetch franchises for all seasons
#' @param connections Named list of ff_connect objects
#' @return Combined data frame of all franchises
fetch_all_franchises <- function(connections) {
  franchises_list <- purrr::imap(connections, function(conn, season) {
    message(sprintf("Fetching franchises for %s...", season))
    fetch_franchises_single(conn, as.integer(season))
  })

  do.call(rbind, franchises_list)
}

#' Get cleaned franchise lookup table
#' @param franchises Data frame from fetch_all_franchises
#' @return Data frame with franchise_id, user_name, season (cleaned names)
get_franchise_lookup <- function(franchises) {
  lookup <- franchises %>%
    select(franchise_id, user_name, franchise_name, user_id, season) %>%
    distinct(user_id, season, .keep_all = TRUE)

  # Apply username cleaning
  lookup$user_name <- sapply(lookup$user_name, clean_username)

  lookup
}

# -----------------------------------------------------------------------------
# Scoring History
# -----------------------------------------------------------------------------

#' Fetch scoring history for a single season
#' @param conn ff_connect object
#' @param season Integer. The season year
#' @return Data frame of scoring with season column
fetch_scoring_single <- function(conn, season) {
  scoring <- ff_scoringhistory(conn, season = season)

  # Filter out defensive positions and select key columns
  scoring %>%
    filter(!pos %in% c("LB", "DB", "DL")) %>%
    select(season, week, sleeper_id, player_name, points, team)
}

#' Fetch scoring history for all seasons
#' @param connections Named list of ff_connect objects
#' @return Combined data frame of all scoring
fetch_all_scoring <- function(connections) {
  scoring_list <- purrr::imap(connections, function(conn, season) {
    message(sprintf("Fetching scoring history for %s...", season))
    fetch_scoring_single(conn, as.integer(season))
  })

  do.call(rbind, scoring_list)
}

# -----------------------------------------------------------------------------
# Schedule Data
# -----------------------------------------------------------------------------

#' Fetch schedule for a single season
#' @param conn ff_connect object
#' @param season Integer. The season year
#' @return Data frame of schedule with season column
fetch_schedule_single <- function(conn, season) {
  schedule <- ff_schedule(conn)
  schedule$season <- season
  schedule
}

#' Fetch schedules for all seasons
#' @param connections Named list of ff_connect objects
#' @return Combined data frame of all schedules
fetch_all_schedules <- function(connections) {
  schedules_list <- purrr::imap(connections, function(conn, season) {
    message(sprintf("Fetching schedule for %s...", season))
    fetch_schedule_single(conn, as.integer(season))
  })

  do.call(rbind, schedules_list)
}

# -----------------------------------------------------------------------------
# Roster Data
# -----------------------------------------------------------------------------

#' Fetch current rosters for a specific season
#' @param conn ff_connect object
#' @return Data frame of current rosters
fetch_rosters <- function(conn) {
  ff_rosters(conn)
}

# -----------------------------------------------------------------------------
# Transaction Data
# -----------------------------------------------------------------------------

#' Fetch trades for a single season
#' @param conn ff_connect object
#' @param season Integer. The season year
#' @return Data frame of trades with season column
fetch_trades_single <- function(conn, season) {
  weeks <- get_weeks_for_season(season)

  trades <- ff_transactions(conn, weeks = weeks) %>%
    filter(type == "trade")

  if (nrow(trades) > 0) {
    trades$season <- season

    # Clean up player names
    trades <- trades %>%
      mutate(
        player_name = if_else(is.na(player_name), player_id, player_name),
        player_name = gsub("_pick_from_franchise_[0-9]+", "", player_name),
        player_name = gsub("_", " ", player_name),
        pos = replace_na(pos, "Pick"),
        franchise_id = as.integer(franchise_id),
        trade_partner = as.integer(trade_partner)
      )
  }

  trades
}

#' Fetch trades for all seasons
#' @param connections Named list of ff_connect objects
#' @return Combined data frame of all trades
fetch_all_trades <- function(connections) {
  trades_list <- purrr::imap(connections, function(conn, season) {
    message(sprintf("Fetching trades for %s...", season))
    fetch_trades_single(conn, as.integer(season))
  })

  # Remove empty results and combine
  trades_list <- Filter(function(x) nrow(x) > 0, trades_list)
  do.call(rbind, trades_list)
}

# -----------------------------------------------------------------------------
# Draft Data
# -----------------------------------------------------------------------------

#' Fetch draft picks for all seasons
#' @return Combined data frame of all draft picks
fetch_all_drafts <- function() {
  draft_list <- lapply(names(DRAFT_IDS), function(year) {
    message(sprintf("Fetching draft for %s...", year))
    draft_id <- DRAFT_IDS[[year]]
    picks <- get_draft_picks(draft_id)
    picks$season <- as.integer(year)
    picks
  })

  do.call(rbind, draft_list)
}

# -----------------------------------------------------------------------------
# Dynasty Values (External Data)
# -----------------------------------------------------------------------------

#' Fetch dynasty player values from DynastyProcess
#' @return Data frame of player values
fetch_dynasty_values <- function() {
  dp <- dp_values("values-players.csv") %>%
    select(fp_id, player:age, ecr_1qb, value_1qb) %>%
    rename(
      dp_value = value_1qb,
      dp_ecr   = ecr_1qb
    )

  dp
}

#' Fetch dynasty pick values from DynastyProcess
#' @return Data frame of pick values
fetch_pick_values <- function() {
  dp_values("values-picks.csv") %>%
    select(draftpick = player, pick, ecr_1qb)
}

#' Fetch player ID mappings
#' @return Data frame mapping various ID systems
fetch_player_ids <- function() {
  dp_playerids() %>%
    select(fp_id = fantasypros_id, name, sleeper_id) %>%
    mutate(name = stringi::stri_trans_totitle(gsub(",", " ", name)))
}

# -----------------------------------------------------------------------------
# Combined Data Fetching
# -----------------------------------------------------------------------------

#' Fetch all core data for the fantasy report
#' @param seasons Integer vector. Seasons to fetch (default: SEASONS from config)
#' @return Named list containing all fetched data
fetch_all_data <- function(seasons = SEASONS) {
  message("Creating connections...")
  connections <- create_all_connections(seasons)

  message("\n=== Fetching Core Data ===\n")

  starters   <- fetch_all_starters(connections)
  franchises <- fetch_all_franchises(connections)
  scoring    <- fetch_all_scoring(connections)
  schedules  <- fetch_all_schedules(connections)
  trades     <- fetch_all_trades(connections)

  # Create franchise lookup
  franchise_lookup <- get_franchise_lookup(franchises)

  list(
    starters         = starters,
    franchises       = franchises,
    franchise_lookup = franchise_lookup,
    scoring          = scoring,
    schedules        = schedules,
    trades           = trades,
    connections      = connections
  )
}

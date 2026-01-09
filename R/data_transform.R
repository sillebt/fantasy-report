# =============================================================================
# Data Transformation Functions
# =============================================================================
# Functions for processing and transforming raw fantasy football data into
# analysis-ready formats: schedules, standings, head-to-head, power rankings.
# =============================================================================

# -----------------------------------------------------------------------------
# Required Libraries
# -----------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(scales)
library(stringr)

# Load configuration
source("R/config.R")

# -----------------------------------------------------------------------------
# Starters Processing
# -----------------------------------------------------------------------------

#' Join starters with scoring data
#' @param starters Data frame of starters
#' @param scoring Data frame of scoring history
#' @return Data frame with points joined to starters
join_starters_scoring <- function(starters, scoring) {
  starters_scoring <- starters %>%
    left_join(
      scoring,
      by = c("season", "week", "player_id" = "sleeper_id", "player_name")
    )

  # Coalesce team columns

  starters_scoring %>%
    mutate(team = coalesce(team.x, team.y)) %>%
    select(-team.x, -team.y) %>%
    select(
      season, week, franchise_id, franchise_name,
      player_id, player_name, pos, team, points, starter_status
    )
}

# -----------------------------------------------------------------------------
# Schedule Processing
# -----------------------------------------------------------------------------

#' Process raw schedule into game-by-game format
#' @param schedule Raw schedule from API
#' @param franchise_lookup Franchise lookup table
#' @param season The season year
#' @return Processed schedule with both team perspectives
process_schedule <- function(schedule, franchise_lookup, season) {
  # Filter franchise lookup for this season
  lookup_season <- franchise_lookup %>%
    filter(season == !!season)

  # Team 1 perspective
  schedule_1 <- schedule %>%
    select(
      week, franchise_id, franchise_score,
      opp_score = opponent_score, opp_id = opponent_id
    ) %>%
    left_join(lookup_season, by = "franchise_id") %>%
    select(week, tm_id = franchise_id, tm = user_name,
           tm_score = franchise_score, opp_score, opp_id) %>%
    left_join(lookup_season, by = c("opp_id" = "franchise_id")) %>%
    select(week, tm_id, tm, tm_score, opp_score, opp_id, opp = user_name)

  # Team 2 perspective (opponent's view)
  schedule_2 <- schedule %>%
    select(
      week, tm_id = opponent_id, tm_score = opponent_score,
      opp_score = franchise_score, franchise_id
    ) %>%
    left_join(lookup_season, by = c("tm_id" = "franchise_id")) %>%
    select(week, tm_id, tm = user_name, tm_score, opp_score, franchise_id) %>%
    left_join(lookup_season, by = c("franchise_id" = "franchise_id")) %>%
    select(week, tm_id, tm, tm_score, opp_score, opp_id = franchise_id, opp = user_name)

  # Combine both perspectives
  bind_rows(schedule_1, schedule_2) %>%
    arrange(week) %>%
    mutate(
      result  = ifelse(tm_score > opp_score, 1, 0),
      season  = season
    ) %>%
    distinct()
}

#' Build full schedule from all seasons
#' @param connections Named list of ff_connect objects
#' @param franchise_lookup Franchise lookup table
#' @return Combined schedule for all seasons
build_full_schedule <- function(connections, franchise_lookup) {
  schedules <- purrr::imap(connections, function(conn, season) {
    raw_schedule <- ff_schedule(conn)
    process_schedule(raw_schedule, franchise_lookup, as.integer(season))
  })

  full_schedule <- do.call(rbind, schedules) %>%
    arrange(season, week, tm) %>%
    mutate(win_loss = ifelse(result == 1, "W", "L"))

  # Add season type classification
  full_schedule <- full_schedule %>%
    mutate(
      season_type = case_when(
        # Regular season
        (season == 2020 & week <= 13) |
        (season %in% 2021:2023 & week <= 14) ~ "Regular",

        # Playoffs (check if team is playoff team)
        (season == 2020 & week >= 14 & tm %in% PLAYOFF_TEAMS[["2020"]]) |
        (season == 2021 & week >= 15 & tm %in% PLAYOFF_TEAMS[["2021"]]) |
        (season == 2022 & week >= 15 & tm %in% PLAYOFF_TEAMS[["2022"]]) |
        (season == 2023 & week >= 15 & tm %in% PLAYOFF_TEAMS[["2023"]]) ~ "Playoffs",

        # Consolation (everyone else in late weeks)
        TRUE ~ "Consolation"
      )
    ) %>%
    # Filter out consolation games for most analyses
    filter(season_type != "Consolation")

  full_schedule
}

# -----------------------------------------------------------------------------
# Standings Calculations
# -----------------------------------------------------------------------------

#' Calculate standings from schedule data
#' @param schedule Data frame of games
#' @param season_filter Optional season to filter (NULL for all-time)
#' @param is_regular TRUE for regular season, FALSE for playoffs
#' @param champion Optional team name to mark with trophy emoji
#' @return Data frame of standings
calculate_standings <- function(schedule, season_filter = NULL,
                                is_regular = TRUE, champion = NULL) {
  season_type_filter <- if (is_regular) "Regular" else "Playoffs"

  data <- schedule
  if (!is.null(season_filter)) {
    data <- data %>% filter(season == season_filter)
  }
  data <- data %>% filter(season_type == season_type_filter)

  # Add trophy emoji to champion
  if (!is.null(champion)) {
    data <- data %>%
      mutate(tm = ifelse(tm %in% champion, paste0(tm, " \U0001F3C6"), tm))
  }

  data %>%
    group_by(tm) %>%
    summarize(
      wins   = sum(result),
      games  = n(),
      pf     = round(mean(tm_score), 1),
      pa     = round(mean(opp_score), 1),
      .groups = "drop"
    ) %>%
    mutate(
      losses  = games - wins,
      win_per = percent(wins / games, accuracy = 0.1),
      winp    = wins / games
    ) %>%
    arrange(-winp) %>%
    select(tm, wins, losses, win_per, pf, pa)
}

# -----------------------------------------------------------------------------
# Head-to-Head Calculations
# -----------------------------------------------------------------------------

#' Calculate head-to-head records
#' @param schedule Full schedule data
#' @param season_type_filter "Regular" or "Playoffs" (default: "Regular")
#' @return Data frame of head-to-head records
calculate_head_to_head <- function(schedule, season_type_filter = "Regular") {
  schedule %>%
    filter(season_type == season_type_filter) %>%
    mutate(
      tm_score  = as.numeric(tm_score),
      opp_score = as.numeric(opp_score),
      margin    = tm_score - opp_score
    ) %>%
    group_by(tm, opp) %>%
    summarize(
      games   = n(),
      wins    = sum(result, na.rm = TRUE),
      losses  = games - wins,
      winperc = percent(wins / games, accuracy = 0.1),
      pf      = sum(tm_score, na.rm = TRUE),
      pa      = sum(opp_score, na.rm = TRUE),
      margin  = sum(margin, na.rm = TRUE),
      avg_mrg = round(margin / games, 1),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    select(tm, opp, wins, losses, winperc, pf, pa, avg_mrg)
}

# -----------------------------------------------------------------------------
# Power Rankings
# -----------------------------------------------------------------------------

#' Calculate power rankings for a season
#' @param schedule Full schedule data
#' @param season_year Season to calculate (NULL for all seasons combined)
#' @return Data frame of power rankings
calculate_power_rankings <- function(schedule, season_year = NULL) {
  data <- schedule %>%
    filter(season_type != "Consolation")

  if (!is.null(season_year)) {
    data <- data %>% filter(season == season_year)
  }

  power_rankings <- data %>%
    group_by(tm, season) %>%
    summarize(
      wins   = sum(result),
      games  = n(),
      pf     = mean(tm_score),
      max_pf = max(tm_score),
      min_pf = min(tm_score),
      .groups = "drop"
    ) %>%
    mutate(
      losses       = games - wins,
      winp         = wins / games,
      adj_winp     = winp * 200,
      adj_pf       = pf * 6,
      adj_variance = (max_pf + min_pf) / 2,
      power_rank   = round((adj_winp + adj_pf + adj_variance) / 10, 2)
    )

  # Normalize by league average
  league_average <- mean(power_rankings$power_rank)

  power_rankings %>%
    mutate(power = round(power_rank / league_average, 4)) %>%
    select(season, team = tm, power) %>%
    arrange(-power)
}

# -----------------------------------------------------------------------------
# Blowouts and Close Calls
# -----------------------------------------------------------------------------

#' Get biggest blowouts
#' @param schedule Full schedule data
#' @param n Number of results to return
#' @return Data frame of biggest margin games
get_blowouts <- function(schedule, n = 20) {
  schedule %>%
    filter(season_type != "Consolation") %>%
    mutate(margin = tm_score - opp_score) %>%
    arrange(-margin) %>%
    head(n) %>%
    select(
      season, week,
      victor = tm, loser = opp,
      pf = tm_score, pa = opp_score, margin
    )
}

#' Get closest games
#' @param schedule Full schedule data
#' @param n Number of results to return
#' @return Data frame of closest margin wins
get_close_calls <- function(schedule, n = 20) {
  schedule %>%
    filter(season_type != "Consolation", tm_score > opp_score) %>%
    mutate(margin = tm_score - opp_score) %>%
    arrange(margin) %>%
    head(n) %>%
    select(
      season, week,
      victor = tm, loser = opp,
      pf = tm_score, pa = opp_score, margin
    )
}

# -----------------------------------------------------------------------------
# Median Score Analysis
# -----------------------------------------------------------------------------

#' Add median score comparisons to schedule
#' @param schedule Full schedule data
#' @return Schedule with median score metrics
add_median_analysis <- function(schedule) {
  schedule %>%
    filter(season_type == "Regular") %>%
    group_by(season, week) %>%
    mutate(median_score = median(tm_score)) %>%
    ungroup() %>%
    mutate(
      median_result = ifelse(tm_score > median_score, 1, 0),
      total_wins    = result + median_result
    )
}

#' Calculate standings with median game included
#' @param schedule Full schedule data
#' @param season_filter Optional season filter
#' @return Standings with actual and potential wins
calculate_median_standings <- function(schedule, season_filter = NULL) {
  median_schedule <- add_median_analysis(schedule)

  if (!is.null(season_filter)) {
    median_schedule <- median_schedule %>%
      filter(season == season_filter)
  }

  median_schedule %>%
    group_by(tm) %>%
    summarize(
      games    = n(),
      wins     = sum(result),
      pot_wins = sum(result) + sum(median_result),
      .groups  = "drop"
    ) %>%
    mutate(
      wp_act = percent(wins / games, accuracy = 0.1),
      wp_pot = percent(pot_wins / (games * 2), accuracy = 0.1)
    ) %>%
    arrange(-pot_wins) %>%
    select(tm, wins, pot_wins, wp_act, wp_pot)
}

# -----------------------------------------------------------------------------
# Trade Processing
# -----------------------------------------------------------------------------

#' Process trades into summary format
#' @param trades Raw trades data
#' @return Processed trade summary
process_trades <- function(trades) {
  # Summarize traded away
  traded_away <- trades %>%
    filter(type_desc == "traded_away") %>%
    select(season, timestamp, team_1 = franchise_id,
           traded_away = player_name, trade_partner) %>%
    group_by(season, timestamp, team_1, trade_partner) %>%
    summarize(
      players_traded_away = toString(traded_away),
      .groups = "drop"
    )

  # Summarize traded for
  traded_for <- trades %>%
    filter(type_desc == "traded_for") %>%
    select(season, timestamp, team_2 = franchise_id, traded_for = player_name) %>%
    group_by(season, timestamp, team_2) %>%
    summarize(
      players_traded_for = toString(traded_for),
      .groups = "drop"
    )

  # Combine
  trades_combined <- traded_away %>%
    left_join(traded_for, by = c("season", "timestamp", "team_1" = "team_2")) %>%
    mutate(timestamp = format(timestamp, "%m/%d/%y"))

  # Apply team name lookups
  trades_combined %>%
    mutate(
      franchise_id = as.character(team_1),
      trade_partner_id = as.character(trade_partner)
    ) %>%
    mutate(
      team_name = case_when(
        franchise_id %in% names(FRANCHISE_ID_LOOKUP) ~
          FRANCHISE_ID_LOOKUP[franchise_id],
        season %in% c(2020, 2021, 2022) & franchise_id == "11" ~ "Logan",
        season == 2023 & franchise_id == "11" ~ "Pat L",
        TRUE ~ franchise_id
      ),
      trade_partner_name = case_when(
        trade_partner_id %in% names(FRANCHISE_ID_LOOKUP) ~
          FRANCHISE_ID_LOOKUP[trade_partner_id],
        season %in% c(2020, 2021, 2022) & trade_partner_id == "11" ~ "Logan",
        season == 2023 & trade_partner_id == "11" ~ "Pat L",
        TRUE ~ trade_partner_id
      )
    ) %>%
    select(
      season, date = timestamp,
      team_name, trade_partner = trade_partner_name,
      players_traded_away, players_traded_for
    ) %>%
    arrange(season, date, team_name)
}

# -----------------------------------------------------------------------------
# Season Recap
# -----------------------------------------------------------------------------

#' Get season recap for a specific team and year
#' @param schedule Full schedule data
#' @param team Team name
#' @param season Season year
#' @return Data frame of team's season games
get_season_recap <- function(schedule, team, season) {
  user_data <- schedule %>%
    filter(season_type != "Consolation", season == !!season, tm == !!team) %>%
    arrange(week) %>%
    select(
      week, opponent = opp, win_loss,
      pf = tm_score, pa = opp_score, result
    ) %>%
    mutate(margin = pf - pa)

  # Calculate win streak
  user_data$streak <- ave(
    user_data$result,
    cumsum(user_data$result == 0),
    FUN = seq_along
  ) - 1

  user_data %>%
    select(week, opponent, win_loss, pf, pa, margin, streak)
}

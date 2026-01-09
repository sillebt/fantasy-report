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

  # Remove rows with NA scores (unplayed games/bracket placeholders)
  full_schedule <- full_schedule %>%
    filter(!is.na(tm_score), !is.na(opp_score))

  # Remove duplicate games (same season/week/tm/opp combination)
  full_schedule <- full_schedule %>%
    distinct(season, week, tm, opp, .keep_all = TRUE)

  # Add season type classification using dynamic week cutoffs
  full_schedule <- full_schedule %>%
    rowwise() %>%
    mutate(
      regular_week_cutoff = get_regular_season_weeks(season),
      playoff_week_start = get_playoff_week_cutoff(season),
      season_type = case_when(
        # Regular season: week <= cutoff for that season (13 for 2020, 14 for 2021+)
        week <= regular_week_cutoff ~ "Regular",

        # Playoffs: post regular season AND team made playoffs
        week >= playoff_week_start & is_playoff_team(tm, season) ~ "Playoffs",

        # Consolation (everyone else in late weeks)
        TRUE ~ "Consolation"
      )
    ) %>%
    ungroup() %>%
    select(-regular_week_cutoff, -playoff_week_start) %>%
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
      wins   = sum(result, na.rm = TRUE),
      games  = n(),
      pf     = round(mean(tm_score, na.rm = TRUE), 1),
      pa     = round(mean(opp_score, na.rm = TRUE), 1),
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
    filter(!is.na(tm_score), !is.na(opp_score)) %>%  # Remove unplayed games
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
      wins   = sum(result, na.rm = TRUE),
      games  = n(),
      pf     = mean(tm_score, na.rm = TRUE),
      max_pf = max(tm_score, na.rm = TRUE),
      min_pf = min(tm_score, na.rm = TRUE),
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

  # Normalize by league average (use na.rm to handle any remaining NAs)
  league_average <- mean(power_rankings$power_rank, na.rm = TRUE)

  power_rankings %>%
    mutate(power = round(power_rank / league_average, 4)) %>%
    select(season, team = tm, power) %>%
    arrange(-power)
}

# -----------------------------------------------------------------------------
# All-Play Record Rankings
# -----------------------------------------------------------------------------

#' Calculate all-play record for each team
#'
#' All-play record answers: "What would your record be if you played every
#' team in the league every week?" This removes schedule luck entirely.
#'
#' @param schedule Full schedule data
#' @param season_filter Optional season to filter (NULL for all seasons)
#' @return Data frame with all-play wins, losses, and win percentage
calculate_all_play_record <- function(schedule, season_filter = NULL) {
  data <- schedule %>%
    filter(season_type == "Regular") %>%
    filter(!is.na(tm_score))  # Remove any rows with NA scores

  if (!is.null(season_filter)) {
    data <- data %>% filter(season == season_filter)
  }

  # For each team-week, count how many teams they would beat
  # Get unique scores per team per week (avoid duplicates from schedule format)
  weekly_scores <- data %>%
    select(season, week, tm, tm_score) %>%
    distinct()

  # Calculate all-play using rank (more efficient and handles ties)
  all_play_weekly <- weekly_scores %>%
    group_by(season, week) %>%
    mutate(
      n_teams = n(),
      weekly_rank = rank(-tm_score, ties.method = "average"),
      ap_wins = n_teams - weekly_rank,
      ap_losses = weekly_rank - 1
    ) %>%
    ungroup()

  # Aggregate to season totals
  all_play_season <- all_play_weekly %>%
    group_by(season, tm) %>%
    summarize(
      weeks_played = n(),
      total_ap_wins = sum(ap_wins, na.rm = TRUE),
      total_ap_losses = sum(ap_losses, na.rm = TRUE),
      avg_weekly_rank = mean(weekly_rank, na.rm = TRUE),
      avg_score = mean(tm_score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      ap_games = total_ap_wins + total_ap_losses,
      ap_win_pct = total_ap_wins / ap_games,
      ap_record = paste0(round(total_ap_wins), "-", round(total_ap_losses))
    ) %>%
    arrange(season, -ap_win_pct)

  all_play_season
}

#' Generate all-play rankings table data
#' @param schedule Full schedule data
#' @param season_filter Optional season filter
#' @return Data frame formatted for display
get_all_play_rankings <- function(schedule, season_filter = NULL) {
  all_play <- calculate_all_play_record(schedule, season_filter)

  all_play %>%
    mutate(
      ap_win_pct_fmt = scales::percent(ap_win_pct, accuracy = 0.1),
      rank = row_number()
    ) %>%
    select(
      season, rank, team = tm,
      ap_wins = total_ap_wins,
      ap_losses = total_ap_losses,
      ap_pct = ap_win_pct_fmt,
      avg_score,
      avg_rank = avg_weekly_rank
    )
}

# -----------------------------------------------------------------------------
# Z-Score Composite Power Rankings
# -----------------------------------------------------------------------------

#' Calculate Z-Score based power rankings
#'
#' Uses standardized scores (z-scores) to create comparable metrics across
#' different scales. Combines scoring, all-play record, and consistency.
#'
#' Weights:
#'   - 45% Average Score (roster strength)
#'   - 35% All-Play Win % (schedule-adjusted performance)
#'   - 20% Consistency (lower std dev = better)
#'
#' @param schedule Full schedule data
#' @param season_filter Optional season to filter (NULL for all seasons)
#' @return Data frame with z-score components and final power rating
calculate_zscore_rankings <- function(schedule, season_filter = NULL) {
  data <- schedule %>%
    filter(season_type == "Regular") %>%
    filter(!is.na(tm_score))  # Remove any rows with NA scores

  if (!is.null(season_filter)) {
    data <- data %>% filter(season == season_filter)
  }

  # Get unique scores per team per week
  weekly_scores <- data %>%
    select(season, week, tm, tm_score) %>%
    distinct()

  # Calculate base stats
  base_stats <- weekly_scores %>%
    group_by(season, tm) %>%
    summarize(
      games = n(),
      avg_score = mean(tm_score, na.rm = TRUE),
      std_score = sd(tm_score, na.rm = TRUE),
      max_score = max(tm_score, na.rm = TRUE),
      min_score = min(tm_score, na.rm = TRUE),
      .groups = "drop"
    )

  # Get all-play data
  all_play <- calculate_all_play_record(schedule, season_filter) %>%
    select(season, tm, ap_win_pct)

  # Get actual wins
  actual_wins <- data %>%
    select(season, tm, result) %>%
    distinct() %>%
    group_by(season, tm) %>%
    summarize(
      actual_wins = sum(result, na.rm = TRUE),
      actual_games = n(),
      actual_win_pct = actual_wins / actual_games,
      .groups = "drop"
    )

  # Combine all metrics
  combined <- base_stats %>%
    left_join(all_play, by = c("season", "tm")) %>%
    left_join(actual_wins, by = c("season", "tm"))

  # Calculate z-scores within each season
  zscore_rankings <- combined %>%
    group_by(season) %>%
    mutate(
      # Z-scores (standardized to mean=0, sd=1)
      z_scoring = as.numeric(scale(avg_score)),
      z_all_play = as.numeric(scale(ap_win_pct)),
      z_consistency = as.numeric(scale(-std_score)),  # Negative because lower std = better

      # Weighted composite: 45% scoring, 35% all-play, 20% consistency
      z_power = (0.45 * z_scoring) + (0.35 * z_all_play) + (0.20 * z_consistency),

      # Rescale to 0-100 scale (mean=50, sd=15)
      power_rating = round(50 + (z_power * 15), 1),

      # Also provide 1.0-centered version for comparison
      power_normalized = round(1 + (z_power * 0.15), 4)
    ) %>%
    ungroup() %>%
    arrange(season, -power_rating)

  zscore_rankings
}

#' Generate z-score rankings table data
#' @param schedule Full schedule data
#' @param season_filter Optional season filter
#' @return Data frame formatted for display
get_zscore_rankings <- function(schedule, season_filter = NULL) {
  zscore <- calculate_zscore_rankings(schedule, season_filter)

  zscore %>%
    group_by(season) %>%
    mutate(rank = row_number()) %>%
    ungroup() %>%
    select(
      season, rank, team = tm,
      avg_pf = avg_score,
      consistency = std_score,
      ap_pct = ap_win_pct,
      actual_wp = actual_win_pct,
      z_score = z_scoring,
      z_allplay = z_all_play,
      z_consist = z_consistency,
      power = power_rating
    )
}

#' Get combined data for quadrant plot
#' @param schedule Full schedule data
#' @param season_filter Season to filter
#' @return Data frame with all-play and z-score data for plotting
get_power_quadrant_data <- function(schedule, season_filter) {
  all_play <- calculate_all_play_record(schedule, season_filter)
  zscore <- calculate_zscore_rankings(schedule, season_filter)

  # Combine for plotting
  quadrant_data <- all_play %>%
    select(season, tm, ap_win_pct, avg_score) %>%
    left_join(
      zscore %>% select(season, tm, z_power, power_rating, actual_win_pct, std_score),
      by = c("season", "tm")
    ) %>%
    mutate(
      # Normalize both to similar scales for plotting
      ap_pct_scaled = ap_win_pct * 100,
      power_scaled = power_rating
    )

  quadrant_data
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
    filter(!is.na(tm_score)) %>%  # Remove unplayed games
    group_by(season, week) %>%
    mutate(median_score = median(tm_score, na.rm = TRUE)) %>%
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
      wins     = sum(result, na.rm = TRUE),
      pot_wins = sum(result, na.rm = TRUE) + sum(median_result, na.rm = TRUE),
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

  # Apply team name lookups using dynamic ownership history
  trades_combined %>%
    mutate(
      franchise_id = as.character(team_1),
      trade_partner_id = as.character(trade_partner)
    ) %>%
    rowwise() %>%
    mutate(
      team_name = get_franchise_owner(franchise_id, season),
      trade_partner_name = get_franchise_owner(trade_partner_id, season)
    ) %>%
    ungroup() %>%
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

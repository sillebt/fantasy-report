# =============================================================================
# Trade Analysis Functions (v2.0)
# =============================================================================
# Complete trade scoring system with:
#   A) Proper draft pick resolution using player_id parsing
#   B) Value Over Replacement (VOR) adjustment for positional fairness
#   C) Projected values for future/unresolved draft picks
# =============================================================================

# -----------------------------------------------------------------------------
# Required Libraries
# -----------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(sleeperapi)
library(ffscrapr)

# Load configuration
source("R/config.R")

# -----------------------------------------------------------------------------
# Trade Analysis Constants
# -----------------------------------------------------------------------------

#' Trade verdict labels
TRADE_VERDICTS <- list(
  winner = "WINS",
  loser = "LOSES",
  push = "PUSH"
)

#' VOR Replacement Level - number of starters at each position
#' For 12-team league: QB12, RB24 (2 RB + flex), WR24 (2 WR + flex), TE12
VOR_REPLACEMENT_LEVEL <- list(
  QB = 12,
  RB = 24,
  WR = 24,
  TE = 12,
  K = 12,
  DEF = 12
)

#' Default pick values by round (VOR points) - used as fallback
#' Based on historical league production
DEFAULT_PICK_VALUES <- list(
  "1" = 120,  # 1st round pick average VOR

  "2" = 60,   # 2nd round
  "3" = 30,   # 3rd round
  "4" = 15,   # 4th round
  "5" = 5     # 5th round
)

#' Future year discount rate (15% per year)
FUTURE_PICK_DISCOUNT <- 0.15

# =============================================================================
# COMPONENT A: Draft Pick Resolution
# =============================================================================

#' Parse pick player_id to extract season, round, and original franchise
#' @param player_id Character like "2023_round_1_pick_from_franchise_4"
#' @return List with season, round, original_franchise
#' @examples
#' parse_pick_player_id("2023_round_1_pick_from_franchise_4")
#' # Returns: list(season=2023, round=1, original_franchise=4)
parse_pick_player_id <- function(player_id) {
  if (is.na(player_id) || player_id == "" || is.null(player_id)) {
    return(list(season = NA_integer_, round = NA_integer_, original_franchise = NA_integer_))
  }

  # Extract season (4 digits at start)
  season <- as.integer(str_extract(player_id, "^\\d{4}"))

  # Extract round number (after "round_")
  round_match <- str_extract(player_id, "round_(\\d+)")
  round <- if (!is.na(round_match)) {
    as.integer(str_extract(round_match, "\\d+"))
  } else {
    NA_integer_
  }

  # Extract original franchise (after "franchise_")
  franchise_match <- str_extract(player_id, "franchise_(\\d+)")
  original_franchise <- if (!is.na(franchise_match)) {
    as.integer(str_extract(franchise_match, "\\d+"))
  } else {
    NA_integer_
  }

  list(
    season = season,
    round = round,
    original_franchise = original_franchise
  )
}

#' Fetch draft results for a specific draft with draft_slot preserved
#' @param draft_id Character. The Sleeper draft ID
#' @return Data frame of draft picks with draft_slot for original owner tracking
fetch_draft_results <- function(draft_id) {
  picks <- get_draft_picks(draft_id)

  if (is.null(picks) || nrow(picks) == 0) {
    return(data.frame())
  }

  # sleeperapi returns: draft_slot (original owner), roster_id (who picked),
  # round, pick_no, player_id, first_name, last_name, position, team
  picks_df <- picks %>%
    mutate(
      # CRITICAL: draft_slot is the ORIGINAL owner's slot
      draft_slot = as.integer(draft_slot),
      round = as.integer(round),
      pick_no = as.integer(pick_no),
      roster_id = as.integer(roster_id),
      player_id = as.character(player_id),
      position = as.character(position),
      team = as.character(team),
      # Create full player name
      player_name = paste(first_name, last_name),
      player_name = trimws(player_name),
      player_name = if_else(player_name == "NA NA" | player_name == "",
                            NA_character_, player_name)
    ) %>%
    select(
      draft_slot,  # Original owner - KEY for matching traded picks
      roster_id,   # Who actually made the selection
      round,
      pick_no,
      player_id,
      player_name,
      position,
      team
    )

  picks_df
}

#' Fetch all draft results across all seasons
#' @return Data frame of all draft picks with season and draft_slot
fetch_all_draft_results <- function() {
  draft_list <- lapply(names(DRAFT_IDS), function(year) {
    message(sprintf("Fetching draft results for %s...", year))
    draft_id <- DRAFT_IDS[[year]]
    picks <- fetch_draft_results(draft_id)

    if (nrow(picks) > 0) {
      picks$season <- as.integer(year)
      picks$draft_id <- draft_id
    }
    picks
  })

  draft_list <- Filter(function(x) nrow(x) > 0, draft_list)
  dplyr::bind_rows(draft_list)
}

#' Create draft pick lookup table keyed by (season, round, draft_slot)
#' @param draft_results Data frame from fetch_all_draft_results()
#' @return Data frame ready for pick resolution
create_draft_pick_lookup <- function(draft_results) {
  draft_results %>%
    select(
      season,
      round,
      draft_slot,      # Original owner's slot - this is the join key
      roster_id,       # Who actually picked
      pick_no,
      player_id,
      player_name,
      position,
      team
    )
}

#' Resolve a draft pick to the actual player selected
#' @param pick_player_id Character. Raw player_id like "2023_round_1_pick_from_franchise_4"
#' @param draft_lookup Data frame from create_draft_pick_lookup()
#' @return List with resolution status and player info
resolve_pick_to_player <- function(pick_player_id, draft_lookup) {
  parsed <- parse_pick_player_id(pick_player_id)

  # Check if we could parse the pick ID
  if (is.na(parsed$season) || is.na(parsed$round)) {
    return(list(
      resolved = FALSE,
      is_future = FALSE,
      pick_player_id = pick_player_id,
      season = NA_integer_,
      round = NA_integer_,
      original_franchise = NA_integer_,
      player_name = NA_character_,
      player_id = NA_character_,
      position = NA_character_,
      reason = "Could not parse pick ID"
    ))
  }

  # If original_franchise is NA, we can't match
  if (is.na(parsed$original_franchise)) {
    return(list(
      resolved = FALSE,
      is_future = TRUE,
      pick_player_id = pick_player_id,
      season = parsed$season,
      round = parsed$round,
      original_franchise = NA_integer_,
      player_name = NA_character_,
      player_id = NA_character_,
      position = NA_character_,
      reason = "No original franchise in pick ID"
    ))
  }

  # Find matching draft pick by season, round, and DRAFT_SLOT (original owner)
  match <- draft_lookup %>%
    filter(
      season == parsed$season,
      round == parsed$round,
      draft_slot == parsed$original_franchise
    )

  if (nrow(match) == 0) {
    # Check if this draft has happened yet
    max_draft_season <- max(draft_lookup$season, na.rm = TRUE)

    if (parsed$season > max_draft_season) {
      return(list(
        resolved = FALSE,
        is_future = TRUE,
        pick_player_id = pick_player_id,
        season = parsed$season,
        round = parsed$round,
        original_franchise = parsed$original_franchise,
        player_name = NA_character_,
        player_id = NA_character_,
        position = NA_character_,
        reason = "Future pick - draft not yet occurred"
      ))
    } else {
      return(list(
        resolved = FALSE,
        is_future = FALSE,
        pick_player_id = pick_player_id,
        season = parsed$season,
        round = parsed$round,
        original_franchise = parsed$original_franchise,
        player_name = NA_character_,
        player_id = NA_character_,
        position = NA_character_,
        reason = "No matching draft pick found in results"
      ))
    }
  }

  # Return matched player
  match <- match[1, ]

  list(
    resolved = TRUE,
    is_future = FALSE,
    pick_player_id = pick_player_id,
    season = parsed$season,
    round = parsed$round,
    original_franchise = parsed$original_franchise,
    pick_no = match$pick_no,
    player_name = match$player_name,
    player_id = match$player_id,
    position = match$position,
    team = match$team,
    selected_by = match$roster_id,
    reason = "Resolved"
  )
}

# =============================================================================
# COMPONENT B: Value Over Replacement (VOR)
# =============================================================================

#' Calculate VOR baselines for each position per season
#' @param scoring_data Data frame of scoring history with sleeper_id, pos, season, points
#' @return Data frame with season, pos, baseline_points
calculate_vor_baselines <- function(scoring_data) {
  message("Calculating VOR baselines by position and season...")

  # Get total points per player per season
  player_season_totals <- scoring_data %>%
    filter(!is.na(pos) & pos %in% c("QB", "RB", "WR", "TE", "K")) %>%
    group_by(season, sleeper_id, pos) %>%
    summarize(
      total_points = sum(points, na.rm = TRUE),
      games = n(),
      .groups = "drop"
    ) %>%
    filter(games >= 6)  # Minimum games to qualify

  # Calculate replacement level (Nth player at each position)
  baselines <- player_season_totals %>%
    group_by(season, pos) %>%
    arrange(desc(total_points)) %>%
    mutate(rank = row_number()) %>%
    filter(rank == VOR_REPLACEMENT_LEVEL[[first(pos)]] |
           (is.null(VOR_REPLACEMENT_LEVEL[[first(pos)]]) & rank == 12)) %>%
    summarize(
      baseline_points = first(total_points),
      replacement_player_rank = first(rank),
      .groups = "drop"
    )

  # Fill in any missing position/season combos with reasonable defaults
  all_combos <- expand.grid(
    season = unique(scoring_data$season),
    pos = c("QB", "RB", "WR", "TE", "K"),
    stringsAsFactors = FALSE
  )

  baselines <- all_combos %>%
    left_join(baselines, by = c("season", "pos")) %>%
    mutate(
      # Default baselines if not enough data
      baseline_points = case_when(
        !is.na(baseline_points) ~ baseline_points,
        pos == "QB" ~ 250,
        pos == "RB" ~ 120,
        pos == "WR" ~ 120,
        pos == "TE" ~ 80,
        pos == "K" ~ 100,
        TRUE ~ 100
      )
    )

  baselines
}

#' Calculate VOR for a player within measurement window
#' @param player_id Character. The Sleeper player ID
#' @param trade_season Integer. Season when trade occurred
#' @param trade_week Integer. Week when trade occurred
#' @param scoring_data Data frame of scoring history
#' @param vor_baselines Data frame from calculate_vor_baselines()
#' @param measurement_seasons Integer. Number of seasons to measure
#' @return List with vor_points, raw_points, games, position
calculate_vor_value <- function(player_id, trade_season, trade_week,
                                 scoring_data, vor_baselines,
                                 measurement_seasons = TRADE_MEASUREMENT_SEASONS) {
  if (is.na(player_id) || player_id == "") {
    return(list(vor_points = 0, raw_points = 0, games = 0, position = NA, status = "No player ID"))
  }

  # Calculate measurement window
  end_season <- trade_season + measurement_seasons - 1

  # Get player's scoring in measurement window
  player_scoring <- scoring_data %>%
    filter(sleeper_id == as.character(player_id)) %>%
    filter(
      (season == trade_season & week >= trade_week) |
        (season > trade_season & season <= end_season)
    )

  if (nrow(player_scoring) == 0) {
    return(list(vor_points = 0, raw_points = 0, games = 0, position = NA, status = "No scoring data"))
  }

  # Get player's position
  player_pos <- player_scoring %>%
    filter(!is.na(pos)) %>%
    pull(pos) %>%
    first()

  if (is.na(player_pos)) {
    player_pos <- "UNK"
  }

  # Calculate raw points by season
  points_by_season <- player_scoring %>%
    group_by(season) %>%
    summarize(
      season_points = sum(points, na.rm = TRUE),
      games = n(),
      .groups = "drop"
    )

  # Calculate VOR by subtracting position baseline for each season
  vor_total <- 0
  for (i in seq_len(nrow(points_by_season))) {
    s <- points_by_season$season[i]
    pts <- points_by_season$season_points[i]

    # Get baseline for this position and season
    baseline <- vor_baselines %>%
      filter(season == s, pos == player_pos) %>%
      pull(baseline_points)

    if (length(baseline) == 0) {
      # Use default if no baseline found
      baseline <- case_when(
        player_pos == "QB" ~ 250,
        player_pos == "RB" ~ 120,
        player_pos == "WR" ~ 120,
        player_pos == "TE" ~ 80,
        TRUE ~ 100
      )
    }

    # VOR = points - (baseline * proportion of season played)
    # Approximate: if player played 10 games, use 10/17 of baseline
    games_in_season <- points_by_season$games[i]
    weeks_in_season <- if (s == 2020) 17 else 18
    baseline_adjustment <- baseline * (games_in_season / weeks_in_season)

    season_vor <- pts - baseline_adjustment
    vor_total <- vor_total + season_vor
  }

  raw_total <- sum(points_by_season$season_points)
  total_games <- sum(points_by_season$games)

  list(
    vor_points = round(vor_total, 1),
    raw_points = round(raw_total, 1),
    games = total_games,
    position = player_pos,
    status = "Calculated"
  )
}

# =============================================================================
# COMPONENT C: Future Pick Values
# =============================================================================

#' Calculate historical pick values from league draft data
#' @param draft_results Data frame from fetch_all_draft_results()
#' @param scoring_data Data frame of scoring history
#' @param vor_baselines Data frame from calculate_vor_baselines()
#' @return Data frame with round and average VOR production
calculate_historical_pick_values <- function(draft_results, scoring_data, vor_baselines) {
  message("Calculating historical pick values from league data...")

  # For each drafted player, calculate their 2-season VOR
  pick_values <- draft_results %>%
    filter(!is.na(player_id)) %>%
    rowwise() %>%
    mutate(
      # Calculate VOR starting from their draft season
      vor_result = list(calculate_vor_value(
        player_id,
        season,
        1,  # Start from week 1 of draft season
        scoring_data,
        vor_baselines,
        measurement_seasons = 2
      )),
      vor_points = vor_result$vor_points,
      position = vor_result$position
    ) %>%
    ungroup() %>%
    select(season, round, draft_slot, player_name, position, vor_points)

  # Average by round
  round_averages <- pick_values %>%
    group_by(round) %>%
    summarize(
      avg_vor = mean(vor_points, na.rm = TRUE),
      median_vor = median(vor_points, na.rm = TRUE),
      picks_sampled = n(),
      hit_rate = mean(vor_points > 50, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(round)

  round_averages
}

#' Get projected VOR value for an unresolved/future pick
#' @param season Integer. The draft season
#' @param round Integer. The round number
#' @param current_season Integer. Current season for discounting
#' @param historical_values Data frame from calculate_historical_pick_values() (optional)
#' @return Numeric. Projected VOR value
get_projected_pick_value <- function(season, round, current_season,
                                      historical_values = NULL) {
  # Base value by round
  if (!is.null(historical_values) && nrow(historical_values) > 0) {
    base_value <- historical_values %>%
      filter(round == !!round) %>%
      pull(avg_vor)

    if (length(base_value) == 0 || is.na(base_value)) {
      base_value <- DEFAULT_PICK_VALUES[[as.character(round)]] %||% 10
    }
  } else {
    base_value <- DEFAULT_PICK_VALUES[[as.character(round)]] %||% 10
  }

  # Apply discount for future years
  years_out <- max(0, season - current_season)
  discount_factor <- (1 - FUTURE_PICK_DISCOUNT) ^ years_out

  round(base_value * discount_factor, 1)
}

#' Fetch DynastyProcess pick values (alternative to historical calculation)
#' @return Data frame with pick values from DynastyProcess
fetch_dynastyprocess_pick_values <- function() {
  tryCatch({
    message("Fetching DynastyProcess pick values...")
    dp_picks <- dp_values("values-picks.csv")

    dp_picks %>%
      select(pick_name = player, value_1qb) %>%
      mutate(
        # Parse pick name like "2025 Mid 1st"
        season = as.integer(str_extract(pick_name, "^\\d{4}")),
        round = case_when(
          str_detect(pick_name, "1st") ~ 1L,
          str_detect(pick_name, "2nd") ~ 2L,
          str_detect(pick_name, "3rd") ~ 3L,
          str_detect(pick_name, "4th") ~ 4L,
          str_detect(pick_name, "5th") ~ 5L,
          TRUE ~ NA_integer_
        ),
        tier = case_when(
          str_detect(pick_name, "Early") ~ "early",
          str_detect(pick_name, "Mid") ~ "mid",
          str_detect(pick_name, "Late") ~ "late",
          TRUE ~ "mid"
        )
      ) %>%
      filter(!is.na(season), !is.na(round))
  }, error = function(e) {
    message("Could not fetch DynastyProcess pick values: ", e$message)
    data.frame()
  })
}

# =============================================================================
# TRADE PROCESSING
# =============================================================================

#' Prepare trades for scoring - preserves raw player_id for picks
#' @param trades Data frame from fetch_all_trades()
#' @param franchise_lookup Data frame from get_franchise_lookup()
#' @return Data frame with trades structured for scoring
prepare_trades_for_scoring <- function(trades, franchise_lookup) {
  if (nrow(trades) == 0) {
    return(data.frame())
  }

  # Create unique trade identifier
  trades <- trades %>%
    mutate(
      trade_id = paste(season, as.numeric(timestamp), franchise_id, trade_partner, sep = "_"),
      # Estimate week from timestamp
      week = as.integer(lubridate::week(timestamp) - 35),
      week = if_else(week < 1, 1L, week),
      week = if_else(week > 18, 18L, week),
      # CRITICAL: Preserve raw player_id for picks (don't clean it!)
      raw_player_id = player_id
    )

  # Join owner names
  franchise_lookup_int <- franchise_lookup %>%
    mutate(franchise_id = as.integer(franchise_id))

  trades_with_names <- trades %>%
    left_join(
      franchise_lookup_int %>%
        select(franchise_id, user_name, season) %>%
        rename(owner = user_name),
      by = c("franchise_id", "season")
    ) %>%
    left_join(
      franchise_lookup_int %>%
        select(franchise_id, user_name, season) %>%
        rename(partner_owner = user_name, trade_partner = franchise_id),
      by = c("trade_partner", "season")
    )

  # Separate into traded away and traded for
  traded_away <- trades_with_names %>%
    filter(type_desc == "traded_away") %>%
    select(trade_id, season, week, timestamp,
           owner, partner_owner, franchise_id, trade_partner,
           player_name, player_id, raw_player_id, pos) %>%
    mutate(side = "gave")

  traded_for <- trades_with_names %>%
    filter(type_desc == "traded_for") %>%
    select(trade_id, season, week, timestamp,
           owner, partner_owner, franchise_id, trade_partner,
           player_name, player_id, raw_player_id, pos) %>%
    mutate(side = "received")

  bind_rows(traded_away, traded_for)
}

#' Process all trades and score them with VOR and proper pick resolution
#' @param trades Data frame from fetch_all_trades()
#' @param scoring_data Data frame from fetch_all_scoring()
#' @param franchise_lookup Data frame from get_franchise_lookup()
#' @return Data frame with all trades scored
process_all_trades <- function(trades, scoring_data, franchise_lookup) {
  message("Processing trades for scoring...")

  # Fetch draft results
  draft_results <- fetch_all_draft_results()
  draft_lookup <- create_draft_pick_lookup(draft_results)

  # Calculate VOR baselines
  vor_baselines <- calculate_vor_baselines(scoring_data)

  # Calculate historical pick values
  historical_pick_values <- tryCatch({
    calculate_historical_pick_values(draft_results, scoring_data, vor_baselines)
  }, error = function(e) {
    message("Could not calculate historical pick values: ", e$message)
    data.frame()
  })

  message("Historical pick values by round:")
  if (nrow(historical_pick_values) > 0) {
    print(historical_pick_values)
  }

  # Prepare trades
  prepared_trades <- prepare_trades_for_scoring(trades, franchise_lookup)

  if (nrow(prepared_trades) == 0) {
    message("No trades to process")
    return(data.frame())
  }

  # Get unique trades (deduplicate)
  unique_trades <- prepared_trades %>%
    distinct(trade_id, season, week, timestamp, owner, partner_owner) %>%
    filter(owner < partner_owner | is.na(partner_owner)) %>%
    arrange(season, timestamp)

  message(sprintf("Found %d unique trades to score", nrow(unique_trades)))

  # Current season for pick discounting
  current_season <- max(scoring_data$season, na.rm = TRUE)

  # Score each trade
scored_trades <- lapply(seq_len(nrow(unique_trades)), function(i) {
    if (i %% 20 == 0) message(sprintf("  Scoring trade %d/%d", i, nrow(unique_trades)))

    trade_info <- unique_trades[i, ]
    trade_id <- trade_info$trade_id

    # Get all assets for this trade
    trade_assets <- prepared_trades %>%
      filter(trade_id == !!trade_id)

    # Side A (owner) - what they RECEIVED
    side_a_owner <- trade_info$owner
    side_a_assets <- trade_assets %>%
      filter(owner == side_a_owner, side == "received")

    # Side B (partner) - what they RECEIVED = what owner GAVE
    side_b_owner <- trade_info$partner_owner
    side_b_assets <- trade_assets %>%
      filter(owner == side_a_owner, side == "gave")

    # Process Side A assets
    side_a_vor <- 0
    side_a_details <- list()

    for (j in seq_len(nrow(side_a_assets))) {
      asset <- side_a_assets[j, ]
      is_pick <- asset$pos == "Pick"

      if (is_pick) {
        # Use RAW player_id which contains franchise info
        resolved <- resolve_pick_to_player(asset$raw_player_id, draft_lookup)

        if (resolved$resolved) {
          # Resolved pick - calculate VOR starting from DRAFT season, not trade season
          # This ensures we measure actual production from when the player was drafted
          draft_season <- resolved$season
          value <- calculate_vor_value(
            resolved$player_id,
            draft_season,  # Use draft season, not trade season
            1,             # Start from week 1 of draft season
            scoring_data,
            vor_baselines
          )
          side_a_vor <- side_a_vor + value$vor_points
          side_a_details[[length(side_a_details) + 1]] <- list(
            asset_name = asset$player_name,
            raw_player_id = asset$raw_player_id,
            resolved_name = resolved$player_name,
            resolved_position = resolved$position,
            is_pick = TRUE,
            is_future = FALSE,
            vor_points = value$vor_points,
            raw_points = value$raw_points,
            games = value$games
          )
        } else if (resolved$is_future) {
          # Future pick - use projected value
          proj_value <- get_projected_pick_value(
            resolved$season,
            resolved$round,
            current_season,
            historical_pick_values
          )
          side_a_vor <- side_a_vor + proj_value
          side_a_details[[length(side_a_details) + 1]] <- list(
            asset_name = asset$player_name,
            raw_player_id = asset$raw_player_id,
            resolved_name = NA,
            is_pick = TRUE,
            is_future = TRUE,
            vor_points = proj_value,
            raw_points = NA,
            games = 0,
            projected = TRUE
          )
        } else {
          # Could not resolve - use default
          parsed <- parse_pick_player_id(asset$raw_player_id)
          proj_value <- get_projected_pick_value(
            parsed$season %||% trade_info$season,
            parsed$round %||% 3,
            current_season,
            historical_pick_values
          )
          side_a_vor <- side_a_vor + proj_value
          side_a_details[[length(side_a_details) + 1]] <- list(
            asset_name = asset$player_name,
            raw_player_id = asset$raw_player_id,
            resolved_name = NA,
            is_pick = TRUE,
            is_future = TRUE,
            vor_points = proj_value,
            raw_points = NA,
            games = 0,
            projected = TRUE,
            resolution_error = resolved$reason
          )
        }
      } else {
        # Regular player - calculate VOR
        value <- calculate_vor_value(
          asset$player_id,
          trade_info$season,
          trade_info$week,
          scoring_data,
          vor_baselines
        )
        side_a_vor <- side_a_vor + value$vor_points
        side_a_details[[length(side_a_details) + 1]] <- list(
          asset_name = asset$player_name,
          player_id = asset$player_id,
          is_pick = FALSE,
          position = value$position,
          vor_points = value$vor_points,
          raw_points = value$raw_points,
          games = value$games
        )
      }
    }

    # Process Side B assets (same logic)
    side_b_vor <- 0
    side_b_details <- list()

    for (j in seq_len(nrow(side_b_assets))) {
      asset <- side_b_assets[j, ]
      is_pick <- asset$pos == "Pick"

      if (is_pick) {
        resolved <- resolve_pick_to_player(asset$raw_player_id, draft_lookup)

        if (resolved$resolved) {
          # Use DRAFT season, not trade season
          draft_season <- resolved$season
          value <- calculate_vor_value(
            resolved$player_id,
            draft_season,
            1,  # Start from week 1 of draft season
            scoring_data,
            vor_baselines
          )
          side_b_vor <- side_b_vor + value$vor_points
          side_b_details[[length(side_b_details) + 1]] <- list(
            asset_name = asset$player_name,
            raw_player_id = asset$raw_player_id,
            resolved_name = resolved$player_name,
            resolved_position = resolved$position,
            is_pick = TRUE,
            is_future = FALSE,
            vor_points = value$vor_points,
            raw_points = value$raw_points,
            games = value$games
          )
        } else if (resolved$is_future) {
          proj_value <- get_projected_pick_value(
            resolved$season,
            resolved$round,
            current_season,
            historical_pick_values
          )
          side_b_vor <- side_b_vor + proj_value
          side_b_details[[length(side_b_details) + 1]] <- list(
            asset_name = asset$player_name,
            raw_player_id = asset$raw_player_id,
            resolved_name = NA,
            is_pick = TRUE,
            is_future = TRUE,
            vor_points = proj_value,
            raw_points = NA,
            games = 0,
            projected = TRUE
          )
        } else {
          parsed <- parse_pick_player_id(asset$raw_player_id)
          proj_value <- get_projected_pick_value(
            parsed$season %||% trade_info$season,
            parsed$round %||% 3,
            current_season,
            historical_pick_values
          )
          side_b_vor <- side_b_vor + proj_value
          side_b_details[[length(side_b_details) + 1]] <- list(
            asset_name = asset$player_name,
            raw_player_id = asset$raw_player_id,
            resolved_name = NA,
            is_pick = TRUE,
            is_future = TRUE,
            vor_points = proj_value,
            raw_points = NA,
            games = 0,
            projected = TRUE,
            resolution_error = resolved$reason
          )
        }
      } else {
        value <- calculate_vor_value(
          asset$player_id,
          trade_info$season,
          trade_info$week,
          scoring_data,
          vor_baselines
        )
        side_b_vor <- side_b_vor + value$vor_points
        side_b_details[[length(side_b_details) + 1]] <- list(
          asset_name = asset$player_name,
          player_id = asset$player_id,
          is_pick = FALSE,
          position = value$position,
          vor_points = value$vor_points,
          raw_points = value$raw_points,
          games = value$games
        )
      }
    }

    # Determine verdict based on VOR
    margin <- side_a_vor - side_b_vor

    verdict <- case_when(
      margin >= TRADE_WIN_THRESHOLD ~ "A_WINS",
      margin <= -TRADE_WIN_THRESHOLD ~ "B_WINS",
      TRUE ~ "PUSH"
    )

    data.frame(
      trade_id = trade_id,
      season = trade_info$season,
      week = trade_info$week,
      timestamp = trade_info$timestamp,
      side_a_owner = side_a_owner,
      side_b_owner = side_b_owner,
      side_a_vor = round(side_a_vor, 1),
      side_b_vor = round(side_b_vor, 1),
      margin = round(abs(margin), 1),
      verdict = verdict,
      winner = case_when(
        verdict == "A_WINS" ~ side_a_owner,
        verdict == "B_WINS" ~ side_b_owner,
        TRUE ~ NA_character_
      ),
      loser = case_when(
        verdict == "A_WINS" ~ side_b_owner,
        verdict == "B_WINS" ~ side_a_owner,
        TRUE ~ NA_character_
      ),
      side_a_assets = I(list(side_a_details)),
      side_b_assets = I(list(side_b_details)),
      stringsAsFactors = FALSE
    )
  })

  result <- dplyr::bind_rows(scored_trades)
  message(sprintf("Scored %d trades using VOR methodology", nrow(result)))

  result
}

#' Get trade balance sheet by owner (using VOR)
#' @param scored_trades Data frame from process_all_trades()
#' @return Data frame with net VOR per owner
get_trade_balance_sheet <- function(scored_trades) {
  if (nrow(scored_trades) == 0) {
    return(data.frame(owner = character(), net_vor = numeric()))
  }

  wins <- scored_trades %>%
    filter(!is.na(winner)) %>%
    group_by(owner = winner) %>%
    summarize(
      vor_won = sum(margin),
      trades_won = n(),
      .groups = "drop"
    )

  losses <- scored_trades %>%
    filter(!is.na(loser)) %>%
    group_by(owner = loser) %>%
    summarize(
      vor_lost = sum(margin),
      trades_lost = n(),
      .groups = "drop"
    )

  all_owners <- unique(c(scored_trades$side_a_owner, scored_trades$side_b_owner))
  all_owners <- all_owners[!is.na(all_owners)]

  balance <- data.frame(owner = all_owners) %>%
    left_join(wins, by = "owner") %>%
    left_join(losses, by = "owner") %>%
    replace_na(list(vor_won = 0, trades_won = 0, vor_lost = 0, trades_lost = 0)) %>%
    mutate(
      net_vor = vor_won - vor_lost,
      total_trades = trades_won + trades_lost
    ) %>%
    arrange(desc(net_vor))

  pushes <- scored_trades %>%
    filter(verdict == "PUSH") %>%
    pivot_longer(cols = c(side_a_owner, side_b_owner), values_to = "owner") %>%
    count(owner, name = "trades_push")

  balance <- balance %>%
    left_join(pushes, by = "owner") %>%
    replace_na(list(trades_push = 0))

  # Rename for clarity
  balance <- balance %>%
    rename(
      net_points = net_vor,
      points_won = vor_won,
      points_lost = vor_lost
    )

  balance
}

#' Get the marquee trades
#' @param scored_trades Data frame from process_all_trades()
#' @return List with best_trade, worst_trade, blockbuster
get_marquee_trades <- function(scored_trades) {
  if (nrow(scored_trades) == 0) {
    return(list())
  }

  # Heists: Top 3 trades with largest margin (biggest winners)
  heists <- scored_trades %>%
    filter(verdict != "PUSH") %>%
    slice_max(margin, n = 3, with_ties = FALSE)

  # Blockbusters: Top 3 trades with most TOTAL VALUE exchanged (absolute VOR on both sides)
  # This captures trades where both sides gave up significant assets
  blockbusters <- scored_trades %>%
    mutate(
      # Count assets on each side
      side_a_count = sapply(side_a_assets, length),
      side_b_count = sapply(side_b_assets, length),
      total_assets = side_a_count + side_b_count,
      # Total absolute value exchanged
      total_value = abs(side_a_vor) + abs(side_b_vor)
    ) %>%
    # Blockbuster = most assets AND significant value
    # Require at least 4 total assets to qualify as blockbuster
    filter(total_assets >= 4) %>%
    slice_max(total_value, n = 3, with_ties = FALSE)

  # If fewer than 3 trades qualify with 4+ assets, fill with highest total value
  if (nrow(blockbusters) < 3) {
    remaining <- 3 - nrow(blockbusters)
    more <- scored_trades %>%
      filter(!trade_id %in% blockbusters$trade_id) %>%
      mutate(total_value = abs(side_a_vor) + abs(side_b_vor)) %>%
      slice_max(total_value, n = remaining, with_ties = FALSE)
    blockbusters <- bind_rows(blockbusters, more)
  }

  list(
    heists = heists,           # Top 3 biggest winners
    blockbusters = blockbusters  # Top 3 most value exchanged
  )
}

# =============================================================================
# MAIN ENTRY POINT
# =============================================================================

#' Run the full Trade Tribunal analysis with VOR
#' @param trades Data frame from fetch_all_trades()
#' @param scoring_data Data frame from fetch_all_scoring()
#' @param franchise_lookup Data frame from get_franchise_lookup()
#' @return List with all trade analysis results
run_trade_tribunal <- function(trades, scoring_data, franchise_lookup) {
  message("\n=== THE TRADE TRIBUNAL (VOR Edition) ===\n")
  message("Analyzing trades with Value Over Replacement methodology...\n")

  scored_trades <- process_all_trades(trades, scoring_data, franchise_lookup)

  balance_sheet <- get_trade_balance_sheet(scored_trades)

  marquee <- get_marquee_trades(scored_trades)

  message("\n=== TRIBUNAL ANALYSIS COMPLETE ===\n")

  list(
    scored_trades = scored_trades,
    balance_sheet = balance_sheet,
    marquee_trades = marquee
  )
}

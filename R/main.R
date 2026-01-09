# =============================================================================
# Fantasy Report - Main Orchestration Script
# =============================================================================
# Run this script to generate all fantasy football report outputs.
# This script coordinates data fetching, transformation, and visualization.
#
# Usage:
#   source("R/main.R")
#   generate_all_reports()           # Generate everything
#   generate_standings_only()        # Just standings tables
#   generate_h2h_only()              # Just head-to-head tables
# =============================================================================

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

# Set working directory to project root if needed
if (!file.exists("R/config.R")) {
  stop("Please run this script from the project root directory (fantasy-report/)")
}

# Load all modules
source("R/config.R")
source("R/data_fetch.R")
source("R/data_transform.R")
source("R/visualizations.R")

# Load additional required libraries
library(dplyr)
library(purrr)
library(readr)

# -----------------------------------------------------------------------------
# Ensure Output Directories Exist
# -----------------------------------------------------------------------------

ensure_output_dirs <- function() {
  dirs <- c(
    "output/history",
    "output/headtohead",
    "output/season_schedule",
    "output/rosters",
    "output/trades",
    "output/2024"
  )

  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      message(sprintf("Created directory: %s", dir))
    }
  }
}

# -----------------------------------------------------------------------------
# Report Generation Functions
# -----------------------------------------------------------------------------

#' Fetch and prepare all data
#' @return List containing all prepared data
prepare_data <- function() {
  message("\n========================================")
  message("   FETCHING DATA FROM SLEEPER API")
  message("========================================\n")

  # Create connections
  connections <- create_all_connections()

  # Fetch core data
  message("Fetching franchises...")
  franchises <- fetch_all_franchises(connections)
  franchise_lookup <- get_franchise_lookup(franchises)

  message("Fetching starters...")
  starters <- fetch_all_starters(connections)

  message("Fetching scoring history...")
  scoring <- fetch_all_scoring(connections)

  message("Building full schedule...")
  full_schedule <- build_full_schedule(connections, franchise_lookup)

  message("Fetching trades...")
  trades <- tryCatch(
    fetch_all_trades(connections),
    error = function(e) {
      message("Warning: Could not fetch trades - ", e$message)
      NULL
    }
  )

  # Process starters with scoring
  starters_final <- join_starters_scoring(starters, scoring)

  list(
    connections      = connections,
    franchises       = franchises,
    franchise_lookup = franchise_lookup,
    starters         = starters,
    starters_final   = starters_final,
    scoring          = scoring,
    full_schedule    = full_schedule,
    trades           = trades
  )
}

#' Generate all standings tables
#' @param full_schedule Full schedule data
generate_standings <- function(full_schedule) {
  message("\n--- Generating Standings Tables ---")
  generate_all_standings(full_schedule)
  message("Standings complete!")
}

#' Generate all head-to-head tables
#' @param full_schedule Full schedule data
generate_head_to_head <- function(full_schedule) {
  message("\n--- Generating Head-to-Head Tables ---")
  h2h_data <- calculate_head_to_head(full_schedule)
  save_all_h2h_tables(h2h_data)
  message("Head-to-head tables complete!")
}

#' Generate all season schedule tables
#' @param full_schedule Full schedule data
generate_schedules <- function(full_schedule) {
  message("\n--- Generating Season Schedule Tables ---")
  generate_all_season_schedules(full_schedule)
  message("Season schedules complete!")
}

#' Generate all power ranking tables (legacy method)
#' @param full_schedule Full schedule data
generate_power_rankings <- function(full_schedule) {
  message("\n--- Generating Legacy Power Rankings ---")
  generate_all_power_rankings(full_schedule)
  message("Legacy power rankings complete!")
}

#' Generate new power rankings (All-Play, Z-Score, Quadrants)
#' @param full_schedule Full schedule data
generate_new_power_rankings <- function(full_schedule) {
  message("\n--- Generating New Power Rankings ---")
  generate_all_new_power_rankings(full_schedule)
  message("New power rankings complete!")
}

#' Generate blowouts and close calls tables
#' @param full_schedule Full schedule data
generate_notable_games <- function(full_schedule) {
  message("\n--- Generating Notable Games Tables ---")

  blowouts <- get_blowouts(full_schedule)
  gt_table <- generate_notable_games_table(blowouts, "**Top-20 Blowouts**")
  gtsave(gt_table, paste0(OUTPUT_PATHS$history, "blowouts.png"))

  close_calls <- get_close_calls(full_schedule)
  gt_table <- generate_notable_games_table(close_calls, "**Top-20 Close Calls**")
  gtsave(gt_table, paste0(OUTPUT_PATHS$history, "closecalls.png"))

  message("Notable games complete!")
}

#' Generate all trade tables
#' @param trades Raw trades data
generate_trades <- function(trades) {
  if (is.null(trades) || nrow(trades) == 0) {
    message("\n--- Skipping Trade Tables (no data) ---")
    return(invisible(NULL))
  }

  message("\n--- Generating Trade Tables ---")
  trade_data <- process_trades(trades)

  # Save processed trades to CSV
  write_csv(trade_data, "trades_final.csv")

  # Generate tables for each team
  save_all_trade_tables(trade_data)
  message("Trade tables complete!")
}

#' Generate median standings analysis
#' @param full_schedule Full schedule data
generate_median_analysis <- function(full_schedule) {
  message("\n--- Generating Median Standings ---")

  # Generate for each season
  for (season in SEASONS) {
    median_standings <- calculate_median_standings(full_schedule, season)

    gt_table <- median_standings %>%
      gt() %>%
      tab_header(title = md(paste(season, "Season Standings w/ League Avg Game"))) %>%
      gt_theme_espn_custom() %>%
      cols_align("left", columns = 1) %>%
      tab_source_note(paste0("Season: ", season, " | PF/PA Represent Avg/Gm"))

    gtsave(gt_table, paste0(OUTPUT_PATHS$history, season, "_standings_median.png"))
  }

  # Combined all seasons
  all_median <- calculate_median_standings(full_schedule)

  gt_table <- all_median %>%
    gt() %>%
    tab_header(title = md("2020-2023 Season Standings w/ League Avg Game")) %>%
    gt_theme_espn_custom() %>%
    cols_align("left", columns = 1)

  gtsave(gt_table, paste0(OUTPUT_PATHS$history, "2020-23_standings_median.png"))

  message("Median analysis complete!")
}

# -----------------------------------------------------------------------------
# Main Entry Points
# -----------------------------------------------------------------------------

#' Generate all reports (full pipeline)
generate_all_reports <- function() {
  start_time <- Sys.time()

  message("\n========================================")
  message("   FANTASY REPORT GENERATION")
  message("========================================")
  message(sprintf("Started at: %s\n", start_time))

  # Ensure output directories exist
  ensure_output_dirs()

  # Fetch and prepare data
  data <- prepare_data()

  message("\n========================================")
  message("   GENERATING VISUALIZATIONS")
  message("========================================\n")

  # Generate all tables
  generate_standings(data$full_schedule)
  generate_head_to_head(data$full_schedule)
  generate_schedules(data$full_schedule)
  generate_power_rankings(data$full_schedule)
  generate_new_power_rankings(data$full_schedule)  # All-Play, Z-Score, Quadrants
  generate_notable_games(data$full_schedule)
  generate_trades(data$trades)
  generate_median_analysis(data$full_schedule)

  # Save full schedule for reference
  write_csv(data$full_schedule, "fullschedule.csv")

  end_time <- Sys.time()
  elapsed <- difftime(end_time, start_time, units = "mins")

  message("\n========================================")
  message("   COMPLETE!")
  message("========================================")
  message(sprintf("Finished at: %s", end_time))
  message(sprintf("Total time: %.1f minutes", elapsed))
  message("\nOutputs saved to: output/")

  invisible(data)
}

#' Generate only standings tables (quick run)
generate_standings_only <- function() {
  ensure_output_dirs()
  connections <- create_all_connections()
  franchise_lookup <- get_franchise_lookup(fetch_all_franchises(connections))
  full_schedule <- build_full_schedule(connections, franchise_lookup)
  generate_standings(full_schedule)
}

#' Generate only head-to-head tables
generate_h2h_only <- function() {
  ensure_output_dirs()
  connections <- create_all_connections()
  franchise_lookup <- get_franchise_lookup(fetch_all_franchises(connections))
  full_schedule <- build_full_schedule(connections, franchise_lookup)
  generate_head_to_head(full_schedule)
}

#' Generate only new power rankings (All-Play, Z-Score, Quadrants)
generate_power_only <- function() {
  ensure_output_dirs()
  connections <- create_all_connections()
  franchise_lookup <- get_franchise_lookup(fetch_all_franchises(connections))
  full_schedule <- build_full_schedule(connections, franchise_lookup)
  generate_new_power_rankings(full_schedule)
}

# -----------------------------------------------------------------------------
# Interactive Usage Message
# -----------------------------------------------------------------------------

message("\n=== Fantasy Report Generator Loaded ===")
message("Available functions:")
message("  generate_all_reports()    - Generate all tables and charts")
message("  generate_standings_only() - Generate only standings tables")
message("  generate_h2h_only()       - Generate only head-to-head tables")
message("  generate_power_only()     - Generate All-Play, Z-Score & Quadrant plots")
message("\nRun generate_all_reports() to start.")

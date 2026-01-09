# =============================================================================
# Trade Tribunal Verification Tests
# =============================================================================
# Unit tests and verification for the trade analysis functions.
# Run this script to verify the implementation before generating outputs.
#
# Usage:
#   source("R/test_trade_analysis.R")
#   run_all_tests()
# =============================================================================

# Load required modules
source("R/config.R")
source("R/data_fetch.R")
source("R/trade_analysis.R")

# -----------------------------------------------------------------------------
# Test Utilities
# -----------------------------------------------------------------------------

test_pass <- function(test_name) {
  message(sprintf("[PASS] %s", test_name))
  TRUE
}

test_fail <- function(test_name, reason = "") {
  message(sprintf("[FAIL] %s: %s", test_name, reason))
  FALSE
}

assert_equal <- function(actual, expected, test_name) {
  if (identical(actual, expected)) {
    test_pass(test_name)
  } else {
    test_fail(test_name, sprintf("Expected %s, got %s", expected, actual))
  }
}

# -----------------------------------------------------------------------------
# Unit Tests: Pick Parser
# -----------------------------------------------------------------------------

test_pick_parser <- function() {
  message("\n=== Testing Pick String Parser ===\n")
  results <- c()

  # Test 1: Standard format "2022 1st"
  result <- parse_pick_string("2022 1st")
  results <- c(results, assert_equal(result$season, 2022L, "Parse '2022 1st' - season"))
  results <- c(results, assert_equal(result$round, 1L, "Parse '2022 1st' - round"))

  # Test 2: Format with "round"
  result <- parse_pick_string("2023 round 2")
  results <- c(results, assert_equal(result$season, 2023L, "Parse '2023 round 2' - season"))
  results <- c(results, assert_equal(result$round, 2L, "Parse '2023 round 2' - round"))

  # Test 3: Third round with "rd"
  result <- parse_pick_string("2024 3rd")
  results <- c(results, assert_equal(result$season, 2024L, "Parse '2024 3rd' - season"))
  results <- c(results, assert_equal(result$round, 3L, "Parse '2024 3rd' - round"))

  # Test 4: Second round
  result <- parse_pick_string("2021 2nd")
  results <- c(results, assert_equal(result$season, 2021L, "Parse '2021 2nd' - season"))
  results <- c(results, assert_equal(result$round, 2L, "Parse '2021 2nd' - round"))

  # Test 5: Fourth round
  result <- parse_pick_string("2023 4th")
  results <- c(results, assert_equal(result$season, 2023L, "Parse '2023 4th' - season"))
  results <- c(results, assert_equal(result$round, 4L, "Parse '2023 4th' - round"))

  # Test 6: Empty string
  result <- parse_pick_string("")
  results <- c(results, assert_equal(is.na(result$season), TRUE, "Parse empty string - season is NA"))

  # Test 7: Case insensitive
  result <- parse_pick_string("2022 1ST")
  results <- c(results, assert_equal(result$round, 1L, "Parse '2022 1ST' (uppercase) - round"))

  message(sprintf("\nPick Parser Tests: %d/%d passed", sum(results), length(results)))
  all(results)
}

# -----------------------------------------------------------------------------
# Unit Tests: Draft Results Fetching
# -----------------------------------------------------------------------------

test_draft_results <- function() {
  message("\n=== Testing Draft Results Fetching ===\n")
  results <- c()

  # Test fetching 2022 draft
  message("Fetching 2022 draft results...")
  draft_2022 <- fetch_draft_results(DRAFT_IDS[["2022"]])

  # Check we got results
  if (nrow(draft_2022) > 0) {
    results <- c(results, test_pass("2022 draft returns data"))

    # Check required columns exist
    required_cols <- c("round", "pick_no", "player_id", "player_name")
    has_cols <- all(required_cols %in% names(draft_2022))
    results <- c(results, assert_equal(has_cols, TRUE, "Draft has required columns"))

    # Check round 1 has picks
    round1_picks <- sum(draft_2022$round == 1, na.rm = TRUE)
    results <- c(results, assert_equal(round1_picks >= 10, TRUE, "Round 1 has 10+ picks"))

    # Display first 5 picks for manual verification
    message("\nFirst 5 picks of 2022 draft (manual verification):")
    top_picks <- draft_2022 %>%
      filter(round == 1) %>%
      arrange(pick_no) %>%
      head(5) %>%
      select(pick_no, player_name, metadata_position)
    print(top_picks)

  } else {
    results <- c(results, test_fail("2022 draft returns data", "No data returned"))
  }

  message(sprintf("\nDraft Results Tests: %d/%d passed", sum(results), length(results)))
  all(results)
}

# -----------------------------------------------------------------------------
# Unit Tests: Pick Resolution
# -----------------------------------------------------------------------------

test_pick_resolution <- function() {
  message("\n=== Testing Pick Resolution ===\n")
  results <- c()

  # Fetch draft results for testing
  message("Fetching all draft results...")
  draft_results <- fetch_all_draft_results()
  draft_lookup <- create_draft_pick_lookup(draft_results)

  message(sprintf("Loaded %d draft picks across all seasons\n", nrow(draft_lookup)))

  # Test resolving a known pick - need to identify a roster_id for testing
  # First, let's see what roster IDs exist
  message("Sample roster IDs in draft data:")
  sample_rosters <- draft_lookup %>%
    filter(season == 2022, round == 1) %>%
    select(original_roster_id, drafted_player_name) %>%
    head(5)
  print(sample_rosters)

  # Try to resolve a pick
  if (nrow(sample_rosters) > 0) {
    test_roster <- sample_rosters$original_roster_id[1]
    resolved <- resolve_pick_to_player("2022 1st", test_roster, draft_lookup)

    if (resolved$resolved) {
      results <- c(results, test_pass("Resolve '2022 1st' pick"))
      message(sprintf("  Resolved to: %s", resolved$player_name))
    } else {
      results <- c(results, test_fail("Resolve '2022 1st' pick", resolved$reason))
    }
  }

  # Test unresolvable pick (future year)
  resolved <- resolve_pick_to_player("2030 1st", "1", draft_lookup)
  results <- c(results, assert_equal(resolved$resolved, FALSE, "Future pick not resolved"))

  message(sprintf("\nPick Resolution Tests: %d/%d passed", sum(results), length(results)))
  all(results)
}

# -----------------------------------------------------------------------------
# Integration Tests
# -----------------------------------------------------------------------------

test_full_pipeline <- function() {
  message("\n=== Testing Full Pipeline ===\n")
  results <- c()

  # Fetch minimal data for testing
  message("Creating test connection for 2022-2023...")
  test_seasons <- 2022:2023
  connections <- create_all_connections(test_seasons)

  message("Fetching franchises...")
  franchises <- fetch_all_franchises(connections, parallel = FALSE)
  franchise_lookup <- get_franchise_lookup(franchises)

  message("Fetching trades...")
  trades <- fetch_all_trades(connections, parallel = FALSE)

  if (nrow(trades) > 0) {
    results <- c(results, test_pass("Trades fetched successfully"))
    message(sprintf("  Found %d trade records", nrow(trades)))

    # Check for picks in trades
    picks_in_trades <- trades %>%
      filter(pos == "Pick") %>%
      nrow()
    message(sprintf("  Draft picks in trades: %d", picks_in_trades))

  } else {
    results <- c(results, test_fail("Trades fetched successfully", "No trades found"))
  }

  message("Fetching scoring data...")
  scoring <- fetch_all_scoring(connections, parallel = FALSE)

  if (nrow(scoring) > 0) {
    results <- c(results, test_pass("Scoring data fetched successfully"))
    message(sprintf("  Found %d scoring records", nrow(scoring)))
  } else {
    results <- c(results, test_fail("Scoring data fetched", "No data"))
  }

  # Run trade processing
  if (nrow(trades) > 0 && nrow(scoring) > 0) {
    message("\nProcessing trades...")
    scored_trades <- tryCatch({
      process_all_trades(trades, scoring, franchise_lookup)
    }, error = function(e) {
      message(sprintf("Error in process_all_trades: %s", e$message))
      NULL
    })

    if (!is.null(scored_trades) && nrow(scored_trades) > 0) {
      results <- c(results, test_pass("Trades processed successfully"))
      message(sprintf("  Scored %d unique trades", nrow(scored_trades)))

      # Check verdict distribution
      message("\n  Verdict distribution:")
      verdict_counts <- table(scored_trades$verdict)
      print(verdict_counts)

      # Verify balance sheet
      balance <- get_trade_balance_sheet(scored_trades)
      if (nrow(balance) > 0) {
        results <- c(results, test_pass("Balance sheet generated"))
        message("\n  Top traders by net points:")
        print(head(balance %>% select(owner, net_points, trades_won, trades_lost), 5))
      }

    } else {
      results <- c(results, test_fail("Trades processed", "No results"))
    }
  }

  message(sprintf("\nFull Pipeline Tests: %d/%d passed", sum(results), length(results)))
  all(results)
}

# -----------------------------------------------------------------------------
# Run All Tests
# -----------------------------------------------------------------------------

run_all_tests <- function() {
  message("\n========================================")
  message("   TRADE TRIBUNAL VERIFICATION")
  message("========================================\n")

  all_passed <- TRUE

  # Run unit tests
  all_passed <- all_passed && test_pick_parser()
  all_passed <- all_passed && test_draft_results()
  all_passed <- all_passed && test_pick_resolution()

  # Run integration tests
  all_passed <- all_passed && test_full_pipeline()

  message("\n========================================")
  if (all_passed) {
    message("   ALL TESTS PASSED!")
  } else {
    message("   SOME TESTS FAILED - Review output above")
  }
  message("========================================\n")

  invisible(all_passed)
}

# Run tests when sourced
message("\n=== Trade Tribunal Test Suite Loaded ===")
message("Run run_all_tests() to execute all verification tests")

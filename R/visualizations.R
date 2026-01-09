# =============================================================================
# Visualization Functions
# =============================================================================
# Functions for creating gt tables and visualizations for fantasy football
# reports. Includes reusable themes and table generator functions.
# =============================================================================

# -----------------------------------------------------------------------------
# Required Libraries
# -----------------------------------------------------------------------------

library(gt)
library(gtExtras)
library(dplyr)
library(scales)
library(showtext)
library(sysfonts)
library(ggplot2)
library(ggrepel)

# Load configuration
source("R/config.R")

# -----------------------------------------------------------------------------
# Font Setup
# -----------------------------------------------------------------------------

#' Initialize fonts for table rendering
setup_fonts <- function() {
  sysfonts::font_add_google("Roboto", "Roboto")
  showtext::showtext_auto()
}

# -----------------------------------------------------------------------------
# GT Themes
# -----------------------------------------------------------------------------

#' ESPN-style theme for standings tables
#' @param data gt object
#' @return Styled gt object
gt_theme_espn_custom <- function(data) {
  data %>%
    gt_theme_espn() %>%
    tab_options(data_row.padding = px(3))
}

#' Schedule/results theme with borders
#' @param data gt object
#' @param ... Additional tab_options arguments
#' @return Styled gt object
gt_theme_schedule <- function(data, ...) {
  data %>%
    opt_all_caps() %>%
    opt_table_font(
      font = list(google_font("Roboto"), default_fonts()),
      weight = "normal"
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "all", color = "black", weight = px(2)
      ),
      locations = cells_body(columns = everything(), rows = everything())
    ) %>%
    tab_options(
      column_labels.background.color = PRIMARY_COLOR,
      column_labels.font.size = px(16),
      heading.border.bottom.width = px(2),
      heading.border.bottom.color = PRIMARY_COLOR,
      heading.border.lr.width = px(2),
      heading.border.lr.color = PRIMARY_COLOR,
      table_body.hlines.color = PRIMARY_COLOR,
      table.border.top.width = px(2),
      table.border.top.color = PRIMARY_COLOR,
      table.border.bottom.color = PRIMARY_COLOR,
      table.border.bottom.width = px(2),
      column_labels.border.top.width = px(2),
      column_labels.border.top.color = PRIMARY_COLOR,
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = PRIMARY_COLOR,
      row_group.font.size = px(12),
      data_row.padding = px(3),
      ...
    )
}

#' Dynasty roster theme with spanners
#' @param data gt object
#' @param season_label Label for the season spanner (e.g., "2024 Season")
#' @param ... Additional tab_options arguments
#' @return Styled gt object
gt_theme_dynasty <- function(data, season_label = "Season Rank", ...) {
  data %>%
    tab_spanner(label = season_label, columns = contains(c("fp_tier", "fp_posrank", "fp_rank"))) %>%
    tab_spanner(label = "Dynasty Value", columns = contains(c("playerprofiler", "dynastypros"))) %>%
    tab_spanner(label = "BLANK", columns = 1:5) %>%
    cols_label(
      fp_posrank = "Pos Rank",
      fp_tier = "Tier",
      fp_rank = "Rank",
      playerprofiler = "PlyrPro",
      dynastypros = "DynPro"
    ) %>%
    sub_missing(columns = everything(), missing_text = "---") %>%
    tab_style(
      style = list(
        cell_fill(color = PRIMARY_COLOR),
        cell_text(color = "white"),
        cell_borders(sides = "all", color = PRIMARY_COLOR, weight = px(1))
      ),
      locations = cells_column_spanners(spanners = c("BLANK", season_label, "Dynasty Value"))
    ) %>%
    tab_style(
      style = list(cell_fill(color = PRIMARY_COLOR), cell_text(color = "transparent")),
      locations = cells_column_spanners(spanners = "BLANK")
    ) %>%
    tab_style(
      style = cell_text(color = "black", weight = "bold"),
      locations = cells_body(columns = everything())
    ) %>%
    opt_all_caps() %>%
    opt_row_striping() %>%
    tab_options(
      column_labels.background.color = PRIMARY_COLOR,
      column_labels.font.size = px(14),
      table_body.hlines.color = "transparent",
      table.border.top.width = px(2),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(2),
      column_labels.border.top.width = px(2),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "transparent",
      row_group.font.size = px(14),
      row.striping.background_color = STRIPE_COLOR,
      data_row.padding = px(4),
      ...
    ) %>%
    opt_table_font(font = google_font("Roboto")) %>%
    tab_source_note(
      md("**PlyrPro:** PlayerProfiler.com  |  **DynPro:** DynastyProcess.com  |  **Season:** FantasyPros.com")
    )
}

#' Trade table theme
#' @param data gt object
#' @return Styled gt object
gt_theme_trades <- function(data) {
  data %>%
    opt_all_caps() %>%
    tab_style(
      style = cell_text(align = "center", weight = "normal", transform = "uppercase"),
      locations = cells_body(columns = c(received, traded))
    ) %>%
    tab_style(
      style = cell_text(align = "right", weight = "bold", transform = "uppercase"),
      locations = cells_body(columns = c(date, team))
    ) %>%
    tab_style(
      style = cell_borders(sides = c("top", "bottom"), color = "grey", weight = px(1.5)),
      locations = cells_body(columns = everything())
    ) %>%
    tab_style(
      style = cell_text(color = "black", align = "center", weight = "bolder",
                       decorate = "underline", transform = "uppercase"),
      locations = cells_title(c("title", "subtitle"))
    ) %>%
    tab_style(
      style = cell_text(color = "black", align = "center", weight = "bolder", transform = "uppercase"),
      locations = cells_column_labels(columns = everything())
    ) %>%
    tab_style(
      style = list(cell_fill(color = "grey"), cell_text(color = "black", weight = "bolder", transform = "uppercase")),
      locations = cells_row_groups()
    ) %>%
    tab_options(
      table.align = "center",
      heading.align = "center",
      heading.background.color = "lightblue",
      heading.title.font.size = px(20),
      heading.title.font.weight = "bolder",
      column_labels.font.size = px(18),
      column_labels.font.weight = "bolder",
      column_labels.background.color = "lightblue",
      table_body.hlines.color = "transparent",
      table.border.top.width = px(2),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(2)
    )
}

# -----------------------------------------------------------------------------
# Table Generators: Standings
# -----------------------------------------------------------------------------

#' Generate and save standings table
#' @param standings Data frame of standings
#' @param title Table title
#' @param subtitle Table subtitle
#' @param filename Output filename
#' @param output_path Output directory (default: OUTPUT_PATHS$history)
generate_standings_table <- function(standings, title, subtitle, filename,
                                     output_path = OUTPUT_PATHS$history) {
  gt_table <- standings %>%
    gt() %>%
    tab_header(title = md(title), subtitle = md(subtitle)) %>%
    gt_theme_espn_custom() %>%
    cols_align("left", columns = 1) %>%
    tab_source_note(md("**Seasons: 2020-2023 | PF/PA Represent Avg/Gm**"))

  gtsave(gt_table, paste0(output_path, filename))
  invisible(gt_table)
}

# -----------------------------------------------------------------------------
# Table Generators: Head-to-Head
# -----------------------------------------------------------------------------

#' Generate head-to-head table for a team
#' @param h2h_data Head-to-head data frame
#' @param team Team name to filter for
#' @return gt table object
generate_h2h_table <- function(h2h_data, team) {
  setup_fonts()

  h2h_data %>%
    filter(tm == team) %>%
    arrange(-wins, -avg_mrg) %>%
    select(opp, wins, losses, wp = winperc, pf, pa, avg_mrg) %>%
    gt() %>%
    tab_header(title = md("**All-Time Results (Head to Head)**")) %>%
    fmt_number(columns = c(pf, pa, avg_mrg), decimals = 1) %>%
    gt_theme_schedule()
}

#' Save head-to-head tables for all teams
#' @param h2h_data Head-to-head data frame
#' @param output_path Output directory
save_all_h2h_tables <- function(h2h_data, output_path = OUTPUT_PATHS$headtohead) {
  unique_teams <- unique(h2h_data$tm)

  for (team in unique_teams) {
    gt_table <- generate_h2h_table(h2h_data, team)
    gtsave(gt_table, paste0(output_path, team, "_head_to_head.png"))
  }
}

# -----------------------------------------------------------------------------
# Table Generators: Season Schedule
# -----------------------------------------------------------------------------

#' Generate season schedule table for a team
#' @param recap_data Season recap data (from get_season_recap)
#' @param team Team name
#' @param season Season year
#' @return gt table object
generate_schedule_table <- function(recap_data, team, season) {
  setup_fonts()

  recap_data %>%
    gt() %>%
    gt_highlight_rows(
      rows = streak > 0 | margin >= 0,
      fill = HIGHLIGHT_COLOR,
      bold_target_only = FALSE
    ) %>%
    tab_header(title = paste(season, "Schedule")) %>%
    fmt_number(columns = c(pf, pa, margin), decimals = 1) %>%
    cols_align(align = "center", columns = c(win_loss, pf, pa, margin, streak)) %>%
    gt_theme_schedule()
}

# -----------------------------------------------------------------------------
# Table Generators: Blowouts & Close Calls
# -----------------------------------------------------------------------------

#' Generate blowouts or close calls table
#' @param data Data frame from get_blowouts or get_close_calls
#' @param title Table title
#' @return gt table object
generate_notable_games_table <- function(data, title) {
  decimals <- ifelse(grepl("Close", title), 2, 1)

  data %>%
    gt() %>%
    tab_header(title = md(title)) %>%
    fmt_number(columns = c(pf, pa, margin), decimals = decimals) %>%
    cols_align(align = "center", columns = c(season, week)) %>%
    cols_align(align = "center", columns = c(pf, pa, margin)) %>%
    gt_theme_schedule()
}

# -----------------------------------------------------------------------------
# Table Generators: Power Rankings
# -----------------------------------------------------------------------------

#' Generate power rankings table
#' @param power_data Power rankings data
#' @param season_year Season year (or "All" for combined)
#' @return gt table object
generate_power_rankings_table <- function(power_data, season_year) {
  setup_fonts()

  title_text <- if (season_year == "All") {
    "All Seasons Power Rankings"
  } else {
    paste(season_year, "Power Rankings")
  }

  data_to_show <- if (season_year != "All") {
    power_data %>% select(-season)
  } else {
    power_data
  }

  data_to_show %>%
    gt() %>%
    tab_header(title = md(title_text), subtitle = "Adj by League Season Avg") %>%
    gt_theme_schedule() %>%
    cols_align("center", columns = everything()) %>%
    tab_options(data_row.padding = px(5)) %>%
    tab_source_note(md("**Calc**: 60% Avg PF, 20% Win %, 20% PF Variance"))
}

# -----------------------------------------------------------------------------
# Table Generators: Trades
# -----------------------------------------------------------------------------

#' Generate trade history table for a team
#' @param trade_data Processed trade data
#' @param team Team name
#' @return gt table object
generate_trade_table <- function(trade_data, team) {
  setup_fonts()

  gt_data <- trade_data %>%
    filter(team_name == team) %>%
    arrange(season, date) %>%
    mutate(
      players_traded_for = gsub(",", "<br>", players_traded_for),
      players_traded_away = gsub(",", "<br>", players_traded_away)
    )

  gt_data %>%
    select(
      season, date, team_name,
      team = trade_partner,
      received = players_traded_for,
      traded = players_traded_away
    ) %>%
    gt() %>%
    cols_hide(columns = c(season, team_name)) %>%
    cols_width(date ~ px(80), team ~ px(80)) %>%
    tab_header(title = paste(team, "Trades by Season"), subtitle = "2020 - 2023") %>%
    fmt_markdown(columns = c(received, traded)) %>%
    gt_theme_trades()
}

#' Save trade tables for all teams
#' @param trade_data Processed trade data
#' @param output_path Output directory
save_all_trade_tables <- function(trade_data, output_path = OUTPUT_PATHS$trades) {
  unique_teams <- unique(trade_data$team_name)

  for (team in unique_teams) {
    gt_table <- generate_trade_table(trade_data, team)
    gtsave(gt_table, paste0(output_path, "Franchise_Trades_", team, ".png"))
  }
}

# -----------------------------------------------------------------------------
# Table Generators: Dynasty Rosters
# -----------------------------------------------------------------------------

#' Generate dynasty roster table for a team
#' @param roster_data Roster data with rankings
#' @param team Team name
#' @param season Season year
#' @return gt table object
generate_dynasty_roster_table <- function(roster_data, team, season = 2024) {
  setup_fonts()

  user_data <- roster_data %>%
    filter(team_name == team) %>%
    arrange(pos, fp_rank)

  # Build starting lineup
  starting_lineup <- bind_rows(
    user_data %>% filter(pos == "QB") %>% head(1),
    user_data %>% filter(pos == "RB") %>% head(2),
    user_data %>% filter(pos == "WR") %>% head(2),
    user_data %>% filter(pos == "TE") %>% head(1)
  ) %>%
    mutate(roster = "A-List")

  # Add flex players
  flex_players <- user_data %>%
    filter(!(name %in% starting_lineup$name), pos %in% c("WR", "RB", "TE")) %>%
    head(2) %>%
    mutate(roster = "A-List")

  # Add bench
  bench <- user_data %>%
    filter(
      !(name %in% starting_lineup$name),
      !(name %in% flex_players$name)
    ) %>%
    arrange(fp_rank) %>%
    head(12) %>%
    mutate(roster = "Bench")

  lineup <- bind_rows(starting_lineup, flex_players, bench)

  lineup %>%
    filter(team_name == team) %>%
    group_by(roster) %>%
    select(headshot, name, team, pos, fp_tier, fp_posrank, fp_rank, playerprofiler, dynastypros) %>%
    gt() %>%
    tab_header(title = md(paste0("**", team, " ", season, " Dynasty Roster**"))) %>%
    cols_align("center", columns = 3:9) %>%
    gt_img_rows(headshot) %>%
    fmt_number(columns = 6:9, decimals = 0) %>%
    gt_theme_dynasty(season_label = paste(season, "Season"))
}

# -----------------------------------------------------------------------------
# Batch Table Generation
# -----------------------------------------------------------------------------

#' Generate all standings tables
#' @param full_schedule Full schedule data
generate_all_standings <- function(full_schedule) {
  # Load transform functions
  source("R/data_transform.R")

  # Season-specific standings
  for (config in SEASON_STANDINGS_CONFIG) {
    standings <- calculate_standings(
      full_schedule,
      season_filter = config$season,
      champion = config$champion
    )
    generate_standings_table(standings, config$title, config$subtitle, config$filename)
  }

  # All-time regular season
  all_time_standings <- calculate_standings(
    full_schedule,
    champion = unlist(CHAMPIONS)
  )
  generate_standings_table(
    all_time_standings,
    "**Dynasty Insanity**",
    "**All-Time Standings**",
    "all_time_standings.png"
  )

  # All-time playoffs
  playoff_standings <- calculate_standings(
    full_schedule,
    is_regular = FALSE,
    champion = unlist(CHAMPIONS)
  )
  generate_standings_table(
    playoff_standings,
    "**Dynasty Insanity**",
    "**All-Time Playoff Standings**",
    "all_time_post_standings.png"
  )
}

#' Generate all power ranking tables
#' @param full_schedule Full schedule data
generate_all_power_rankings <- function(full_schedule) {
  source("R/data_transform.R")

  for (season in SEASONS) {
    power_rankings <- calculate_power_rankings(full_schedule, season)
    gt_table <- generate_power_rankings_table(power_rankings, season)
    gtsave(gt_table, paste0(OUTPUT_PATHS$history, "power_rank_standings_yearly_", season, ".png"))
  }

  # Combined
  combined_rankings <- calculate_power_rankings(full_schedule)
  gt_table <- generate_power_rankings_table(combined_rankings, "All")
  gtsave(gt_table, paste0(OUTPUT_PATHS$history, "combined_power_rankings.png"))
}

#' Generate all season schedule tables
#' @param full_schedule Full schedule data
generate_all_season_schedules <- function(full_schedule) {
  source("R/data_transform.R")

  for (season in SEASONS) {
    teams <- unique(full_schedule$tm[full_schedule$season == season])

    for (team in teams) {
      recap <- get_season_recap(full_schedule, team, season)
      gt_table <- generate_schedule_table(recap, team, season)
      filename <- paste0(OUTPUT_PATHS$season_schedule, season, "_", team, "_season_schedule.png")
      gtsave(gt_table, filename)
    }
  }
}

# -----------------------------------------------------------------------------
# Table Generators: All-Play Rankings
# -----------------------------------------------------------------------------

#' Generate All-Play rankings table for a season
#' @param all_play_data All-play rankings data
#' @param season_year Season year
#' @return gt table object
generate_all_play_table <- function(all_play_data, season_year) {
  setup_fonts()

  title_text <- if (season_year == "All") {
    "All-Time All-Play Rankings"
  } else {
    paste(season_year, "All-Play Rankings")
  }

  data_filtered <- all_play_data
  if (season_year != "All") {
    data_filtered <- data_filtered %>%
      filter(season == season_year) %>%
      select(-season)
  }

  data_filtered %>%
    mutate(
      rank = row_number(),
      avg_score = round(avg_score, 1),
      avg_rank = round(avg_rank, 1)
    ) %>%
    select(rank, team, ap_wins, ap_losses, ap_pct, avg_score, avg_rank) %>%
    gt() %>%
    tab_header(
      title = md(paste0("**", title_text, "**")),
      subtitle = "What if you played everyone every week?"
    ) %>%
    cols_label(
      rank = "#",
      team = "Team",
      ap_wins = "AP Wins",
      ap_losses = "AP Losses",
      ap_pct = "AP Win%",
      avg_score = "Avg PF",
      avg_rank = "Avg Rank"
    ) %>%
    cols_align(align = "center", columns = c(rank, ap_wins, ap_losses, ap_pct, avg_score, avg_rank)) %>%
    cols_align(align = "left", columns = team) %>%
    gt_theme_schedule() %>%
    tab_source_note(
      md("**All-Play**: Record if you played every team each week")
    )
}

#' Save all-play tables for all seasons
#' @param full_schedule Full schedule data
#' @param output_path Output directory
save_all_play_tables <- function(full_schedule, output_path = OUTPUT_PATHS$history) {
  source("R/data_transform.R")

  for (season in SEASONS) {
    all_play_data <- get_all_play_rankings(full_schedule, season)
    gt_table <- generate_all_play_table(all_play_data, season)
    gtsave(gt_table, paste0(output_path, "all_play_rankings_", season, ".png"))
  }

  # Combined all seasons
  all_play_combined <- get_all_play_rankings(full_schedule, NULL)
  gt_table <- generate_all_play_table(all_play_combined, "All")
  gtsave(gt_table, paste0(output_path, "all_play_rankings_combined.png"))
}

# -----------------------------------------------------------------------------
# Table Generators: Z-Score Rankings
# -----------------------------------------------------------------------------

#' Generate Z-Score rankings table for a season
#' @param zscore_data Z-score rankings data
#' @param season_year Season year
#' @return gt table object
generate_zscore_table <- function(zscore_data, season_year) {
  setup_fonts()

  title_text <- if (season_year == "All") {
    "All-Time Z-Score Power Rankings"
  } else {
    paste(season_year, "Z-Score Power Rankings")
  }

  data_filtered <- zscore_data
  if (season_year != "All") {
    data_filtered <- data_filtered %>%
      filter(season == season_year) %>%
      select(-season)
  }

  data_filtered %>%
    mutate(
      rank = row_number(),
      avg_pf = round(avg_pf, 1),
      consistency = round(consistency, 1),
      ap_pct = scales::percent(ap_pct, accuracy = 0.1),
      actual_wp = scales::percent(actual_wp, accuracy = 0.1),
      z_score = round(z_score, 2),
      z_allplay = round(z_allplay, 2),
      z_consist = round(z_consist, 2)
    ) %>%
    select(rank, team, power, avg_pf, ap_pct, actual_wp, z_score, z_allplay, z_consist) %>%
    gt() %>%
    tab_header(
      title = md(paste0("**", title_text, "**")),
      subtitle = "45% Scoring | 35% All-Play | 20% Consistency"
    ) %>%
    tab_spanner(label = "Performance", columns = c(avg_pf, ap_pct, actual_wp)) %>%
    tab_spanner(label = "Z-Scores", columns = c(z_score, z_allplay, z_consist)) %>%
    cols_label(
      rank = "#",
      team = "Team",
      power = "Power",
      avg_pf = "Avg PF",
      ap_pct = "All-Play%",
      actual_wp = "Actual W%",
      z_score = "Scoring",
      z_allplay = "All-Play",
      z_consist = "Consist"
    ) %>%
    cols_align(align = "center", columns = -team) %>%
    cols_align(align = "left", columns = team) %>%
    tab_style(
      style = cell_fill(color = "#e8f4e8"),
      locations = cells_body(columns = power, rows = power >= 55)
    ) %>%
    tab_style(
      style = cell_fill(color = "#f4e8e8"),
      locations = cells_body(columns = power, rows = power < 45)
    ) %>%
    gt_theme_schedule() %>%
    tab_source_note(
      md("**Power**: 50 = league average | **Z-Score**: 0 = average, +1 = 1 std dev above")
    )
}

#' Save z-score tables for all seasons
#' @param full_schedule Full schedule data
#' @param output_path Output directory
save_zscore_tables <- function(full_schedule, output_path = OUTPUT_PATHS$history) {
  source("R/data_transform.R")

  for (season in SEASONS) {
    zscore_data <- get_zscore_rankings(full_schedule, season)
    gt_table <- generate_zscore_table(zscore_data, season)
    gtsave(gt_table, paste0(output_path, "zscore_rankings_", season, ".png"))
  }

  # Combined all seasons
  zscore_combined <- get_zscore_rankings(full_schedule, NULL)
  gt_table <- generate_zscore_table(zscore_combined, "All")
  gtsave(gt_table, paste0(output_path, "zscore_rankings_combined.png"))
}

# -----------------------------------------------------------------------------
# Quadrant Plot: All-Play vs Z-Score Power
# -----------------------------------------------------------------------------

#' Generate quadrant plot showing All-Play Win% vs Z-Score Power Rating
#'
#' Creates a scatter plot with four quadrants:
#'   - Top-Right: High scoring + high all-play (true elite)
#'   - Top-Left: Low scoring + high all-play (lucky/efficient)
#'   - Bottom-Right: High scoring + low all-play (unlucky)
#'   - Bottom-Left: Low scoring + low all-play (rebuilding)
#'
#' @param full_schedule Full schedule data
#' @param season_year Season to plot
#' @param output_path Output directory
#' @return ggplot object
generate_power_quadrant_plot <- function(full_schedule, season_year, output_path = OUTPUT_PATHS$history) {
  source("R/data_transform.R")

  # Get quadrant data

  quadrant_data <- get_power_quadrant_data(full_schedule, season_year)

  # Calculate means for quadrant lines
  mean_ap <- mean(quadrant_data$ap_win_pct)
  mean_power <- mean(quadrant_data$power_rating)

  # Assign quadrant labels
  quadrant_data <- quadrant_data %>%
    mutate(
      quadrant = case_when(
        ap_win_pct >= mean_ap & power_rating >= mean_power ~ "Elite",
        ap_win_pct >= mean_ap & power_rating < mean_power ~ "Efficient",
        ap_win_pct < mean_ap & power_rating >= mean_power ~ "Unlucky",
        TRUE ~ "Rebuilding"
      ),
      quadrant = factor(quadrant, levels = c("Elite", "Efficient", "Unlucky", "Rebuilding"))
    )

  # Create the plot
  p <- ggplot(quadrant_data, aes(x = ap_win_pct * 100, y = power_rating)) +
    # Quadrant shading
    annotate("rect",
      xmin = mean_ap * 100, xmax = Inf, ymin = mean_power, ymax = Inf,
      fill = "#d4edda", alpha = 0.3
    ) +
    annotate("rect",
      xmin = -Inf, xmax = mean_ap * 100, ymin = mean_power, ymax = Inf,
      fill = "#fff3cd", alpha = 0.3
    ) +
    annotate("rect",
      xmin = mean_ap * 100, xmax = Inf, ymin = -Inf, ymax = mean_power,
      fill = "#cce5ff", alpha = 0.3
    ) +
    annotate("rect",
      xmin = -Inf, xmax = mean_ap * 100, ymin = -Inf, ymax = mean_power,
      fill = "#f8d7da", alpha = 0.3
    ) +
    # Quadrant dividers
    geom_vline(xintercept = mean_ap * 100, linetype = "dashed", color = "gray40", linewidth = 0.8) +
    geom_hline(yintercept = mean_power, linetype = "dashed", color = "gray40", linewidth = 0.8) +
    # Points
    geom_point(aes(color = quadrant), size = 4, alpha = 0.8) +
    # Labels
    geom_text_repel(
      aes(label = tm),
      size = 3.5,
      fontface = "bold",
      box.padding = 0.4,
      point.padding = 0.3,
      segment.color = "gray50",
      max.overlaps = 15
    ) +
    # Quadrant labels
    annotate("text", x = max(quadrant_data$ap_win_pct * 100) - 5,
             y = max(quadrant_data$power_rating) - 2,
             label = "ELITE", fontface = "bold", color = "#155724", size = 4, alpha = 0.7) +
    annotate("text", x = min(quadrant_data$ap_win_pct * 100) + 5,
             y = max(quadrant_data$power_rating) - 2,
             label = "EFFICIENT", fontface = "bold", color = "#856404", size = 4, alpha = 0.7) +
    annotate("text", x = max(quadrant_data$ap_win_pct * 100) - 5,
             y = min(quadrant_data$power_rating) + 2,
             label = "UNLUCKY", fontface = "bold", color = "#004085", size = 4, alpha = 0.7) +
    annotate("text", x = min(quadrant_data$ap_win_pct * 100) + 5,
             y = min(quadrant_data$power_rating) + 2,
             label = "REBUILDING", fontface = "bold", color = "#721c24", size = 4, alpha = 0.7) +
    # Scales and theme
    scale_color_manual(
      values = c(
        "Elite" = "#28a745",
        "Efficient" = "#ffc107",
        "Unlucky" = "#007bff",
        "Rebuilding" = "#dc3545"
      ),
      guide = "none"
    ) +
    scale_x_continuous(
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = 0.1)
    ) +
    labs(
      title = paste(season_year, "Power Quadrant Analysis"),
      subtitle = "All-Play Win% (X) vs Z-Score Power Rating (Y)",
      x = "All-Play Win %",
      y = "Z-Score Power Rating",
      caption = paste0(
        "Quadrant lines at league averages: ",
        round(mean_ap * 100, 1), "% AP Win / ",
        round(mean_power, 1), " Power"
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 9, color = "gray50"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      axis.title = element_text(face = "bold"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  # Save the plot
  filename <- paste0(output_path, "power_quadrant_", season_year, ".png")
  ggsave(filename, p, width = 10, height = 8, dpi = 150, bg = "white")

  invisible(p)
}

#' Generate quadrant plots for all seasons
#' @param full_schedule Full schedule data
#' @param output_path Output directory
save_all_quadrant_plots <- function(full_schedule, output_path = OUTPUT_PATHS$history) {
  for (season in SEASONS) {
    generate_power_quadrant_plot(full_schedule, season, output_path)
  }
}

# -----------------------------------------------------------------------------
# Batch Generation: All New Power Rankings
# -----------------------------------------------------------------------------

#' Generate all new power ranking outputs (All-Play, Z-Score, Quadrants)
#' @param full_schedule Full schedule data
generate_all_new_power_rankings <- function(full_schedule) {
  message("\n--- Generating All-Play Rankings ---")
  save_all_play_tables(full_schedule)

  message("\n--- Generating Z-Score Rankings ---")
  save_zscore_tables(full_schedule)

  message("\n--- Generating Power Quadrant Plots ---")
  save_all_quadrant_plots(full_schedule)

  message("New power rankings complete!")
}

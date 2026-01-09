# =============================================================================
# Fantasy Report Configuration
# =============================================================================
# Central configuration file containing all constants, league IDs, and lookup
# tables used throughout the fantasy report generation pipeline.
# =============================================================================

# -----------------------------------------------------------------------------
# League Configuration
# -----------------------------------------------------------------------------

#' Sleeper league IDs by season
#' Each season has a unique league ID on the Sleeper platform
LEAGUE_IDS <- list(
  "2020" = "591530379404427264",
  "2021" = "650064757319118848",
  "2022" = "785068521058115584",
  "2023" = "917507729030352896",
  "2024" = "1048425511736713216",
  "2025" = "1180377149789876224"
)

#' Draft IDs corresponding to each league
DRAFT_IDS <- list(
  "2020" = "591530380872433664",
  "2021" = "650064757319118849",
  "2022" = "785068521058115585",
  "2023" = "1003817666127179776"
)

#' Active seasons for analysis
#' Add new seasons to this vector to include them automatically
SEASONS <- 2020:2025

#' Get the most recent season
CURRENT_SEASON <- max(SEASONS)

#' Number of weeks per season (2020 had 17, 2021+ have 18)
WEEKS_BY_SEASON <- list(
  "2020" = 1:17,
  "2021" = 1:18,
  "2022" = 1:18,
  "2023" = 1:18,
  "2024" = 1:18,
  "2025" = 1:18
)

# -----------------------------------------------------------------------------
# User Name Lookups
# -----------------------------------------------------------------------------

#' Map Sleeper usernames to display names
#' Used for consistent naming across all outputs
USERNAME_LOOKUP <- c(
  "nhosta"         = "Hosta",
  "bellist"        = "Tom",
  "tbellis"        = "Tom",
  "pdpablo"        = "Patrick",
  "zacgeoffray"    = "Zac",
  "naaderbanki"    = "Naad",
  "ttsao"          = "Tommy",
  "elrandal"       = "Randal",
  "Alanasty"       = "JP",
  "JayPeeA"        = "JP",
  "JohnWickMD"     = "Aviel",
  "slobonmynoblin" = "Logan",
  "asmontalvo"     = "Montel",
  "cpmadden1"      = "Conor",
  "patrickliou"    = "Pat Liou",
  "stankmasterP"   = "Peter"
)

#' Map franchise IDs to display names (for current season)
FRANCHISE_ID_LOOKUP <- c(
  "1"  = "Tom",
  "2"  = "Peter",
  "3"  = "Tommy",
  "4"  = "JP",
  "5"  = "Patrick",
  "6"  = "Aviel",
  "7"  = "Conor",
  "8"  = "Hosta",
  "9"  = "Naad",
  "10" = "Zac",
  "11" = "Pat Liou",
  "12" = "Randal"
)

#' Historical franchise ownership changes
#' Used to track owner changes for historical accuracy
#' Format: franchise_id -> list of (seasons, owner_name)
FRANCHISE_OWNERSHIP_HISTORY <- list(
  "2" = list(
    list(seasons = 2020:2024, owner = "Montel"),
    list(seasons = 2025:2099, owner = "Peter")  # 2025 onwards
  ),
  "11" = list(
    list(seasons = 2020:2022, owner = "Logan"),
    list(seasons = 2023:2099, owner = "Pat L")  # 2023 onwards
  )
)

# -----------------------------------------------------------------------------
# Playoff Teams by Season
# -----------------------------------------------------------------------------

PLAYOFF_TEAMS <- list(
  "2020" = c("Hosta", "Patrick", "Tom", "Zac", "Tommy", "JP"),
  "2021" = c("Hosta", "Patrick", "Tom", "Zac", "Naad", "Randal"),
  "2022" = c("Hosta", "Patrick", "Tom", "Zac", "Tommy", "Naad"),
  "2023" = c("Zac", "Aviel", "Tommy", "JP", "Tom", "Naad"),
  "2024" = c("Naad", "Tommy", "JP", "Zac", "Tom", "Montel"),
  "2025" = c("JP", "Tom", "Pat Liou", "Zac", "Naad", "Patrick")
)

#' Champions by season (for trophy indicator)
CHAMPIONS <- list(
  "2020" = "Hosta",
  "2021" = "Naad",
  "2022" = "Tommy",
  "2023" = "Tom",
  "2024" = "JP",
  "2025" = "JP"
)

# -----------------------------------------------------------------------------
# Season Standings Configuration
# -----------------------------------------------------------------------------

#' Standings metadata for each season
SEASON_STANDINGS_CONFIG <- list(

  list(
    season   = 2020,
    title    = "**2020 Standings**",
    subtitle = "**Nick Chubb's Golden Taint**",
    filename = "2020_standings.png",
    champion = "Hosta"
  ),
  list(
    season   = 2021,
    title    = "**2021 Standings**",
    subtitle = "**Iron Banki Claims His Throne**",
    filename = "2021_standings.png",
    champion = "Naad"
  ),
  list(
    season   = 2022,
    title    = "**2022 Standings**",
    subtitle = "**Tommy's Tainted Trophy**",
    filename = "2022_standings.png",
    champion = "Tommy"
  ),
  list(
    season   = 2023,
    title    = "**2023 Standings**",
    subtitle = "**Tom Avoids Buffalo Futility**",
    filename = "2023_standings.png",
    champion = "Tom"
  ),
  list(
    season   = 2024,
    title    = "**2024 Standings**",
    subtitle = "**JP's Redemption Arc**",
    filename = "2024_standings.png",
    champion = "JP"
  ),
  list(
    season   = 2025,
    title    = "**2025 Standings**",
    subtitle = "**JP Goes Back-to-Back**",
    filename = "2025_standings.png",
    champion = "JP"
  )
)

# -----------------------------------------------------------------------------
# Trade Tribunal Constants
# -----------------------------------------------------------------------------

#' Win threshold for declaring a trade winner (in fantasy points)
TRADE_WIN_THRESHOLD <- 50

#' Measurement window in seasons for trade evaluation
TRADE_MEASUREMENT_SEASONS <- 2

#' Trade analysis color palette
TRADE_COLORS <- list(
  winner = "#10b981",     # Emerald - winning verdict
  loser = "#f43f5e",      # Rose - losing verdict
  push = "#64748b",       # Slate - neutral/push
  header = "#1e3a5a",     # Navy - table headers
  background = "#f8fafc", # Off-white - backgrounds
  accent = "#fbbf24"      # Gold - marquee/best trades
)

# -----------------------------------------------------------------------------
# Styling Constants
# -----------------------------------------------------------------------------

#' Primary color for gt table styling
PRIMARY_COLOR <- "#585d93"

#' Alternate row color for striping
STRIPE_COLOR <- "#ededed"

#' Highlight color for wins/positive margins
HIGHLIGHT_COLOR <- "#FFE990"

#' Header background color for trade tables
TRADE_HEADER_COLOR <- "#ffdc73"

# -----------------------------------------------------------------------------
# Output Paths
# -----------------------------------------------------------------------------

#' Output paths - current_year is dynamically set based on CURRENT_SEASON
OUTPUT_PATHS <- list(
  history         = "output/history/",
  headtohead      = "output/headtohead/",
  season_schedule = "output/season_schedule/",
  rosters         = "output/rosters/",
  trades          = "output/trades/",
  current_year    = paste0("output/", CURRENT_SEASON + 1, "/")
)

# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

#' Clean username using lookup table
#' @param username Character. Raw username from Sleeper API
#' @return Character. Cleaned display name
clean_username <- function(username) {
  if (username %in% names(USERNAME_LOOKUP)) {
    return(USERNAME_LOOKUP[username])
  }
  return(username)
}

#' Get franchise owner name for a specific season
#' @param franchise_id Character. The franchise ID
#' @param season Integer. The season year
#' @return Character. The owner's display name
get_franchise_owner <- function(franchise_id, season) {
  franchise_id <- as.character(franchise_id)

  # Check if this franchise has ownership history

  if (franchise_id %in% names(FRANCHISE_OWNERSHIP_HISTORY)) {
    history <- FRANCHISE_OWNERSHIP_HISTORY[[franchise_id]]
    for (period in history) {
      if (season %in% period$seasons) {
        return(period$owner)
      }
    }
  }

  # Fall back to current lookup
  if (franchise_id %in% names(FRANCHISE_ID_LOOKUP)) {
    return(FRANCHISE_ID_LOOKUP[franchise_id])
  }

  # Return franchise_id if no mapping found
  return(franchise_id)
}

#' Get weeks for a given season
#' @param season Integer. The season year
#' @return Integer vector. Week numbers for that season
get_weeks_for_season <- function(season) {
  WEEKS_BY_SEASON[[as.character(season)]]
}

#' Get league ID for a given season
#' @param season Integer. The season year
#' @return Character. The Sleeper league ID
get_league_id <- function(season) {

  LEAGUE_IDS[[as.character(season)]]
}

#' Check if team made playoffs in a given season
#' @param team Character. Team display name
#' @param season Integer. The season year
#' @return Logical. TRUE if team made playoffs
is_playoff_team <- function(team, season) {
  playoff_teams <- PLAYOFF_TEAMS[[as.character(season)]]
  team %in% playoff_teams
}

#' Get regular season week count for a season
#' @param season Integer. The season year
#' @return Integer. Number of regular season weeks
#' @details NFL expanded to 17 games in 2021, so 2020 had 13 regular season weeks
#'          and 2021+ have 14 regular season weeks for fantasy purposes.
get_regular_season_weeks <- function(season) {
  if (season == 2020) return(13)
  return(14)
}

#' Get playoff week cutoff for a season
#' @param season Integer. The season year
#' @return Integer. First week of playoffs
get_playoff_week_cutoff <- function(season) {
  get_regular_season_weeks(season) + 1
}

#' Check if a week is regular season for a given season
#' @param week Integer. The week number
#' @param season Integer. The season year
#' @return Logical. TRUE if regular season week
is_regular_season_week <- function(week, season) {
  week <= get_regular_season_weeks(season)
}

# -----------------------------------------------------------------------------
# Season Configuration Factory
# -----------------------------------------------------------------------------

#' Create a season configuration object
#' @param season Integer. The season year
#' @param title Optional. Custom title for the season
#' @param subtitle Optional. Custom subtitle for the season
#' @param champion Optional. Champion team name (for trophy emoji)
#' @return List containing season configuration
create_season_config <- function(season, title = NULL, subtitle = NULL, champion = NULL) {
  # Auto-generate title/subtitle if not provided
  if (is.null(title)) {
    title <- paste0("**", season, " Standings**")
  }
  if (is.null(subtitle)) {
    subtitle <- paste0("**Season ", season, "**")
  }

  list(
    season   = season,
    title    = title,
    subtitle = subtitle,
    filename = paste0(season, "_standings.png"),
    champion = champion,
    regular_season_weeks = get_regular_season_weeks(season),
    playoff_week_cutoff = get_playoff_week_cutoff(season),
    league_id = get_league_id(season),
    weeks = get_weeks_for_season(season)
  )
}

#' Get season range as formatted string
#' @param seasons Integer vector of seasons (default: SEASONS)
#' @return Character. Formatted range string (e.g., "2020-2023")
get_season_range_label <- function(seasons = SEASONS) {
  paste(min(seasons), max(seasons), sep = "-")
}

#' Get all configured seasons' metadata
#' @return List of season configs for all SEASONS
get_all_season_configs <- function() {
  configs <- lapply(SEASONS, function(s) {
    # Use predefined config if available
    existing <- Filter(function(x) x$season == s, SEASON_STANDINGS_CONFIG)
    if (length(existing) > 0) {
      return(existing[[1]])
    }
    # Otherwise generate default
    champion <- CHAMPIONS[[as.character(s)]]
    create_season_config(s, champion = champion)
  })
  names(configs) <- as.character(SEASONS)
  configs
}

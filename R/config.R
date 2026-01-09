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
  "2024" = "1048425511736713216"
)

#' Draft IDs corresponding to each league
DRAFT_IDS <- list(
  "2020" = "591530380872433664",
  "2021" = "650064757319118849",
  "2022" = "785068521058115585",
  "2023" = "1003817666127179776"
)

#' Active seasons for analysis
SEASONS <- 2020:2023

#' Number of weeks per season (2020 had 17, 2021+ have 18)
WEEKS_BY_SEASON <- list(

  "2020" = 1:17,
  "2021" = 1:18,
  "2022" = 1:18,
  "2023" = 1:18,
  "2024" = 1:18
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
  "patrickliou"    = "Pat Liou"
)

#' Map franchise IDs to display names (for 2024 season)
FRANCHISE_ID_LOOKUP <- c(
  "1"  = "Tom",
  "2"  = "Montel",
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

# -----------------------------------------------------------------------------
# Playoff Teams by Season
# -----------------------------------------------------------------------------

PLAYOFF_TEAMS <- list(
  "2020" = c("Hosta", "Patrick", "Tom", "Zac", "Tommy", "JP"),
  "2021" = c("Hosta", "Patrick", "Tom", "Zac", "Naad", "Randal"),
  "2022" = c("Hosta", "Patrick", "Tom", "Zac", "Tommy", "Naad"),
  "2023" = c("Zac", "Aviel", "Tommy", "JP", "Tom", "Naad")
)

#' Champions by season (for trophy indicator)
CHAMPIONS <- list(
  "2020" = "Hosta",
  "2021" = "Naad",
  "2022" = "Tommy",
  "2023" = "Tom"
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
  )
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

OUTPUT_PATHS <- list(
  history         = "output/history/",
  headtohead      = "output/headtohead/",
  season_schedule = "output/season_schedule/",
  rosters         = "output/rosters/",
  trades          = "output/trades/",
  current_year    = "output/2024/"
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

#' Get playoff week cutoff for a season
#' @param season Integer. The season year
#' @return Integer. First week of playoffs
get_playoff_week_cutoff <- function(season) {
  if (season == 2020) return(14)
  return(15)
}

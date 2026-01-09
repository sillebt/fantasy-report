# Dynasty Fantasy Insanity - Fantasy Report Generator

A comprehensive R-based analytics pipeline for generating fantasy football reports from Sleeper dynasty league data. Produces standings, power rankings, head-to-head records, trade histories, and various visualizations.

## Features

- **Multi-Season Support**: Analyzes data from 2020-2025 (easily extensible)
- **Modular Architecture**: Separated config, data fetching, transformation, and visualization layers
- **Dynamic Season Handling**: Automatic adjustment for NFL rule changes (13 vs 14 regular season weeks)
- **Comprehensive Outputs**:
  - Season standings with champion indicators
  - All-time standings (regular season and playoffs)
  - Head-to-head records for each team
  - Power rankings (legacy, All-Play, Z-Score)
  - Power quadrant visualizations (Luck vs Skill)
  - Trade history tables by team
  - Notable games (blowouts and close calls)
  - Median standings analysis

## Project Structure

```
fantasy-report/
├── R/
│   ├── config.R           # Central configuration (league IDs, team mappings, seasons)
│   ├── data_fetch.R       # Sleeper API data fetching functions
│   ├── data_transform.R   # Data processing and calculations
│   ├── visualizations.R   # GT table and ggplot generation
│   └── main.R             # Orchestration script
├── output/
│   ├── history/           # All-time standings, power rankings
│   ├── headtohead/        # H2H record tables by team
│   ├── season_schedule/   # Individual season schedules per team
│   ├── trades/            # Trade history tables
│   └── [year]/            # Year-specific outputs
├── report.Rmd             # R Markdown report template
└── README.md
```

## Quick Start

```r
# From project root
source("R/main.R")

# Generate all reports
generate_all_reports()

# Or generate specific outputs
generate_standings_only()    # Just standings tables
generate_h2h_only()          # Just head-to-head tables
generate_power_only()        # All-Play, Z-Score, Quadrant plots
```

## Configuration

All configuration is centralized in `R/config.R`:

### Adding a New Season

1. Add league ID to `LEAGUE_IDS`:
   ```r
   LEAGUE_IDS <- list(
     ...
     "2026" = "your_league_id_here"
   )
   ```

2. Update `SEASONS` range:
   ```r
   SEASONS <- 2020:2026
   ```

3. Add weeks config to `WEEKS_BY_SEASON`:
   ```r
   WEEKS_BY_SEASON <- list(
     ...
     "2026" = 1:18
   )
   ```

4. After season ends, add playoff teams and champion:
   ```r
   PLAYOFF_TEAMS <- list(
     ...
     "2026" = c("Team1", "Team2", ...)
   )

   CHAMPIONS <- list(
     ...
     "2026" = "WinningTeam"
   )
   ```

5. Add standings config with custom subtitle:
   ```r
   SEASON_STANDINGS_CONFIG <- list(
     ...
     list(
       season   = 2026,
       title    = "**2026 Standings**",
       subtitle = "**Your Creative Subtitle**",
       filename = "2026_standings.png",
       champion = "WinningTeam"
     )
   )
   ```

### Handling Franchise Ownership Changes

When a team changes owners, update `FRANCHISE_OWNERSHIP_HISTORY`:

```r
FRANCHISE_OWNERSHIP_HISTORY <- list(
  "2" = list(
    list(seasons = 2020:2024, owner = "OldOwner"),
    list(seasons = 2025:2099, owner = "NewOwner")
  )
)
```

Also update `USERNAME_LOOKUP` with the new owner's Sleeper username:
```r
USERNAME_LOOKUP <- c(
  ...
  "new_sleeper_username" = "NewOwner"
)
```

## Key Technical Details

### Regular Season Weeks
- **2020**: 13 weeks (NFL had 16-game season)
- **2021+**: 14 weeks (NFL expanded to 17-game season)

This is handled dynamically by `get_regular_season_weeks()`.

### Playoff vs Consolation Classification

Games are classified as:
- **Regular**: Weeks 1-13 (2020) or 1-14 (2021+)
- **Playoffs**: Championship bracket games only
- **Consolation**: 3rd/5th place games (excluded from analysis)

A game is "Playoffs" (championship path) only if at least one team hasn't lost yet that postseason. Once both teams have lost, subsequent games are consolation.

### League ID Precision

Sleeper league IDs are 18+ digit integers. R's numeric type loses precision at this scale. **Always keep league IDs as character strings** - never convert to numeric.

### Trade Data Column Handling

The Sleeper API returns inconsistent columns across seasons (e.g., 2021 missing `waiver_priority`). The code uses `dplyr::bind_rows()` instead of `rbind()` to handle this gracefully.

## Dependencies

```r
# Core
library(ffscrapr)
library(sleeperapi)
library(dplyr)
library(tidyr)
library(purrr)

# Visualization
library(gt)
library(gtExtras)
library(ggplot2)
library(scales)
library(showtext)

# Parallel processing (optional)
library(furrr)
library(future)

# Table export
library(webshot2)
```

## Output Examples

The pipeline generates PNG images for all tables and visualizations:

- `output/history/2025_standings.png` - Season standings
- `output/history/all_time_standings.png` - All-time regular season
- `output/history/all_time_post_standings.png` - All-time playoff standings
- `output/history/all_play_rankings_2025.png` - All-Play win percentage
- `output/history/zscore_rankings_2025.png` - Z-Score power rankings
- `output/history/power_quadrant_2025.png` - Luck vs Skill plot
- `output/headtohead/Tom_head_to_head.png` - H2H records
- `output/trades/Franchise_Trades_Tom.png` - Trade history

## Troubleshooting

### "404 Not Found" for league data
- Verify league ID is correct and stored as character (not numeric)
- Check that the season is included in `SEASONS`

### NA values in standings/rankings
- Ensure `na.rm = TRUE` is used in all aggregations
- Check for unplayed games (NA scores) being filtered out

### Inflated playoff loss counts
- Verify consolation games are being filtered out
- Check `season_type` classification logic

### Missing season outputs
- Ensure season is added to all config lists:
  - `LEAGUE_IDS`
  - `SEASONS`
  - `WEEKS_BY_SEASON`
  - `PLAYOFF_TEAMS` (after season ends)
  - `CHAMPIONS` (after season ends)
  - `SEASON_STANDINGS_CONFIG`

## License

Private project for Dynasty Fantasy Insanity league.

# Claude Code Project Instructions - Fantasy Report

## Project Overview

This is an R-based fantasy football analytics pipeline for the "Dynasty Fantasy Insanity" Sleeper league. It generates comprehensive reports including standings, power rankings, head-to-head records, and trade histories.

## Architecture

```
R/config.R         → Central configuration (league IDs, team names, seasons)
R/data_fetch.R     → Sleeper API interactions via ffscrapr
R/data_transform.R → Data processing, schedule building, calculations
R/visualizations.R → GT tables and ggplot visualizations
R/main.R           → Orchestration and entry points
```

## Key Functions

- `generate_all_reports()` - Main entry point, generates everything
- `build_full_schedule()` - Builds schedule with playoff/consolation classification
- `calculate_standings()` - Calculates win/loss records
- `calculate_power_rankings()` - Legacy power ranking calculation
- `calculate_all_play_record()` - All-Play win percentage
- `calculate_zscore_rankings()` - Z-Score based rankings

## Critical Technical Knowledge

### 1. League ID Precision
Sleeper league IDs are 18+ digit integers. **R's numeric type loses precision** at this scale.
- WRONG: `as.numeric("1180377149789876224")` → `1180377149789876352`
- RIGHT: Keep as character string throughout

### 2. Regular Season Weeks
NFL expanded from 16 to 17 games in 2021, affecting fantasy:
- **2020**: 13 regular season weeks
- **2021+**: 14 regular season weeks
- Handled by `get_regular_season_weeks(season)` in config.R

### 3. Playoff vs Consolation Games
Championship bracket vs consolation bracket classification:
- **Playoffs**: At least one team hasn't lost yet that postseason
- **Consolation**: Both teams already eliminated (lost before current week)
- A team can only lose 1 playoff game per season (elimination)
- 3rd/5th place games are consolation and excluded from playoff standings

### 4. Trade Data Column Mismatch
Sleeper API returns different columns across seasons:
- 2021 is missing `waiver_priority` column
- Use `dplyr::bind_rows()` not `rbind()` to handle mismatched columns

### 5. NA Handling
Always use `na.rm = TRUE` in aggregations:
- `sum(result, na.rm = TRUE)`
- `mean(tm_score, na.rm = TRUE)`
- Unplayed games have NA scores and should be filtered

## Adding a New Season Checklist

1. **config.R - LEAGUE_IDS**: Add new league ID as character string
2. **config.R - SEASONS**: Extend range (e.g., `2020:2026`)
3. **config.R - WEEKS_BY_SEASON**: Add `"2026" = 1:18`
4. **config.R - USERNAME_LOOKUP**: Add any new owner usernames
5. **config.R - FRANCHISE_OWNERSHIP_HISTORY**: Update if ownership changed
6. **After season ends**:
   - Add to `PLAYOFF_TEAMS`
   - Add to `CHAMPIONS`
   - Add to `SEASON_STANDINGS_CONFIG` with creative subtitle

## Ownership Change Pattern

When a franchise changes owners (like Montel → Peter in 2025):

```r
# Add to FRANCHISE_OWNERSHIP_HISTORY
"2" = list(
  list(seasons = 2020:2024, owner = "Montel"),
  list(seasons = 2025:2099, owner = "Peter")
)

# Update FRANCHISE_ID_LOOKUP to current owner
"2" = "Peter"

# Add username mapping
"stankmasterP" = "Peter"
```

## Testing Changes

After modifying data processing code:

```r
source("R/main.R")
generate_all_reports(parallel = FALSE)
```

Verify outputs in `output/` directory match expected data.

## Common Issues & Fixes

| Issue | Cause | Fix |
|-------|-------|-----|
| 404 API errors | League ID precision loss | Keep as character, don't convert to numeric |
| NA in standings | Missing `na.rm = TRUE` | Add to all sum/mean/max/min calls |
| Inflated playoff losses | Consolation games counted | Check `build_full_schedule()` classification |
| Missing season output | Season not in all configs | Add to all 6 config lists |
| Trade fetch fails | Column mismatch | Use `bind_rows()` not `rbind()` |

## File Locations

- **Outputs**: `output/history/`, `output/headtohead/`, etc.
- **Data exports**: `fullschedule.csv`, `trades_final.csv` in project root
- **Config**: All in `R/config.R`

## Current League Members (2025)

| ID | Owner | Sleeper Username |
|----|-------|------------------|
| 1 | Tom | tbellis |
| 2 | Peter | stankmasterP |
| 3 | Tommy | ttsao |
| 4 | JP | JayPeeA |
| 5 | Patrick | pdpablo |
| 6 | Aviel | JohnWickMD |
| 7 | Conor | cpmadden1 |
| 8 | Hosta | nhosta |
| 9 | Naad | naaderbanki |
| 10 | Zac | zacgeoffray |
| 11 | Pat Liou | patrickliou |
| 12 | Randal | elrandal |

## Historical Ownership Changes

- **Franchise 11**: Logan (2020-2022) → Pat Liou (2023+)
- **Franchise 2**: Montel (2020-2024) → Peter (2025+)

## Champions History

| Year | Champion |
|------|----------|
| 2020 | Hosta |
| 2021 | Naad |
| 2022 | Tommy |
| 2023 | Tom |
| 2024 | JP |
| 2025 | JP |

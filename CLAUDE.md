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

---

## Trade Tribunal System

### Overview

The Trade Tribunal (`R/trade_analysis.R`, `R/trade_visualizations.R`) objectively scores all trades from 2020-2024 using Value Over Replacement (VOR) methodology.

### Architecture

```
R/trade_analysis.R       → Trade scoring engine (VOR calculations, pick resolution)
R/trade_visualizations.R → GT tables for trade ledger, marquee cards, balance sheet
```

### Key Functions

- `generate_tribunal_only()` - Generate all Trade Tribunal outputs
- `process_all_trades()` - Score all trades with VOR
- `resolve_pick_to_player()` - Map draft picks to actual players selected
- `calculate_vor_value()` - Calculate player's VOR within measurement window
- `get_trade_balance_sheet()` - Net VOR per owner across all trades

### VOR (Value Over Replacement) Methodology

**Concept**: Measures player value relative to a "replacement level" player (someone freely available on waivers).

| Position | Replacement Level | Meaning |
|----------|-------------------|---------|
| QB | QB12 | 12th-best QB in league |
| RB | RB24 | 24th-best RB (2 starters + flex × 12 teams) |
| WR | WR24 | 24th-best WR |
| TE | TE12 | 12th-best TE |

**Calculation**:
```r
VOR = Player_Points - (Position_Baseline × Games_Played / Season_Length)
```

**Interpreting VOR**:
- **Positive VOR (+65)**: Player scored 65 more points than a waiver pickup would have
- **Negative VOR (-145)**: Player scored 145 FEWER points than replacement - actively hurt your team
- **Zero (0)**: Performed exactly at replacement level

**Why VOR over Raw Points**: QBs score ~300+ points/season while RBs score ~150. Raw points would make every QB-for-RB trade look like a robbery. VOR normalizes across positions.

### Draft Pick Resolution

**Critical Discovery**: Sleeper trade data includes `player_id` for picks in format:
```
"2023_round_1_pick_from_franchise_4"
```

This encodes the **original owner** of the pick. To resolve what player was drafted:

1. Parse `player_id` to extract: season (2023), round (1), original_franchise (4)
2. Fetch draft results using `sleeperapi::get_draft_picks(draft_id)`
3. Match on `draft_slot` (NOT `roster_id`) - `draft_slot` = original owner's slot
4. `roster_id` = who actually made the selection (may differ if pick was traded)

```r
# Key insight: draft_slot is the ORIGINAL owner
draft_lookup %>%
  filter(season == 2023, round == 1, draft_slot == 4)
# Returns: Jaxon Smith-Njigba (picked by whoever owned franchise 4's slot)
```

### Future Pick Valuation

Unresolved/future picks use projected values with 15% annual discount:
```r
base_value <- historical_avg_vor_for_round  # From league draft history
years_out <- pick_season - current_season
projected_value <- base_value * (0.85 ^ years_out)
```

### Trade Verdict Rules

| Margin | Verdict |
|--------|---------|
| ≥ 50 VOR | Winner declared |
| < 50 VOR | Push (too close to call) |

### Output Files

```
output/trades/
├── trade_tribunal_ledger.png      # Full trade table with all 104 trades
├── trade_balance_sheet.png        # Owner rankings by net VOR
├── trade_marquee_heists.png       # Top 3 biggest winners
├── trade_marquee_blockbusters.png # Top 3 most value exchanged (4+ assets)
├── trade_pick_revelations.png     # All draft picks traded → what they became
└── trade_analysis_data.rds        # Raw data for debugging
```

### Common Issues & Fixes

| Issue | Cause | Fix |
|-------|-------|-----|
| All VOR values = 0 | Visualization using wrong field | Use `asset$vor_points` not `asset$points` |
| Picks not resolving | Matching on `roster_id` | Match on `draft_slot` (original owner) |
| Players showing 0 games | VOR calc from trade date, not draft date | For picks, calculate from draft season |
| Missing `pos` column | `fetch_scoring_single` didn't include it | Add `pos` to select() in data_fetch.R |
| Mediocre "blockbuster" | Using sum(VOR) | Use `abs(side_a_vor) + abs(side_b_vor)` and require 4+ assets |

### Adding New Draft Year

Add to `config.R`:
```r
DRAFT_IDS <- list(
  "2020" = "591530380872433664",
  "2021" = "650064757319118849",
  "2022" = "785068521058115585",
  "2023" = "1003817666127179776",
  "2024" = "NEW_DRAFT_ID_HERE"  # Get from Sleeper
)
```

### Learnings & Gotchas

1. **sleeperapi vs ffscrapr**: `sleeperapi::get_draft_picks()` returns data directly (not nested). Don't use `map_dfr()` on it.

2. **draft_slot vs roster_id**: Critical distinction. `draft_slot` = original pick owner, `roster_id` = who selected. For traded picks, these differ.

3. **VOR can be negative**: This is correct! A player with -100 VOR means you lost 100 points vs just streaming free agents.

4. **Measurement window for picks**: Calculate VOR from the **draft season**, not the trade season. A 2020 trade for a 2023 pick should measure 2023-2024 production.

5. **Historical pick values**: Most picks have negative average VOR because most draft picks bust. Only 1st round has positive expected value (~48 VOR).

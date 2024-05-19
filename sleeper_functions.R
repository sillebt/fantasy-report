### Libraries -------------------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(nflverse)
library(ffscrapr)
library(sleeperapi)
library(stringi)
library(purrr)
library(stats)
library(scales)
library(gt)
library(gtExtras)
library(lamisc)
library(showtext)
library(sysfonts)
library(tvthemes)
library(patchwork)
# Connection --------------------------------------------------------------------
conn1 = ff_connect(platform = "sleeper", league_id = 591530379404427264, season = 2020)
conn2 = ff_connect(platform = "sleeper", league_id = 650064757319118848, season = 2021)
conn3 = ff_connect(platform = "sleeper", league_id = 785068521058115584, season = 2022)
conn4 = ff_connect(platform = "sleeper", league_id = 917507729030352896, season = 2023)

conn_temp <-  ff_template(scoring_type = "ppr",roster_type = "1qb")

players <- sleeper_players()
position_start <- ff_starter_positions(conn_temp)

# ROSTERS BY WEEK CLEAN UP AND LOAD --------------------------------------------
# Define a list with the details for each season
season_details <- list(
  list(conn = conn1, weeks = 1:17, season = 2020),
  list(conn = conn2, weeks = 1:18, season = 2021),
  list(conn = conn3, weeks = 1:18, season = 2022),
  list(conn = conn4, weeks = 1:18, season = 2023)
)


# Initialize an empty list to store starters data for each season
starters_list <- list()

# Loop over the season_details list to fetch and process starters data
for (i in seq_along(season_details)) {
  details <- season_details[[i]]
  starters_temp <- ff_starters(details$conn, week = details$weeks)
  starters_temp$season <- details$season
  starters_list[[i]] <- starters_temp
}

# Combine all starters data into a single dataframe
starters <- do.call(rbind, starters_list)

# Optionally, clean up the list of temporary dataframes if you want to free up memory
rm(starters_list)

starters$player_name <- ifelse(is.na(starters$player_name), starters$player_id, starters$player_name) 

scoring1 <- ff_scoringhistory(conn1, season = 2020)
scoring2 <- ff_scoringhistory(conn2, season = 2021)
scoring3 <- ff_scoringhistory(conn3, season = 2022)
scoring4 <- ff_scoringhistory(conn4, season = 2023)

scoring <- rbind(scoring1,scoring2,scoring3,scoring4) |>
  filter(pos != "LB" & pos != "DB" &  pos != "DL") |>
  select(season, week, sleeper_id, player_name, points, team)

starters_scoring <- starters |>
  left_join(scoring, by = c("season", "week", "player_id" = "sleeper_id", "player_name"))

starters_scoring$team.x <- ifelse(is.na(starters_scoring$team.x), starters_scoring$team.y, starters_scoring$team.x) 

starters_final <- starters_scoring |>
  select(-team.y) |>
  rename(team = team.x) |>
  select(season, week, franchise_id, franchise_name, player_id, player_name, pos, team, points, starter_status)

rm(scoring1,scoring2,scoring3,scoring4,starters_scoring)

starters_team <- unique(starters$franchise_name)

# FRANCHISES  --------------------------------------------
franchise1 <- ff_franchises(conn1)
franchise1$season <- 2020
franchise2 <- ff_franchises(conn2)
franchise2$season <- 2021
franchise3 <- ff_franchises(conn3)
franchise3$season <- 2022
franchise4 <- ff_franchises(conn4)
franchise4$season <- 2023
franchises <- rbind(franchise1,franchise2,franchise3,franchise4)

rm(franchise1,franchise2,franchise3,franchise4)

franchise_short <- franchises |>
  select(franchise_id, user_name, franchise_name, user_id, season) |>
  distinct(user_id, .keep_all = TRUE) 



# SCHEDULE CLEAN AND BIND --------------------------------------------

# Define a function to process schedule for a single year
process_schedule <- function(schedule, franchise_short, year) {
  schedule_1 <- schedule |>
    select(week, franchise_id, franchise_score, opp_score = opponent_score, opp_id = opponent_id, -c(result)) |>
    left_join(franchise_short, by =c("franchise_id" = "franchise_id")) |>
    select(week, tm_id = franchise_id, tm = user_name, tm_score = franchise_score, opp_score, opp_id) |>
    left_join(franchise_short, by =c("opp_id" = "franchise_id")) |>
    select(week, tm_id, tm, tm_score, opp_score, opp_id, opp = user_name)
  
  schedule_2 <- schedule |>
    select(week, tm_id = opponent_id, tm_score = opponent_score, opp_score = franchise_score, franchise_id) |>
    left_join(franchise_short, by =c("tm_id" = "franchise_id")) |>
    select(week, tm_id, tm = user_name, tm_score, opp_score, franchise_id) |>
    left_join(franchise_short, by =c("franchise_id" = "franchise_id")) |>
    select(week, tm_id, tm, tm_score, opp_score, opp_id = franchise_id, opp = user_name)
  
  final_schedule <- merge(schedule_1, schedule_2) |>
    arrange(week) |>
    mutate(result = ifelse(tm_score > opp_score, 1, 0),
           season = year)
  
  return(final_schedule)
}

# Assuming conn1, conn2, conn3, and conn4 are defined somewhere else
conns <- list(conn1, conn2, conn3, conn4)
years <- c(2020, 2021, 2022, 2023)

# Process each year's schedule and combine
listofgames <- map2_dfr(conns, years, ~process_schedule(ff_schedule(.x), franchise_short, .y))

# Continue with the rest of your code to finalize the full_schedule
full_schedule <- listofgames |>
  select(season, week:result) |>
  arrange(season, week, tm) |>
  mutate(win_loss = ifelse(result == 1, "W", "L"))


lookup <- c("nhosta" = "Hosta",
            "bellist" = "Tom",
            "tbellis" = "Tom",
            "pdpablo" = "Patrick",
            "zacgeoffray" = "Zac",
            "naaderbanki" = "Naad",
            "ttsao" = "Tommy",
            "elrandal" = "Randal",
            "Alanasty" = "JP",
            "JohnWickMD" = "Aviel",
            "slobonmynoblin" = "Logan",
            "asmontalvo" = "Montel",
            "cpmadden1" = "Conor",
            "patrickliou" = "Pat Liou")

full_schedule$tm <- sapply(full_schedule$tm, function(x) {
  if (x %in% names(lookup)) {
    return(gsub(x, lookup[x], x))
  } else {
    return(x)
  }
})

full_schedule$opp <- sapply(full_schedule$opp, function(x) {
  if (x %in% names(lookup)) {
    return(gsub(x, lookup[x], x))
  } else {
    return(x)
  }
})

playoff_tm_2020 <- c("Hosta","Patrick",
            "Tom","Zac","Tommy","JP")

playoff_tm_2021 <- c("Hosta","Patrick",
                    "Tom","Zac","Naad","Randy")

playoff_tm_2022 <- c("Hosta","Patrick",
                    "Tom","Zac","Tommy","Naad")

playoff_tm_2023 <- c("Zac","Aviel",
                     "Tommy","JP","Tom","Naad")

is_playoff_team <- function(season, week, team) {
  playoff_teams <- get(paste0("playoff_tm_", season))
  week_cutoff <- ifelse(season == 2020, 14, 15)
  week >= week_cutoff & team %in% playoff_teams
}

full_schedule <- full_schedule %>%
  mutate(
    season_type = case_when(
      (season == 2020 & week <= 13) | (season %in% c(2021, 2022, 2023) & week <= 14) ~ "Regular",
      (season == 2020 & week >= 14 & tm %in% playoff_tm_2020) |
        (season == 2021 & week >= 15 & tm %in% playoff_tm_2021) |
        (season == 2022 & week >= 15 & tm %in% playoff_tm_2022) |
        (season == 2023 & week >= 15 & tm %in% playoff_tm_2023) ~ "Playoffs",
      TRUE ~ "Consolation"
    )
  )


full_schedule <- full_schedule |>
  filter(season_type != "Consolation")

rm(listofgames,schedule_2020_final,schedule_2021_final,schedule_2022_final,schedule_2023_final,
   schedule4_1, schedule4_2, schedule4,
   schedule3_1, schedule3_2, schedule3,
   schedule2_1, schedule2_2, schedule2,
   schedule1_1, schedule1_2, schedule1)

write_csv(full_schedule, 'fullschedule.csv')

# STANDINGS ----------------

# Define a function to generate standings
generate_standings <- function(data, season_filter, title, subtitle, filename, special_tm = NULL, is_regular = TRUE) {
  season_type_filter <- if (is_regular) "Regular" else "Playoffs"
  
  # Apply conditional filtering before summarization
  if (!is.null(season_filter)) {
    data <- data |> filter(season == season_filter)
  }
  
  data <- data |> filter(season_type == season_type_filter)
  
  # Modify team names if needed before grouping and summarization
  if (!is.null(special_tm)) {
    data$tm <- ifelse(data$tm %in% special_tm, paste0(data$tm, " 🏆"), data$tm)
  }
  
  standings <- data |> 
    group_by(tm) |> 
    summarize(
      wins = sum(result),
      games = n(),
      pf = round(mean(tm_score), 1),
      pa = round(mean(opp_score), 1),
      .groups = 'drop'
    ) |> 
    mutate(
      losses = games - wins,
      win_per = percent(wins / games, accuracy = 0.1),
      winp = wins / games
    ) |> 
    arrange(-winp) |> 
    select(tm, wins, losses, win_per, pf, pa) |> 
    gt() |> 
    tab_header(title = md(title), subtitle = md(subtitle)) |> 
    gt_theme_espn() |> 
    cols_align("left", columns = vars(tm)) |> 
    tab_options(data_row.padding = px(3)) |> 
    tab_source_note(md("**Seasons: 2020-2023 | PF/PA Represent Avg/Gm**"))
  
  gtsave(standings, paste0('output/history/', filename))
}

#### Season-specific standings
seasons <- list(
  list(season = 2020, title = "**2020 Final Standings**", subtitle = "**Nicks Chubb's Golden Taint**", filename = "2020_standings.png", special_tm = "Hosta"),
  list(season = 2021, title = "**2021 Final Standings**", subtitle = "**Iron Banki Claims His Throne**", filename = "2021_standings.png", special_tm = "Naad"),
  list(season = 2022, title = "**2022 Season Standings**", subtitle = "**Tommy's Tainted Trophy**", filename = "2022_standings.png", special_tm = "Tommy"),
  list(season = 2023, title = "**2023 Season Standings**", subtitle = "**Tom Avoids Buffalo Futility**", filename = "2023_standings.png", special_tm = "Tom")
)

for (season_details in seasons) {
  generate_standings(full_schedule, season_details$season, season_details$title, season_details$subtitle, season_details$filename, special_tm = season_details$special_tm)
}

# All-time standings (Regular Season)
generate_standings(full_schedule, NULL, "**Dynasty Insanity**", "**All-Time Standings**", "all_time_standings.png", special_tm = c("Naad", "Hosta", "Tom", "Tommy"), is_regular = TRUE)

# All-time standings (Playoffs)
generate_standings(full_schedule, NULL, "**Dynasty Insanity**", "**All-Time Playoff Standings**", "all_time_post_standings.png", special_tm = c("Naad", "Hosta", "Tom", "Tommy"), is_regular = FALSE)


# DYNASTY -----------------

options(ffpros.cache = "memory") 
options(ffpros.cache = "filesystem")
options(ffpros.cache = "off")

conn4 = ff_connect(platform = "sleeper", league_id = 917507729030352896, season = 2023)
starters4 <- ff_rosters(conn4)|>
  select(-franchise_name)
starters4$season <- 2023
starters4$franchise_id <- as.integer(starters4$franchise_id)

dp <- dp_values("values-players.csv") |>
  select(fp_id,player:age,ecr_1qb,value_1qb) |>
  rename(
    dp_value = value_1qb,
    dp_ecr = ecr_1qb)

dp_draft <- dp_values("values-picks.csv") |>
  select(draftpick = player, pick, ecr_1qb)

ffpros <- ffpros::fp_rankings("dynasty-overall") |>
  rename(fp_id = fantasypros_id) |>
  select(fp_id:team, fp_rank = rank, fp_ecr = ecr, headshot = player_image_url, fp_posrank = pos_rank, fp_tier = tier) |>
  left_join(dp, by = c("fp_id" = "fp_id", "team" = "team", "pos")) |>
  relocate(headshot, .after = last_col()) |>
  select(fp_id:team, age, fp_rank:fp_tier, dp_ecr:dp_value, headshot)
  

ffpros$team <- gsub("JAC", "JAX", ffpros$team)
ffpros$player_name <- gsub("Kenneth Walker Iii", "Kenneth Walker", ffpros$player_name)
ffpros$player_name <- gsub("Kenneth Walker III", "Kenneth Walker", ffpros$player_name)
id <- dp_playerids()
id_map <- dp_playerids() |>
  select(fp_id = fantasypros_id, name, sleeper_id) |>
  mutate(name = stri_trans_totitle(gsub(",", " ", name)))

id_map$name <- gsub("Kenneth Walker Iii", "Kenneth Walker", id_map$name)
id_map$name <- gsub("Kenneth Walker III", "Kenneth Walker", id_map$name)

franchises_dynasty <- franchises |>
  select(season, user_id, franchise_id, franchise_name, user_name) |>
  distinct(season, user_id, .keep_all = TRUE)
franchises_team_dynasty <- franchises_dynasty |>
  select(season, franchise_id, user_name)

team_dynasty <- starters4 |>
  left_join(id_map, by = c("player_id" = "sleeper_id")) |>
  filter(pos != "DEF" & pos != "K")  |>
  relocate(fp_id) |>
  left_join(ffpros, by = c("fp_id")) |>
  select(season, franchise_id, fp_id, player_id, player_name = player_name.x, name = player_name.y, age = age.x, pos = pos.x, team = team.x, fp_rank:headshot) |>
  arrange(fp_tier, -dp_value) |>
  left_join(franchises_team_dynasty, by = c("season" = "season", "franchise_id" = "franchise_id")) |>
  relocate(user_name, .before = 2) 


lookup_2 <- c("nhosta" = "Hosta",
            "bellist" = "Tom",
            "pdpablo" = "Patrick",
            "zacgeoffray" = "Zac",
            "naaderbanki" = "Naad",
            "ttsao" = "Tommy",
            "elrandal" = "Randal",
            "Alanasty" = "JP",
            "JayPeeA" = "JP",
            "JohnWickMD" = "Aviel",
            "slobonmynoblin" = "Logan",
            "asmontalvo" = "Montel",
            "cpmadden1" = "Conor",
            "patrickliou" = "Pat Liou")

team_dynasty$team_name <- sapply(team_dynasty$user_name, function(x) {
  if (x %in% names(lookup_2)) {
    return(gsub(x, lookup[x], x))
  } else {
    return(x)
  }
})
team_dynasty$team_name <- ifelse(is.na(team_dynasty$team_name), "JP", team_dynasty$team_name)
team_dynasty$name <- ifelse(is.na(team_dynasty$team_name), team_dynasty$player_name, team_dynasty$name)
team_dynasty$player_name <- ifelse(is.na(team_dynasty$player_name), team_dynasty$name, team_dynasty$player_name)


playerprofiler <- read.csv('~/Desktop/fantasy-report/csv/playerprofiler.csv') |>
  select(name = Full.Name, team = Team.Abbrev, pp_posrank = Positional.Rank,  pp_lifetime = Lifetime.Value)

playerprofiler$name <- gsub("Kenneth Walker Iii", "Kenneth Walker", playerprofiler$name)
playerprofiler$name <- gsub("Kenneth Walker III", "Kenneth Walker", playerprofiler$name)

team_dynasty_rosters <- team_dynasty |>
  left_join(playerprofiler, by = c("player_name" = "name")) |>
  select(team_name,franchise_id:pos,team = team.x,fp_tier,fp_posrank,pp_posrank,pp_lifetime,fp_rank:fp_ecr,dp_ecr:dp_value,headshot) |>
  mutate(season = "2023") |>
  arrange(team_name, fp_tier, -pp_lifetime)


# TEAM ROSTER IMAGES --------------------

team_dynasty_rosters_df <- team_dynasty_rosters %>%
  select(headshot, team_name, name, team, pos, fp_tier, fp_rank, fp_ecr, playerprofiler = pp_lifetime, dynastypros = dp_value)

gt_theme_pff <- function(data, ...) {
  data %>%
    # add spanner for PFF Grade
    tab_spanner(
      label = "Season Rank",
      columns = contains(c("fp_tier", "fp_rank"))
    ) %>%
    # add spanner for SNAPS
    tab_spanner(
      label = "Dynasty Value",
      columns = contains(c("playerprofiler", "dynastypros"))
    ) %>%
    # Add a "blank" spanner to add white space
    tab_spanner(
      label = "BLANK",
      columns = 1:5
    ) %>%
    # Relabel columns
    cols_label(
      fp_tier = "Tier",
      fp_rank = "Rank",
      playerprofiler = "PlyrPro",
      dynastypros = "DynPro"
    ) %>%
    # if missing, replace NA w/ ---
    fmt_missing(
      columns = everything(),
      missing_text = "---"
    ) %>%
    # add exact color from PFF table to spanners
    tab_style(
      style = list(
        cell_fill(color = "#585d93"),
        cell_text(color = "white"),
        cell_borders(sides = "all", color = "#585d93", weight = px(1))
      ),
      locations = list(
        cells_column_spanners(
          spanners = c("BLANK", "Season Rank", "Dynasty Value")
        )
      )
    ) %>%
    # hide spanner with transparent color
    tab_style(
      style = list(
        cell_fill(color = "#585d93"),
        cell_text(color = "transparent")
      ),
      locations = list(
        cells_column_spanners(
          spanners = c("BLANK")
        )
      )
    ) %>%
    # Change font color and weight for numeric col
    tab_style(
      style = list(
        cell_text(color = "black", weight = "bold")
      ),
      locations = cells_body(
        columns = 1:9
      )
    ) %>%
    # Make column labels and spanners all caps
    opt_all_caps() %>%
    # add row striping
    opt_row_striping() %>%
    # change overall table styling for borders and striping
    tab_options(
      column_labels.background.color = "#585d93",
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
      row.striping.background_color = "#ededed",
      data_row.padding= px(4),

      ...
    ) %>% 
    # change font to Lato throughout (note no need to have Lato locally!)
    opt_table_font(
      font = c(
        google_font(name = "Roboto")
      )
    ) %>%
    # add source note
    tab_source_note(
      source_note = md("**PlyrPro:** PlayerProfiler.com  |  **DynPro:** DynastyProcess.com  |  **Season:** FantasyPros.com")
    )
}

showtext_auto()

create_user_table <- function(user_name1) {
  
  # Import font from sysfonts
  sysfonts::font_add_google("Roboto", "Roboto")
  showtext_auto()
  
  
  # Filter data for the specific user_name
  user_data <- team_dynasty_rosters_df %>%
    filter(team_name == user_name1) %>%
    arrange(pos, fp_rank) # arrange players by ECR
  
  # Define the number of players for each pos in the starting lineup
  starting_lineup <- bind_rows(
    user_data %>% filter(pos == "QB") %>% head(n = 1),
    user_data %>% filter(pos == "RB") %>% head(n = 2),
    user_data %>% filter(pos == "WR") %>% head(n = 2),
    user_data %>% filter(pos == "TE") %>% head(n = 1),
  )
  
  starting_lineup$roster <- "A-List"
  
  # Add the two highest ranked players from the "WR", "RB", "TE" positions to the starting lineup
  flex_players <- user_data %>%
    filter(!(name %in% starting_lineup$name)) %>%
    filter(pos %in% c("WR", "RB", "TE")) %>%
    head(n = 2)
  
  flex_players$roster <- "A-List"
  
  bench <- user_data %>%
    filter(!(name %in% starting_lineup$name)) %>%
    filter(!(name %in% flex_players$name)) %>%
    arrange(fp_rank) %>%
    head(n = 12)
  
  bench$roster <- "Bench"
  
  lineup <- rbind(starting_lineup, flex_players, bench)
  
  # Create the gt table and apply filter
  gt_table <- lineup %>%
    filter(team_name == user_name1) %>%
    group_by(roster) %>%
    select(headshot, name, team, pos, fp_tier, fp_rank, playerprofiler, dynastypros) %>%
    gt() %>%
    tab_header(title = md("**2023 Dynasty Roster**")) %>%
    gt_img_rows(headshot) %>% 
    fmt_number(
      columns = 5:9,
      decimals = 0
    ) %>% 
    gt_theme_pff()
  
  return(gt_table)
  
}

# Get unique user_names
unique_user_names <- unique(team_dynasty_rosters_df$team_name)


# Loop through each user_name, create the table, and save it as PNG
for (user_name in unique_user_names) {
  gt_table <- create_user_table(user_name)
  
  gtsave(gt_table,
         paste0("output/2023/v2024/dynasty_roster_", user_name,".png"))
}

# ROSTER IMAGES --------------------
df2 <- starters_final |>
  filter(season == 2022) |>
  left_join(franchise_short, by = c("franchise_id" = "franchise_id")) |>
  group_by(season, user_name, player_name, pos) |>
  summarize(total_pts = sum(points, na.rm = TRUE),
            avg_pts = mean(points, na.rm = TRUE))

df2_v2 <- starters_final |>
  filter(season == 2022) |>
  group_by(player_name, pos) |>
  summarize(total_pts = sum(points, na.rm = TRUE),
            avg_pts = mean(points, na.rm = TRUE)) |>
  arrange(-total_pts) |>
  rank_in_group2(group_var = pos, arrange_var = total_pts)

dataheadshot <- load_players() |>
  select(name = display_name, pos = position, team = team_abbr, headshot)

data <- df2_v2 |>
  left_join(df2, by = c("player_name", "pos")) |>
  select(user_name, name = player_name, pos, total_pts = total_pts.y, avg_pts = avg_pts.y, rank) |>
  left_join(dataheadshot, by = c("name" = "name", "pos" = "pos")) |>
  relocate(headshot, .before = 1)

gt_theme_538 <- function(data,...) {
  data %>%
    tab_spanner(
    label = "Player Scoring",
    columns = contains(c("rank", "total_pts", "avg_pts"))
  ) %>%
    # Add a "blank" spanner to add white space
    tab_spanner(
      label = "BLANK",
      columns = 1:4
    ) %>%
    # Relabel columns
    cols_label(
      rank = "Rank",
      total_pts = "Total",
      avg_pts = "Average") %>%
    # if missing, replace NA w/ ---
    fmt_missing(
      columns = everything(),
      missing_text = "---"
    ) %>%
    # add exact color from PFF table to spanners
    tab_style(
      style = list(
        cell_fill(color = "#585d93"),
        cell_text(color = "white"),
        cell_borders(sides = "all", color = "#585d93", weight = px(1))
      ),
      locations = list(
        cells_column_spanners(
          spanners = "Player Scoring")
        )
      ) %>%
    # hide spanner with transparent color
    tab_style(
      style = list(
        cell_fill(color = "transparent"),
        cell_text(color = "transparent")
      ),
      locations = list(
        cells_column_spanners(
          spanners = c("BLANK")
        )
      )
    ) %>%
    # Change font color and weight for numeric col
    tab_style(
      style = list(
        cell_text(color = "#585d93", weight = "bold")
      ),
      locations = cells_body(
        columns = 1:7
      )
    ) %>%
    # Make column labels and spanners all caps
    opt_all_caps() %>%
    # add row striping
    opt_row_striping() %>%
    # change overall table styling for borders and striping
    tab_options(
      column_labels.background.color = "#585d93",
      column_labels.font.size = px(16),
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
      row.striping.background_color = "#FFFDE9",
      data_row.padding= px(3),
      ...
    ) %>% 
    # change font to Lato throughout (note no need to have Lato locally!)
    opt_table_font(
      font = c(
        google_font(name = "Bebas Neue")
      )
    ) 
}

historical_table <- function(user_name1) {

  # Filter data for the specific user_name
  user_data <- data %>%
    filter(user_name == user_name1) %>%
    select(user_name, headshot, name, team, pos, rank, total_pts, avg_pts) %>%
    arrange(pos, -total_pts) # arrange players by Total Points
  
  # Define the number of players for each pos in the starting lineup
  starting_lineup <- bind_rows(
    user_data %>% filter(pos == "QB") %>% head(1),
    user_data %>% filter(pos == "RB") %>% head(2),
    user_data %>% filter(pos == "WR") %>% head(2),
    user_data %>% filter(pos == "TE") %>% head(1),
  )
  
  starting_lineup$roster <- "A-List"
  
  # Add the two highest ranked players from the "WR", "RB", "TE" positions to the starting lineup
  flex_players <- user_data %>%
    filter(!(name %in% starting_lineup$name)) %>%
    filter(pos %in% c("WR", "RB", "TE")) %>%
    head(n = 2)
  
  flex_players$roster <- "A-List"
  
  bench <- user_data %>%
    filter(!(name %in% starting_lineup$name)) %>%
    filter(!(name %in% flex_players$name)) %>%
    filter(!pos == "K" & !pos == "DEF") %>%
    arrange(-total_pts) %>%
    head(n = 10)
  
  bench$roster <- "Bench"
  
  lineup <- rbind(starting_lineup, flex_players, bench)
  
  tab_538 <- lineup %>%
    filter(user_name == user_name1) %>%
    group_by(roster) %>%
    select(-user_name) %>%
    gt() %>%
    tab_header(title = md("**2022 Dynasty Roster**"),
               subtitle = "Ranked by Points Scored") %>%
    gt_img_rows(headshot) %>% 
    cols_label(
      rank = "Rank"
    ) %>% 
    tab_source_note(
      source_note = md("**Bench**: Top-10 Only")
    ) %>% 
    fmt_number(
      columns = 5:6,
      decimals = 0
    ) %>% 
    fmt_number(
      columns = 7,
      decimals = 1
    ) %>% 
    gt_theme_538()
  
  return(tab_538)
  
}

# Get unique user_names
unique_user_names <- unique(franchise_short$user_name)


# Loop through each user_name, create the table, and save it as PNG
for(user_name in unique_user_names) {
  gt_table <- historical_table(user_name)
  
  gtsave(gt_table, paste0("output/2022/dynasty_roster_",user_name,".png"))
}

# Head to Head -------------

head2head <- full_schedule |>
  filter(season_type != "Consolation") |>
  mutate(margin = tm_score - opp_score) |>
  group_by(tm, opp) |>
  mutate(
    games = length(win_loss),
    wins = sum(result),
    losses = games - wins,
    winperc = percent(wins / games),
    pf = sum(tm_score),
    pa = sum(opp_score),
    margin = sum(margin),
    avg_mrg = margin / games) |>
  ungroup()

head2head$avg_mrg <- round(head2head$avg_mrg, 1)

df <- head2head |>
  select(tm, opp, wins, losses, winperc, pf, pa, avg_mrg) |>
  ungroup()

gt_theme_schedule <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Bebas Neue"),
        default_fonts()
      ),
      weight = "normal"
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "all", color = "#585d93", weight = px(2)
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      )
    ) %>% 
    # change overall table styling for borders and striping
    tab_options(
      column_labels.background.color = "#585d93",
      column_labels.font.size = px(16),
      heading.border.bottom.width = px(2),
      heading.border.bottom.color = "#585d93",
      heading.border.lr.width = px(2),
      heading.border.lr.color =  "#585d93",
      table_body.hlines.color = "#585d93",
      table.border.top.width = px(2),
      table.border.top.color = "#585d93",
      table.border.bottom.color = "#585d93",
      table.border.bottom.width = px(2),
      column_labels.border.top.width = px(2),
      column_labels.border.top.color = "#585d93",
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "#585d93",
      row_group.font.size = px(14),
      row.striping.background_color = "#585d93",
      data_row.padding= px(2),
      ...
    ) 
} 

create_headtohead <- function(tm1) {
  
  # Import font from sysfonts
  sysfonts::font_add_google("Bebas Neue", "Bebas Neue")
  showtext_auto()
  
  
  # Filter data for the specific user_name
  user_data <- df %>%
    filter(tm == tm1) %>%
    arrange(-wins, -avg_mrg) # arrange players by ECR
  
  # Create the gt table and apply filter
  gt_table <- user_data %>%
    select(-tm, opp, wins, losses, percent = winperc, pf, pa, avg_mrg) %>%
    gt() %>%
    tab_header(title = md("**All-Time Results (Head to Head)**")) |>
    fmt_number(
      columns = 4:6,
      decimals = 0
    ) %>% 
    gt_theme_schedule()
  
  return(gt_table)
  
}

unique_tms <- unique(df$tm)

for (tm in unique_tms) {
  gt_table <- create_headtohead(tm)
  
  gtsave(gt_table,
         paste0("output/headtohead/", tm, "_head_to_head.png"))
}


# SCHEDULES -------------------

seasonrecap <- full_schedule |>
  filter(season_type != "Consolation") 

get_season_recap <- function(tm1, year) {
  # Import font from sysfonts
  sysfonts::font_add_google("Bebas Neue", "Bebas Neue")
  showtext_auto()
  
  # Filter data for the specific user_name and year
  user_data <- full_schedule %>%
    filter(season_type != "Consolation", season == year, tm == tm1) %>%
    arrange(week) %>%
    select(week, opponent = opp, win_loss, pf = tm_score, pa = opp_score, result) %>%
    mutate(margin = pf - pa)
  
  wins <- sum(user_data$result, na.rm = TRUE)
  losses <- nrow(user_data) - wins
  
  user_data$streak <- ave(user_data$result, cumsum(user_data$result == 0), FUN = seq_along) - 1
  
  gt_table <- user_data %>%
    select(week, opponent, win_loss, pf, pa, margin, streak) %>%
    gt() %>%
    gt_highlight_rows(rows = streak > 0 | margin >= 0, fill = "#FFFDE9", bold_target_only = FALSE) %>%
    tab_header(title = paste(year, "Schedule")) %>%
    fmt_number(columns = vars(pf, pa, margin), decimals = 1) %>% 
    cols_align(align = "center", columns = vars(win_loss, pf, pa, margin, streak)) %>%
    gt_theme_schedule() # Ensure this function is defined
  
  return(gt_table)
}

unique_tms <- unique(seasonrecap$tm)

years <- c(2020, 2021, 2022, 2023)

for (year in years) {
  # Filter teams for the current year
  unique_tms <- unique(full_schedule$tm[full_schedule$season == year])
  
  for (tm in unique_tms) {
    gt_table <- get_season_recap(tm, year)
    
    # Define a dynamic file path based on both team and year
    file_path <- paste0("output/py_schedule/", year, "_season_schedule_", gsub(" ", "_", tm), ".png")
    gtsave(gt_table, file_path)
  }
}



# BLOWOUTS  ----------
blowouts <- full_schedule |>
  filter(season_type != "Consolation") |>
  mutate(margin = tm_score - opp_score) |>
  arrange(-margin) |>
  head(15)

# GT TABLE ----
gt_table <- blowouts %>%
  select(season, week, victor = tm, loser = opp, pf = tm_score, pa = opp_score, margin) %>%
  gt() %>%
  tab_header(title = md("**Top-15 Blowouts**")) %>%
  fmt_number(
    columns = 5:7,
    decimals = 1
  ) %>% 
  cols_align(
    align = "center",
    columns = 1:2
  ) %>% 
  cols_align(
    align = "center",
    columns = 5:7
  ) %>%
  gt_theme_schedule()

gtsave(gt_table,"output/history/blowouts.png")

# NAIL BITERS ----------
closecalls <- full_schedule |>
  filter(season_type != "Consolation") |>
  mutate(margin = tm_score - opp_score) |>
  filter(margin > 0) |>
  arrange(margin) |>
  head(15)


# Create the gt table and apply filter
gt_table <- closecalls %>%
  select(season, week, victor = tm, loser = opp, pf = tm_score, pa = opp_score, margin) %>%
  gt() %>%
  tab_header(title = md("**Top-15 Close Calls**")) %>%
  fmt_number(
    columns = 5:7,
    decimals = 2
  ) %>% 
  cols_align(
    align = "center",
    columns = 1:2
  ) %>% 
  cols_align(
    align = "center",
    columns = 5:7
  ) %>%
  gt_theme_schedule()

gtsave(gt_table,"output/history/closecalls.png")



# POSTSEASON H2H -------------

head2head2 <- full_schedule |>
  filter(season_type == "Playoffs") |>
  mutate(margin = tm_score - opp_score) |>
  group_by(tm, opp) |>
  mutate(games = length(win_loss),
            wins = sum(result),
            losses = games - wins,
            winperc = percent(wins / games),
            pf = sum(tm_score),
            pa = sum(opp_score),
            margin = sum(margin),
            avg_mrg = margin / games) |>
  ungroup()

head2head2$avg_mrg <- round(head2head2$avg_mrg, 1)

df <- head2head2 |>
  select(tm, opp, wins, losses, winperc, pf, pa, avg_mrg) |>
  ungroup()

gt_theme_schedule <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Bebas Neue"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "all", color = "#585d93", weight = px(1)
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      )
    ) %>% 
    tab_options(
      column_labels.background.color = "#585d93",
      column_labels.font.size = px(16),
      heading.border.bottom.width = px(2),
      heading.border.bottom.color = "#585d93",
      heading.border.lr.width = px(2),
      heading.border.lr.color =  "#585d93",
      table_body.hlines.color = "#585d93",
      table.border.top.width = px(2),
      table.border.top.color = "#585d93",
      table.border.bottom.color = "#585d93",
      table.border.bottom.width = px(2),
      column_labels.border.top.width = px(2),
      column_labels.border.top.color = "#585d93",
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "#585d93",
      row_group.font.size = px(14),
      row.striping.background_color = "#585d93",
      data_row.padding= px(3),
      ...
    ) 
} 

create_headtohead2 <- function(tm1) {
  
  # Import font from sysfonts
  sysfonts::font_add_google("Bebas Neue", "Bebas Neue")
  showtext_auto()
  
  
  # Filter data for the specific user_name
  user_data <- df %>%
    filter(tm == tm1) %>%
    arrange(-wins, -avg_mrg) # arrange players by ECR
  
  # Create the gt table and apply filter
  gt_table <- user_data %>%
    select(-tm, opp, wins, losses, percent = winperc, pf, pa, avg_mrg) %>%
    gt() %>%
    tab_header(title = md("**All-Time Results (Head to Head)**")) |>
    fmt_number(
      columns = 4:5,
      decimals = 0
    ) %>% 
    gt_theme_schedule()
  
  return(gt_table)
  
}

unique_tms <- unique(df$tm)

for (tm in unique_tms) {
  gt_table <- create_headtohead2(tm)
  
  gtsave(gt_table,
         paste0("output/headtohead/post/", tm, "_head_to_head.png"))
}

# POWER RANKS ---------
calculate_power_rankings <- function(season_data, season_year) {
  # Calculate power rankings
  power_rankings <- season_data %>%
    filter(season == season_year, season_type != "Consolation") %>%
    group_by(tm, season) %>%
    summarize(
      wins = sum(result),
      games = n(),
      pf = mean(tm_score),
      max_pf = max(tm_score),
      min_pf = min(tm_score),
      .groups = 'drop'
    ) %>%
    mutate(
      losses = games - wins,
      winp = wins / games,
      adj_winp = winp * 200,
      adj_pf = pf * 6,
      adj_variance = (max_pf + min_pf) / 2,
      power_rank = round((adj_winp + adj_pf + adj_variance) / 10, 2)
    )
  
  # Calculate league average
  league_average <- mean(power_rankings$power_rank)
  
  # Adjust power rankings by league average
  power_rankings <- power_rankings %>%
    mutate(adj_power_ranking = round(power_rank / league_average, 4)) %>%
    select(team = tm, season, power = adj_power_ranking) %>%
    arrange(-power)
  
  return(power_rankings)
}

generate_and_save_table <- function(power_rankings, season_year, file_path) {
  gt_table <- power_rankings %>%
    gt() %>%
    tab_header(title = md(paste(season_year, "Power Rankings")),
               subtitle = "Adj by League Season Avg") %>%
    gt_theme_schedule() %>%
    cols_align("left", columns = vars(team)) %>%
    tab_options(data_row.padding = px(3)) %>%
    tab_source_note(md("**Calc**: 60% Avg PF, 20% Win %, 20% PF Variance"))
  
  gtsave(gt_table, file_path)
}


season_years <- unique(full_schedule$season)

for (season_year in season_years) {
  power_rankings <- calculate_power_rankings(full_schedule, season_year)
  file_path <- paste0('output/history/power_rank_standings_yearly_', season_year, '.png')
  generate_and_save_table(power_rankings, season_year, file_path)
}
######## vs Median Score Each Week -------------
library(gt)
library(ggplot2)
library(dplyr)
library(scales) # For percent()

median_schedule <- full_schedule |>
  filter(season_type == "Regular") |>
  group_by(season, week) |> 
  mutate(
    median_score = median(tm_score), 
  ) |> 
  ungroup() |> 
  mutate(
    median_result = ifelse(tm_score > median_score, 1, 0)
  ) |>
  mutate(
    total_wins = result + median_result
  ) 

median_standings <- median_schedule |>
  group_by(season, tm) |>
  mutate(
    actual_wins = sum(result),
    potential_wins = sum(result, median_result)
  ) |>
  ungroup()

generate_gt_table <- function(data, season) {
  title_text <- ifelse(season == "2020:2023", "2020-2023 Season Standings w/ League Avg Game", paste(season, "Season Standings w/ League Avg Game"))
  file_name <- ifelse(season == "2020:2023", "2020-23_standings_median.png", paste(season, "_standings_median.png", sep=""))
  
  gt_table <- data %>%
    filter(season_type == "Regular", ifelse(season == "2020:2023", TRUE, season == as.numeric(season))) %>%
    group_by(tm) %>%
    summarize(
      games = n(),
      wins = sum(result),
      pot_wins = sum(result, median_result),
      .groups = 'drop'
    ) %>%
    mutate(
      wp_act = percent(wins / games, accuracy = 0.1),
      wp_pot = percent(pot_wins / (games * 2), accuracy = 0.1) # Adjusted calculation
    ) %>%
    arrange(-pot_wins) %>%
    select(tm, wins, pot_wins, wp_act, wp_pot) %>%
    gt() %>%
    tab_header(title = md(title_text)) %>%
    gt_theme_nytimes() %>%
    cols_align("left", columns = vars(tm)) %>%
    tab_options(data_row.padding = px(1)) %>%
    tab_source_note(paste0("Season: ", season, " | PF/PA Represent Avg/Gm"))
  
  gtsave(gt_table, paste0('output/history/', file_name))
}


seasons <- c(2020, 2021, 2022, 2023, "2020:2023")

for (season in seasons) {
  generate_gt_table(median_standings, season)
}


##### Draft History --------------

franchise_draft <- franchises |>
  select(user_id, franchise_id) |>
  distinct(user_id, .keep_all = TRUE) |>
  left_join(franchise_short, by = c("user_id", "franchise_id")) |>
  select(user_id, user_name)


league_ids <- c("591530380872433664", "650064757319118849", "785068521058115585", "1003817666127179776")

draft_lookup <- c("591530380872433664" = "2020",
                  "650064757319118849" = "2021",
                  "785068521058115585" = "2022",
                  "1003817666127179776" = "2023")

draft_list <- list()

for (i in seq_along(league_ids)) {
  league_id <- league_ids[i]
  
  # Fetch draft picks for the current league_id
  year_drafts <- get_draft_picks(league_id)
  
  # Store in the draft_list, using league_id as key (or consider using year if more appropriate)
  draft_list[[as.character(league_id)]] <- year_drafts
}

# Combine all drafts into a single data frame
drafts_combined <- dplyr::bind_rows(draft_list) 

drafts_combined <- drafts_combined |>
  left_join(franchise_draft, by = c("picked_by" = "user_id"))


drafts_combined <- drafts_combined |>
  mutate(full_name = paste(metadata$first_name, metadata$last_name)) 

draftday <- drafts_combined|>
  select(draft_id, user_name, round, pick_no, full_name) 

draftday$year <- sapply(draftday$draft_id, function(x) {
  if (x %in% names(draft_lookup)) {
    return(gsub(x, draft_lookup[x], x))
  } else {
    return(x)
  }
})

draft_history <- draftday |>
  select(year, user_name:full_name) |>
  arrange(year, round, pick_no)

rm(draftday,drafts_combined,franchise_draft)


##### Draft Picks

draft_assets <- ff_draftpicks(platform = "sleeper", conn4)


draft_assets_transformed <- draft_assets %>%
  mutate(player_name = paste("Draft Pick:", round),
         pos = "PICK", # Indicate draft pick
         age = NA, # No age for draft picks
         fp_posrank = NA,
         pp_lifetime = NA) %>% # No FP rank for draft picks
  select(franchise_id, season, player_name, pos, age, fp_posrank, pp_lifetime)
  


# Assuming the current season for team_dynasty_rosters is 2023
team_dynasty_simplified <- team_dynasty_rosters %>%
  select(franchise_id, season, player_name, pos, age, fp_posrank, pp_lifetime)

team_dynasty_simplified <- team_dynasty_simplified |>
  mutate(pos = factor(pos, levels = c("QB", "RB", "WR", "TE"))) %>%
  arrange(franchise_id, pos, -pp_lifetime)

full_roster_with_draft_picks <- bind_rows(team_dynasty_simplified, draft_assets_transformed) |>
  left_join(franchise_short, by = "franchise_id") |>
  select(user_name, season:pp_lifetime)

user_names <- unique(franchise_short$user_name)

for (name in user_names) {
  
  sysfonts::font_add_google("Bebas Neue", "Bebas Neue")
  showtext_auto()
  
  # Filter the data for the current franchise_id
  team_data <- full_roster_with_draft_picks %>%
    filter(user_name == name)
  
  # Create and display a gt table for the current team
  team_data %>%
  
    gt() %>%
    tab_header(title = paste("2024 Full Roster with Draft Assets -:", name),
                             subtitle = "FP Rank: FantasyPros, Career Value: PlayerProfiler") %>%
    cols_label(
      player_name = "Player/Draft Pick",
      pos = "Position",
      age = "Age",
      fp_posrank = "Pos Rank",
      pp_lifetime = "Career Value"
    ) %>%
    sub_missing(columns = 5:7, 
                      missing_text = "-") %>%
    cols_hide(columns = vars(user_name, season)) %>%
    tab_options(
      table.font.names = "Bebas Neue"
    ) %>%
    cols_align(align = "right", columns = player_name) %>% # Right-align the first column
    cols_align(align = "center", columns = 2:7) %>%
    gt_theme_schedule() %>%
    gtsave(filename = paste("output/2024/Franchise_Assets_", name, ".png")) # Saving the table as an HTML file
  
  cat("\n") # Add a newline for better separation when displaying multiple tables
}


##### TRADES ---------------

trades1 <- ff_transactions(conn1, weeks = 1:17) |>
  filter(type == "trade") |>
  select(-waiver_priority)
trades1$season <- 2020

trades2 <- ff_transactions(conn2, weeks = 1:18) |>
  filter(type == "trade")
trades2$season <- 2021

trades3 <- ff_transactions(conn3,weeks = 1:18) |>
  filter(type == "trade") |>
  select(-waiver_priority)
trades3$season <- 2022

trades4 <- ff_transactions(conn4,weeks = 1:18) |>
  filter(type == "trade") |>
  select(-waiver_priority)
trades4$season <- 2023
trades <- rbind(trades1,trades2,trades3,trades4)

trades <- trades |>
  mutate(franchise_id = as.integer(franchise_id),
         trade_partner = as.integer(trade_partner))


trades$player_name <- ifelse(is.na(trades$player_name), trades$player_id, trades$player_name)

trades$player_name <- gsub("_pick_from_franchise_[0-9]+", "", trades$player_name) # Remove the pick part
trades$player_name  <- gsub("_", " ", trades$player_name ) # Replace underscores with spaces


trades <- trades |>
  mutate(pos = replace_na(pos, "Pick")) |>
  mutate(franchise_id = as.integer(franchise_id)) |>
  mutate(trade_partner = as.integer(trade_partner)) 



# Assuming your data frame is named trades_df
traded_away_pre <- filter(trades, type_desc == "traded_away")
traded_for_pre <- filter(trades, type_desc == "traded_for")

traded_away <- filter(trades, type_desc == "traded_away") |>
  select(season, timestamp, team_1 = franchise_id, team_1_name = franchise_name, traded_away = player_name, traded_away_pos = pos, trade_partner_1 = trade_partner) |>
  group_by(season, timestamp, team_1, trade_partner_1) %>%
  summarise(players_traded_away = toString(traded_away), .groups = 'drop') 

traded_for <- filter(trades, type_desc == "traded_for") |>
  select(season, timestamp, team_2 = franchise_id, team_2_name = franchise_name, traded_for = player_name, traded_for_pos = pos, trade_partner_2= trade_partner) |>
  group_by(season, timestamp, team_2, trade_partner_2) %>%
  summarise(players_traded_for = toString(traded_for), .groups = 'drop') 

trades_combined <- traded_away %>%
  left_join(traded_for, by = c("season", "timestamp", "team_1" = 'team_2', "trade_partner_1" = "trade_partner_2")) |>
  mutate(timestamp = format(timestamp, "%m/%d/%y"))



trade_df <- readxl::read_excel('trades.xlsx')

trade_team_list <- franchises_team_dynasty |>
  distinct(user_name, .keep_all = TRUE)

trade_team_list$user_name <- sapply(trade_team_list$user_name, function(x) {
  if (x %in% names(lookup_2)) {
    return(gsub(x, lookup[x], x))
  } else {
    return(x)
  }
})


# Your lookup vector
lookup_3 <- c("8" = "Hosta",
              "1" = "Tom",
              "5" = "Patrick",
              "10" = "Zac",
              "9" = "Naad",
              "3" = "Tommy",
              "12" = "Randal",
              "4" = "JP",
              "6" = "Aviel",
              "2" = "Montel",
              "7" = "Conor")

# Convert your lookup vector to a data frame for easier handling
lookup_df <- data.frame(franchise_id = names(lookup_3), team_name = unname(lookup_3), stringsAsFactors = FALSE)
lookup_df$franchise_id <- as.character(lookup_df$franchise_id) # Ensure the key column is of the same type
lookup_df$season <- 2024

trades_final <- trades_combined %>%
  mutate(
    franchise_id = as.integer(team_1),
    trade_partner = as.integer(trade_partner_1),
    season = as.double(season)) |>
  select(season, date = timestamp, franchise_id, trade_partner, players_traded_away, players_traded_for)

trade_df <- trades_final %>%
  mutate(
    franchise_id = as.character(franchise_id),
    trade_partner = as.character(trade_partner))



trade_df3 <- trade_df |>
  left_join(lookup_df, by = c("franchise_id" = "franchise_id")) |>
  left_join(lookup_df, by = c("trade_partner" = "franchise_id")) |>
  select(season = season.x, date, franchise_id, team_name = team_name.x, trade_partner_id = trade_partner, trade_partner = team_name.y, players_traded_away,players_traded_for)

# Assuming trade_df is your data frame
trade_df4 <- trade_df3 %>%
  mutate(
    team_name = case_when(
      season == 2020 & franchise_id == "11" ~ "Logan",
      season == 2020 & is.na(franchise_id) ~ "Logan",
      season == 2021 & franchise_id == "11" ~ "Logan",
      season == 2021 & is.na(franchise_id) ~ "Logan",
      season == 2022 & is.na(franchise_id) ~ "Logan",
      season == 2022 & franchise_id == "11" ~ "Logan",
      season == 2023 & franchise_id == "11" ~ "Pat L",
      franchise_id == NA ~ "Logan",# Specify the condition and the new value
      TRUE ~ team_name  # For all other cases, keep the original season value
    )) |>
      mutate(
        trade_partner = case_when(
          season == 2020 & trade_partner_id == "11" ~ "Logan",
          season == 2020 & is.na(trade_partner_id) ~ "Logan",
          season == 2021 & trade_partner_id == "11" ~ "Logan",
          season == 2021 & is.na(trade_partner_id) ~ "Logan",
          season == 2022 & is.na(trade_partner_id) ~ "Logan",
          season == 2022 & trade_partner_id == "11" ~ "Logan",
          season == 2023 & trade_partner_id == "11" ~ "Pat L",# Specify the condition and the new value
          TRUE ~ trade_partner  # For all other cases, keep the original season value
        )
      ) |>
  select(season, date, team_name, trade_partner, players_traded_away, players_traded_for) |>
  arrange(season, date, team_name, trade_partner)

unique_clubs <- unique(trade_df4$team_name)

write_csv(trade_df4, 'trades_final.csv')

# Assuming the necessary libraries are loaded
library(dplyr)
library(gt)


unique_clubs <- unique(trade_df4$team_name)

generate_gt_trade <- function(club) {
  # Filter the data for the specified club
  gt_table <- trade_df4 %>%
    filter(team_name == club) %>%
    arrange(season, date) %>%
    mutate(
      players_traded_for = gsub(",", "<br>", players_traded_for, fixed = TRUE),
      players_traded_away = gsub(",", "<br>", players_traded_away, fixed = TRUE)
    )
  
  
  # Import font from sysfonts
  sysfonts::font_add_google("Bebas Neue", "Bebas Neue")
  showtext_auto()
  
  # Start building the gt table
  gt_tbl <- gt_table |>
    dplyr::select(season:team_name, team = trade_partner, received = players_traded_for, traded = players_traded_away) |>
    gt() |>
    cols_hide(columns = c(season, team_name)) |>
    cols_width(
      date ~ px(80),
      team ~ px(80)) |>
    opt_all_caps() |>
    tab_header(
      title = paste(club, "Trades by Season"),
      subtitle = "2020 - 2023"
    )
  
  # Dynamically add row groups based on available seasons in the filtered data
  for (year in c("2020", "2021", "2022", "2023")) {
    if (year %in% gt_table$season) {
      gt_tbl <- gt_tbl |>
        tab_row_group(label = year, rows = season == year)
    }
  }
  
  # Apply markdown formatting to allow line breaks
  gt_tbl <- gt_tbl %>%
    fmt_markdown(columns = vars(received, traded))
  
  # Continue with the rest of the gt table styling and options
  gt_tbl <- gt_tbl |>
    tab_style(
      style = cell_text(align = "center", style = "normal", weight = "normal", stretch = "extra-condensed", transform = "uppercase"),
      locations = cells_body(columns = c(received, traded))) |>
    tab_style(
      style = cell_text(align = "right", style = "normal", weight = "bold", stretch = "condensed", transform = "uppercase"),
      locations = cells_body(columns = c(date, team))) 
    # Apply border styling separately
  gt_tbl <- gt_tbl |>
    tab_style(
      style = cell_borders(c("top", "bottom"), color = "lightgrey", style = "solid", weight = px(1.5)),
      locations = cells_body(columns = c(date, team, received, traded))
    )
  gt_tbl <- gt_tbl |>
    tab_style(
      style = cell_text(color = "black", align = "center", style = "normal", weight = "bolder", stretch = "extra-expanded", decorate = "underline", transform = "uppercase"),
      locations = cells_title(c("title", "subtitle"))
    ) |>
    tab_style(
      style = cell_text(color = "black", align = "center", style = "normal", weight = "bolder", stretch = "expanded", transform = "uppercase"),
      locations = cells_column_labels(columns = everything())
    ) |>
    tab_style(
      style = list(cell_fill(color = "#FEFEE7"), cell_text(color = "black", size = "medium", style = "normal", weight = "bolder", stretch = "extra-expanded", transform = "uppercase")),
      locations = cells_row_groups()
    ) |>
    tab_options(
      table.align = "center",
      heading.align = "center",
      heading.background.color = "#ffdc73",
      heading.title.font.size = px(20),
      heading.title.font.weight = "bolder",
      column_labels.font.size = px(16),
      column_labels.font.weight = "bold",
      column_labels.background.color = "#ffdc73",
      table_body.hlines.color = "transparent",
      table.border.top.width = px(2),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(2),
      column_labels.border.top.width = px(2),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "transparent"
    ) 
  
  return(gt_tbl)
}

# Loop over each unique club and generate the gt table
for (club in unique_clubs) {
  p <- generate_gt_trade(club)
  # Assuming the path exists and permissions are set correctly
  gtsave(p, filename = paste0("output/history/Franchise_Trades_", club, ".png")) # Adjusted to correctly save the table
}


# OPTIMAL??? ============
optimal_lineup_df <- do.call(rbind, optimal_lineups)

lineup_optimal <- optimal_lineup_df |>
  select(season, week, roster_version, franchise_id, franchise_name, player_name, pos, team, points, starter_status)

lineup_analysis <- rbind(plyrscore_started, lineup_optimal) |>
  arrange(season, week, franchise_id, roster_version)



# Summing up points for started players
started_points_summary <- plyrscore_started %>%
  group_by(season, week, franchise_name) %>%
  summarise(started_points_total = sum(points, na.rm = TRUE))

# Summing up points for optimal lineup players
optimal_points_summary <- lineup_optimal %>%
  group_by(season, week, franchise_name) %>%
  summarise(optimal_points_total = sum(points, na.rm = TRUE))

# Calculating point difference
point_diff_df <- left_join(started_points_summary, optimal_points_summary, 
                           by = c("season", "week", "franchise_name")) %>%
  mutate(point_diff = round(started_points_total - optimal_points_total, 2)) %>%
  select(season, week, franchise_name, point_diff)

# View the result
print(point_diff_df)

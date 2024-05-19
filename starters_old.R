### Libraries -------------------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(ffscrapr)
library(purrr)
library(gt)
library(gtExtras)
library(scales)
library(stringi)
library(showtext)
library(nflverse)
library(writexl)
library(readxl)
# Options -------------------------------------------------------------------------
options(ffpros.cache = "memory") 
ffscrapr::clear_cache()
# Connection --------------------------------------------------------------------
league_ids <- c(591530379404427264, 650064757319118848, 785068521058115584, 917507729030352896)
seasons <- 2020:2023

# Connection --------------------------------------------------------------------
conn1 = ff_connect(platform = "sleeper", league_id = 591530379404427264, season = 2020)
conn2 = ff_connect(platform = "sleeper", league_id = 650064757319118848, season = 2021)
conn3 = ff_connect(platform = "sleeper", league_id = 785068521058115584, season = 2022)
conn4 = ff_connect(platform = "sleeper", league_id = 917507729030352896, season = 2023)
conn5 = ff_connect(platform = "sleeper", league_id = 1048425511736713216, season = 2024)

# Create connections for each league and season
connections <- mapply(function(id, season) ff_connect(platform = "sleeper", league_id = id, season = season), league_ids, seasons, SIMPLIFY = FALSE)

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


# Function to fetch and process scoring history for a season
fetch_scoring <- function(conn, season) {
  ff_scoringhistory(conn, season = season)
}

# Combine scoring data for all seasons
scoring <- purrr::map2_df(connections, seasons, ~fetch_scoring(.x, .y)) %>%
  filter(pos != "LB" & pos != "DB" & pos != "DL") %>%
  select(season, week, sleeper_id, player_name, points, team)

# Join starters and scoring data
starters_scoring <- starters %>%
  left_join(scoring, by = c("season", "week", "player_id" = "sleeper_id", "player_name"))

# Update team information and handle duplicates
starters_scoring <- starters_scoring %>%
  mutate(team = coalesce(team.x, team.y)) %>%
  select(-team.x, -team.y)

starters_final <- starters_scoring %>%
  select(season, week, franchise_id, franchise_name, player_id, player_name, pos, team, points, starter_status)

# Extract unique team names
starters_team <- unique(starters$franchise_name)

# Clean up intermediate variables to free memory
rm(players, position_start, scoring, starters_scoring)

# Fetch franchises for each season and combine into a single dataframe
fetch_franchises <- function(conn, season) {
  franchises <- ff_franchises(conn)
  franchises$season <- season
  return(franchises)
}

# Combine franchise data for all seasons
franchises <- purrr::map2_df(connections, seasons, fetch_franchises)

# Select relevant columns and remove duplicates
franchise_short <- franchises %>%
  select(franchise_id, user_name, franchise_name, user_id, season) %>%
  distinct(user_id, season, .keep_all = TRUE)



process_schedule <- function(schedule, franchise_short, year) {
  
  # Filter the franchise_short for the specific year
  franchise_short_season <- franchise_short %>%
    filter(season == year)
  
  schedule_1 <- schedule %>%
    select(week, franchise_id, franchise_score, opp_score = opponent_score, opp_id = opponent_id) %>%
    left_join(franchise_short_season, by = c("franchise_id" = "franchise_id")) %>%
    select(week, tm_id = franchise_id, tm = user_name, tm_score = franchise_score, opp_score, opp_id) %>%
    left_join(franchise_short_season, by = c("opp_id" = "franchise_id")) %>%
    select(week, tm_id, tm, tm_score, opp_score, opp_id, opp = user_name)
  
  schedule_2 <- schedule %>%
    select(week, tm_id = opponent_id, tm_score = opponent_score, opp_score = franchise_score, franchise_id) %>%
    left_join(franchise_short_season, by = c("tm_id" = "franchise_id")) %>%
    select(week, tm_id, tm = user_name, tm_score, opp_score, franchise_id) %>%
    left_join(franchise_short_season, by = c("franchise_id" = "franchise_id")) %>%
    select(week, tm_id, tm, tm_score, opp_score, opp_id = franchise_id, opp = user_name)
  
  final_schedule <- bind_rows(schedule_1, schedule_2) %>%
    arrange(week) %>%
    mutate(result = ifelse(tm_score > opp_score, 1, 0),
           season = year)
  
  return(final_schedule)
}

# Assuming conn1, conn2, conn3, and conn4 are defined somewhere else
conns <- list(conn1, conn2, conn3, conn4)
years <- c(2020, 2021, 2022, 2023)

# Process each year's schedule and combine
listofgames <- map2_dfr(conns, years, ~process_schedule(ff_schedule(.x), franchise_short, .y))

listofgames <- listofgames |>
  select(season, week:result) |>
  arrange(season, week)

# Continue with the rest of your code to finalize the full_schedule
full_schedule <- listofgames |>
  select(season, week:result) |>
  arrange(season, week, tm) |>
  mutate(win_loss = ifelse(result == 1, "W", "L")) |>
  distinct()


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
            "patrickliou" = "Pat Liou",
            "JayPeeA" = "JP")

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


# Function to generate standings
generate_standings <- function(data, season_filter, title, subtitle, filename, special_tm = NULL, is_regular = TRUE) {
  season_type_filter <- if (is_regular) "Regular" else "Playoffs"
  
  # Apply conditional filtering before summarization
  if (!is.null(season_filter)) {
    data <- data %>% filter(season == season_filter)
  }
  
  data <- data %>% filter(season_type == season_type_filter)
  
  # Modify team names if needed before grouping and summarization
  if (!is.null(special_tm)) {
    data$tm <- ifelse(data$tm %in% special_tm, paste0(data$tm, " 🏆"), data$tm)
  }
  
  standings <- data %>% 
    group_by(tm) %>% 
    summarize(
      wins = sum(result),
      games = n(),
      pf = round(mean(tm_score), 1),
      pa = round(mean(opp_score), 1),
      .groups = 'drop'
    ) %>% 
    mutate(
      losses = games - wins,
      win_per = percent(wins / games, accuracy = 0.1),
      winp = wins / games
    ) %>% 
    arrange(-winp) %>% 
    select(tm, wins, losses, win_per, pf, pa) %>% 
    gt() %>% 
    tab_header(title = md(title), subtitle = md(subtitle)) %>% 
    gt_theme_espn() %>% 
    cols_align("left", columns = vars(tm)) %>% 
    tab_options(data_row.padding = px(3)) %>% 
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

# Generate standings for each season
for (season_details in seasons) {
  generate_standings(full_schedule, season_details$season, season_details$title, season_details$subtitle, season_details$filename, special_tm = season_details$special_tm)
}

# All-time standings (Regular Season)
generate_standings(full_schedule, NULL, "**Dynasty Insanity**", "**All-Time Standings**", "all_time_standings.png", special_tm = c("Naad", "Hosta", "Tom", "Tommy"), is_regular = TRUE)

# All-time standings (Playoffs)
generate_standings(full_schedule, NULL, "**Dynasty Insanity**", "**All-Time Playoff Standings**", "all_time_post_standings.png", special_tm = c("Naad", "Hosta", "Tom", "Tommy"), is_regular = FALSE)


# Additional Data Processing ---------------------------------------------------
# Load dp_values and process
dp <- dp_values("values-players.csv") %>%
  select(fp_id, player:age, ecr_1qb, value_1qb) %>%
  rename(
    dp_value = value_1qb,
    dp_ecr = ecr_1qb
  )

dp_draft <- dp_values("values-picks.csv") %>%
  select(draftpick = player, pick, ecr_1qb)

# Load FantasyPros rankings and join with dp
ffpros <- ffpros::fp_rankings("dynasty-overall") %>%
  rename(fp_id = fantasypros_id) %>%
  select(fp_id:team, fp_rank = rank, fp_ecr = ecr, headshot = player_image_url, fp_posrank = pos_rank, fp_tier = tier) %>%
  left_join(dp, by = c("fp_id" = "fp_id", "team" = "team", "pos")) %>%
  relocate(headshot, .after = last_col()) %>%
  select(fp_id:team, age, fp_rank:fp_tier, dp_ecr:dp_value, headshot)

# Correct team and player names
ffpros$team <- gsub("JAC", "JAX", ffpros$team)
ffpros$player_name <- gsub("Kenneth Walker Iii", "Kenneth Walker", ffpros$player_name)
ffpros$player_name <- gsub("Kenneth Walker III", "Kenneth Walker", ffpros$player_name)

# Load and process player IDs
id_map <- dp_playerids() %>%
  select(fp_id = fantasypros_id, name, sleeper_id) %>%
  mutate(name = stri_trans_totitle(gsub(",", " ", name)))

# Correct player names
id_map$name <- gsub("Kenneth Walker Iii", "Kenneth Walker", id_map$name)
id_map$name <- gsub("Kenneth Walker III", "Kenneth Walker", id_map$name)

# Fetch and process franchises
franchises_dynasty <- franchises %>%
  select(season, user_id, franchise_id, franchise_name, user_name) %>%
  distinct(season, user_id, .keep_all = TRUE)
franchises_team_dynasty <- franchises_dynasty %>%
  select(season, franchise_id, user_name)

# Fetch and process 2023 rosters
conn5 <- ff_connect(platform = "sleeper", league_id = 1048425511736713216, season = 2024)
starters24 <- ff_rosters(conn5) %>%
  select(-franchise_name) %>%
  mutate(season = 2024, franchise_id = as.integer(franchise_id))

# Join data to create team_dynasty
team_dynasty <- starters4 %>%
  left_join(id_map, by = c("player_id" = "sleeper_id")) %>%
  filter(pos != "DEF" & pos != "K") %>%
  relocate(fp_id) %>%
  left_join(ffpros, by = c("fp_id")) %>%
  select(season, franchise_id, fp_id, player_id, player_name = player_name.x, name = player_name.y, age = age.x, pos = pos.x, team = team.x, fp_rank:headshot) %>%
  arrange(fp_tier, -dp_value) %>%
  left_join(franchises_team_dynasty, by = c("season", "franchise_id")) %>%
  relocate(user_name, .before = 2)

# Apply team name lookup
team_dynasty$team_name <- sapply(team_dynasty$user_name, function(x) {
  if (x %in% names(lookup)) {
    return(lookup[x])
  } else {
    return(x)
  }
})

team_dynasty <- team_dynasty %>%
  mutate(
    team_name = ifelse(is.na(team_name), "JP", team_name),
    name = ifelse(is.na(team_name), player_name, name),
    player_name = ifelse(is.na(player_name), name, player_name)
  )

# Load and process PlayerProfiler data
playerprofiler <- read.csv('~/Desktop/fantasy-report/csv/playerprofiler.csv') %>%
  select(name = Full.Name, team = Team.Abbrev, pp_posrank = Positional.Rank, pp_lifetime = Lifetime.Value) %>%
  mutate(
    name = gsub("Kenneth Walker Iii", "Kenneth Walker", name),
    name = gsub("Kenneth Walker III", "Kenneth Walker", name)
  )

# Create team_dynasty_rosters
team_dynasty_rosters <- team_dynasty %>%
  left_join(playerprofiler, by = c("player_name" = "name")) %>%
  select(team_name, franchise_id:pos, team = team.x, fp_tier, fp_posrank, pp_posrank, pp_lifetime, fp_rank:fp_ecr, dp_ecr:dp_value, headshot) %>%
  mutate(season = "2024") %>%
  arrange(team_name, fp_tier, -pp_lifetime)

# TEAM ROSTER IMAGES --------------------

team_dynasty_rosters_df <- team_dynasty_rosters %>%
  select(headshot, team_name, name, team, pos, fp_posrank, fp_tier, fp_rank, fp_ecr, playerprofiler = pp_lifetime, dynastypros = dp_value)

gt_theme_pff <- function(data, ...) {
  data %>%
    # add spanner for PFF Grade
    tab_spanner(
      label = "2024 Season",
      columns = contains(c("fp_tier", "fp_posrank", "fp_rank"))
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
      fp_posrank = "Pos Rank",
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
          spanners = c("BLANK", "2024 Season", "Dynasty Value")
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
        columns = everything()
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
    select(headshot, name, team, pos, fp_tier, fp_posrank, fp_rank, playerprofiler, dynastypros) %>%
    gt() %>%
    tab_header(title = md(paste0("**", user_name1, " 2024 Dynasty Roster**"))) %>%
    cols_align("center", columns = 3:9) %>% 
    gt_img_rows(headshot) %>% 
    fmt_number(
      columns = 6:9,
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


################################

# Function to apply gt_theme_pff
gt_theme_pff <- function(data, ...) {
  data %>%
    tab_spanner(
      label = "Player Scoring",
      columns = contains(c("total_pts", "avg_pts"))
    ) %>%
    tab_spanner(
      label = "BLANK",
      columns = 1:3
    ) %>%
    cols_label(
      total_pts = "Total",
      avg_pts = "Average",
      player_name = "Player",
      headshot_url = "Image",
      pos = "Pos"
    ) %>%
    fmt_missing(
      columns = everything(),
      missing_text = "---"
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#585d93"),
        cell_text(color = "white"),
        cell_borders(sides = "all", color = "#585d93", weight = px(1))
      ),
      locations = list(
        cells_column_spanners(
          spanners = "Player Scoring"
        )
      )
    ) %>%
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
    tab_style(
      style = list(
        cell_text(color = "black", weight = "bold")
      ),
      locations = cells_body(
        columns = 1:5
      )
    ) %>%
    opt_all_caps() %>%
    opt_row_striping() %>%
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
    opt_table_font(
      font = c(
        google_font(name = "Roboto")
      )
    ) 
}

starters <- starters_final |>
  left_join(
  franchises %>% select(franchise_id, season, user_name),
  by = c("franchise_id", "season")
) %>%
  mutate(user_name = if_else(user_name %in% names(lookup), lookup[user_name], user_name))


headshot_players <- load_rosters(seasons = 2020:2024) |>
  select(sleeper_id, season, headshot_url)


team_dynasty2 <- starters %>%
  left_join(id_map, by = c("player_id" = "sleeper_id")) %>%
  relocate(player_id) %>%
  group_by(season, user_name, player_id, player_name, pos) %>%
  summarize(
    total_pts = sum(points, na.rm = TRUE),
    avg_pts = mean(points, na.rm = TRUE),
  )
  

# Apply team name lookup
team_dynasty2$team_name <- sapply(team_dynasty2$user_name, function(x) {
  if (x %in% names(lookup)) {
    return(lookup[x])
  } else {
    return(x)
  }
})


team_dynasty <- team_dynasty2 |>
  left_join(headshot_players, by = c("season", "player_id" = "sleeper_id")) |>
  select(headshot_url, season, team_name, user_name, player_id:avg_pts)

create_historical_table <- function(user_name1, szn) {
  
  # Import font from sysfonts
  sysfonts::font_add_google("Roboto", "Roboto")
  showtext_auto()
  
  # Filter user data
  user_data <- team_dynasty %>%
    filter(team_name == user_name1, season == szn) %>%
    select(headshot_url, user_name, season, pos, player_name, total_pts, avg_pts) %>%
    arrange(pos, -total_pts) 
  
  # Create the starting lineup
  starting_lineup <- bind_rows(
    user_data %>% filter(pos == "QB") %>% head(1),
    user_data %>% filter(pos == "RB") %>% head(2),
    user_data %>% filter(pos == "WR") %>% head(2),
    user_data %>% filter(pos == "TE") %>% head(1)
  )
  starting_lineup$roster <- "A-List"
  
  # Add flex players
  flex_players <- user_data %>%
    filter(!(player_name %in% starting_lineup$player_name)) %>%
    filter(pos %in% c("WR", "RB", "TE")) %>%
    head(n = 2)
  flex_players$roster <- "A-List"
  
  # Create bench
  bench <- user_data %>%
    filter(!(player_name %in% starting_lineup$player_name)) %>%
    filter(!(player_name %in% flex_players$player_name)) %>%
    filter(!pos %in% c("K", "DEF")) %>%
    arrange(-total_pts) %>%
    head(n = 12)
  bench$roster <- "Bench"
  
  # Combine all into final lineup
  final_lineup <- bind_rows(starting_lineup, flex_players, bench)
  
  # Generate gt table
  gt_table <- final_lineup %>%
    select(-user_name, -season, -player_id) %>%
    group_by(roster) %>%
    gt() %>%
    tab_header(title = md(paste0("**", user_name1, " ", szn, " Dynasty Roster**")),
               subtitle = "Ranked by Points Scored") %>%
    tab_source_note(source_note = md("**Bench**: Top-12 Only")) %>%
    gt_img_rows(headshot_url) %>% 
    fmt_number(columns = "total_pts",
               decimals = 0) %>%
    fmt_number(columns = "avg_pts",
               decimals = 1) %>%
    gt_theme_pff()
  
  return(gt_table)
}

# Get unique user names and seasons
unique_names <- unique(team_dynasty2$team_name)
unique_szns <- unique(team_dynasty2$season)

# Loop through each season and each user name to create and save the tables
for (name in unique_names) {
  for (szn in unique_szns) {
    gt_table <- create_historical_table(name, szn)
    gtsave(
      gt_table,
      paste0("output/rosters/", szn, "/dynasty_roster_", name, ".png")
    )
  }
}

# Head to Head -------------
full_schedule <- full_schedule %>%
  mutate(
    tm_score = as.numeric(tm_score),
    opp_score = as.numeric(opp_score),
    result = as.numeric(result))

full_schedule <- full_schedule %>%
  filter(!is.na(tm_score) & !is.na(opp_score) & !is.na(result))

head2head <- full_schedule %>%
  filter(season_type == "Regular") %>%
  select(tm, opp, tm_score, opp_score, result) %>%
  mutate(margin = tm_score - opp_score) %>%
  group_by(tm, opp) %>%
  summarise(
    games = n(),
    wins = sum(result, na.rm = TRUE),
    losses = games - wins,
    winperc = scales::percent(wins / games, accuracy = 0.1),
    pf = sum(tm_score, na.rm = TRUE),
    pa = sum(opp_score, na.rm = TRUE),
    margin = sum(margin, na.rm = TRUE),
    avg_mrg = round(margin / games, 1),
    .groups = "drop"
  ) %>%
  ungroup()

df <- head2head %>%
  select(tm, opp, wins, losses, winperc, pf, pa, avg_mrg)


gt_theme_schedule <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Roboto"),
        default_fonts()
      ),
      weight = "normal"
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "all", color = "black", weight = px(2)
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      )
    ) %>% 
    opt_row_striping() %>%
    # change overall table styling for borders and striping
    tab_options(
      column_labels.background.color = "#585d73",
      column_labels.font.size = px(16),
      heading.border.bottom.width = px(2),
      heading.border.bottom.color = "#585d73",
      heading.border.lr.width = px(2),
      heading.border.lr.color =  "#585d73",
      table_body.hlines.color = "#585d73",
      table.border.top.width = px(2),
      table.border.top.color = "#585d73",
      table.border.bottom.color = "#585d73",
      table.border.bottom.width = px(2),
      column_labels.border.top.width = px(2),
      column_labels.border.top.color = "#585d73",
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "#585d73",
      row_group.font.size = px(14),
      row.striping.background_color = "#ededed",
      data_row.padding= px(3),
      ...
    ) 
} 

create_headtohead <- function(tm1) {
  
  # Import font from sysfonts
  sysfonts::font_add_google("Roboto", "Roboto")
  showtext_auto()
  
  
  # Filter data for the specific user_name
  user_data <- df %>%
    filter(tm == tm1) %>%
    arrange(-wins, -avg_mrg) # arrange players by ECR
  
  # Create the gt table and apply filter
  gt_table <- user_data %>%
    select(-tm, opp, wins, losses, wp = winperc, pf, pa, avg_mrg) %>%
    gt() %>%
    tab_header(title = md("**All-Time Results (Head to Head)**")) |>
    fmt_number(
      columns = 4:6,
      decimals = 1
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

###### SZN RECAP
seasonrecap <- full_schedule |>
  filter(season_type != "Consolation") 

get_season_recap <- function(tm1, year) {
  # Import font from sysfonts
  sysfonts::font_add_google("Roboto", "Roboto")
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
  head(20)

# GT TABLE ----
gt_table <- blowouts %>%
  select(season, week, victor = tm, loser = opp, pf = tm_score, pa = opp_score, margin) %>%
  gt() %>%
  tab_header(title = md("**Top-20 Blowouts**")) %>%
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
  head(20)


# Create the gt table and apply filter
gt_table <- closecalls %>%
  select(season, week, victor = tm, loser = opp, pf = tm_score, pa = opp_score, margin) %>%
  gt() %>%
  tab_header(title = md("**Top-20 Close Calls**")) %>%
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

#### POWER RANK
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
    cols_align("center", columns = 1:3) %>%
    tab_options(data_row.padding = px(5)) %>%
    tab_source_note(md("**Calc**: 60% Avg PF, 20% Win %, 20% PF Variance"))
  
  gtsave(gt_table, file_path)
}


season_years <- unique(full_schedule$season)

for (season_year in season_years) {
  power_rankings <- calculate_power_rankings(full_schedule, season_year)
  file_path <- paste0('output/history/power_rank_standings_yearly_', season_year, 'v2_.png')
  generate_and_save_table(power_rankings, season_year, file_path)
}



#### CONSOLIDATED PWER RANK
# Define the colors for each season
season_colors <- c("2020" = "#FFDFD3", "2021" = "#FFFFB5", "2022" = "#D4F1F4", "2023" = "#DAD7CD")

#### POWER RANK
calculate_power_rankings_all <- function(season_data) {
  # Calculate power rankings for all seasons
  power_rankings <- season_data %>%
    filter(season_type != "Consolation") %>%
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

apply_row_stripping <- function(data, colors) {
  data %>%
    rowwise() %>%
    mutate(row_color = colors[as.character(season)]) %>%
    ungroup()
}

generate_and_save_table <- function(power_rankings, season_year, file_path, colors) {
  power_rankings <- apply_row_stripping(power_rankings, colors)
  
  gt_table <- power_rankings %>%
    gt() %>%
    tab_header(title = md(paste(season_year, "Power Rankings")),
               subtitle = "Adj by League Season Avg") %>%
    gt_theme_schedule() %>%
    cols_align("center", columns = 1:3) %>%
    tab_options(data_row.padding = px(5)) %>%
    tab_style(
      style = list(
        cell_fill(color = row_color)
      ),
      locations = cells_body(rows = TRUE)
    ) %>%
    tab_source_note(md("**Calc**: 60% Avg PF, 20% Win %, 20% PF Variance"))
  
  gtsave(gt_table, file_path)
}

generate_and_save_combined_table <- function(combined_power_rankings, file_path, colors) {
  combined_power_rankings <- apply_row_stripping(combined_power_rankings, colors)
  
  gt_table <- combined_power_rankings %>%
    gt() %>%
    tab_header(title = md("All Seasons Power Rankings"),
               subtitle = "Adj by League Season Avg") %>%
    gt_theme_schedule() %>%
    cols_align("center", columns = 1:3) %>%
    tab_options(data_row.padding = px(5)) %>%
    tab_style(
      style = list(
        cell_fill(color = row_color)
      ),
      locations = cells_body(rows = TRUE)
    ) %>%
    tab_source_note(md("**Calc**: 60% Avg PF, 20% Win %, 20% PF Variance"))
  
  gtsave(gt_table, file_path)
}

season_years <- unique(full_schedule$season)

for (season_year in season_years) {
  power_rankings <- calculate_power_rankings(full_schedule, season_year)
  file_path <- paste0('output/history/power_rank_standings_yearly_', season_year, 'v2_.png')
  generate_and_save_table(power_rankings, season_year, file_path, season_colors)
}

# Calculate power rankings for all seasons
combined_power_rankings <- calculate_power_rankings_all(full_schedule)

# Save the combined power rankings table
file_path_combined <- 'output/history/combined_power_rankings.png'
generate_and_save_combined_table(combined_power_rankings, file_path_combined, season_colors)


#### Summary Teams

conn_season_24 = ff_connect(platform = "sleeper", league_id = 1048425511736713216, season = 2024)

new_year_rosters <- ff_rosters(conn_season_24)

player_values <- dp_values("values-players.csv")

# The values are stored by fantasypros ID since that's where the data comes from. 
# To join it to our rosters, we'll need playerID mappings.

player_ids <- dp_playerids() %>% 
  select(sleeper_id,fantasypros_id)

player_values <- player_values %>% 
  left_join(player_ids, by = c("fp_id" = "fantasypros_id")) %>% 
  select(sleeper_id,ecr_1qb,ecr_pos,value_1qb)

# Drilling down to just 1QB values and IDs, we'll be joining it onto rosters and don't need the extra stuff

roster_values <- new_year_rosters %>% 
  left_join(player_values, by = c("player_id"="sleeper_id")) %>% 
  arrange(franchise_id,desc(value_1qb)) 

headshot_players <- headshot_players |>
  filter(season == 2024) |>
  distinct()

roster_values_head <- roster_values %>%
  left_join(headshot_players, by = c("player_id" = "sleeper_id"))

# Select and rename the relevant columns
roster_values <- roster_values_head %>%
  select(franchise_name, headshot_url, player_name, pos, team, age, ecr = ecr_1qb, ecr_pos, value_1qb)

# Create and save a table for each unique franchise_name
unique_franchises <- unique(roster_values$franchise_name)


for (franchise in unique_franchises) {
  franchise_data <- roster_values %>%
    filter(franchise_name == franchise) %>%
    select(-franchise_name) %>%
    head(20)
  
  table <- franchise_data %>%
    gt() %>%
    gtExtras::gt_img_rows(columns = headshot_url) %>%
    cols_label(
      player_name = "Name",
      pos = "Position",
      team = "Team",
      age = "Age",
      ecr = "ECR (1QB)",
      ecr_pos = "ECR Position",
      value_1qb = "Value (1QB)",
      headshot_url = "Headshot"
    ) %>%
    tab_header(
      title = md(paste0("**", franchise, " - 2024 Roster Value (Top-20 Players)**")),
      subtitle = md("Player values for the 2024 season")
    ) %>%
    cols_align(
      align = "center",
      columns = c(headshot_url, player_name, pos, team, age, ecr, ecr_pos, value_1qb)
    ) %>%
    gtExtras::gt_theme_538() %>%
    fmt_number(
      columns = c(age, ecr, ecr_pos, value_1qb),
      decimals = 1
    )
  
  gtsave(
    table,
    filename = file.path('output/2024/', paste0(gsub(" ", "_", franchise), "_2024_Roster_Value.png")),
    zoom = 1.5
  )
}

value_summary <- roster_values %>% 
  group_by(franchise_name,pos) %>% 
  summarise(total_value = sum(value_1qb,na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(franchise_name) %>% 
  mutate(team_value = sum(total_value)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = pos, values_from = total_value) %>% 
  arrange(desc(team_value)) %>%
  select(franchise_name, team_value, QB, RB, WR, TE)

# Create the gt table
gt_table <- value_summary %>%
  gt() %>%
  cols_label(
    franchise_name = "Franchise Name",
    team_value = "Team Value",
    QB = "QB Value",
    RB = "RB Value",
    WR = "WR Value",
    TE = "TE Value"
  ) %>%
  tab_header(
    title = md("**2024 Team Value Summary**"),
    subtitle = md("Breakdown of Team Dynasty Values by Position (DynastyProcess.com)")
  ) %>%
  cols_align(
    align = "center",
    columns = c(team_value, QB, RB, WR, TE)
  ) %>%
  gtExtras::gt_theme_538() %>%
  opt_all_caps() %>%
  fmt_number(
    columns = c(team_value, QB, RB, WR, TE),
    decimals = 0
  )

# Return the gt table
gtsave(
  gt_table,
  filename = file.path('output/2024/', "2024_Roster_Value_by_Position.png"),
  zoom = 1.5
)

value_summary_pct <- value_summary %>% 
  mutate_at(c("team_value","QB","RB","WR","TE"),~.x/sum(.x)) %>% 
  mutate_at(c("team_value","QB","RB","WR","TE"),round, 3)


# Create the gt table
gt_table <- value_summary_pct %>%
  gt() %>%
  cols_label(
    franchise_name = "Franchise Name",
    team_value = "Team Value",
    QB = "QB Value",
    RB = "RB Value",
    WR = "WR Value",
    TE = "TE Value"
  ) %>%
  tab_header(
    title = md("**2024 Team Value Summary**"),
    subtitle = md("Breakdown of Team Dynasty Values as %% of League Total by Position")
  ) %>%
  cols_align(
    align = "center",
    columns = c(team_value, QB, RB, WR, TE)
  ) %>%
  gtExtras::gt_theme_538() %>%
  fmt_percent(
    columns = c(team_value, QB, RB, WR, TE),
    decimals = 2
  )
gt_table
# Return the gt table
gtsave(
  gt_table,
  filename = file.path('output/2024/', "2024_Roster_Value_as_Percent_by_Position.png"),
  zoom = 1.5)


age_summary <- roster_values %>%
  group_by(franchise_name, pos) %>%
  mutate(position_value = sum(value_1qb, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(weighted_age = age * value_1qb / position_value,
         weighted_age = round(weighted_age, 1)) %>%
  group_by(franchise_name, pos) %>%
  summarise(count = n(),
            age = sum(weighted_age, na.rm = TRUE)) %>%
  pivot_wider(names_from = pos, values_from = c(age, count)) %>%
  select(-age_DEF, -age_K, -count_DEF, -count_K)%>%
  arrange(franchise_name) %>%
  ungroup()

head(age_summary)
# Create the gt table
gt_table <- age_summary %>%
  gt() %>%
  cols_label(
    franchise_name = "Franchise Name",
    age_QB = "QB Age",
    age_RB = "RB Age",
    age_WR = "WR Age",
    age_TE = "TE Age",
    count_QB = "QB Count",
    count_RB = "RB Count",
    count_WR = "WR Count",
    count_TE = "TE Count"
  ) %>%
  tab_header(
    title = md("**2024 Team Age Summary**"),
    subtitle = md("Breakdown of Team Age by Position")
  ) %>%
  cols_align(
    align = "left",
    columns = 1
  ) %>%
  cols_align(
    align = "center",
    columns = 2:9
  ) %>%
  gtExtras::gt_theme_538() %>%
  fmt_number(
    columns = starts_with("age"),
    decimals = 1
  ) %>%
  fmt_number(
    columns = starts_with("count"),
    decimals = 0
  )

# Return the gt table
gt_table

# Return the gt table
gtsave(
  gt_table,
  filename = file.path('output/2024/', "2024_Roster_Age_by_Position.png"),
  zoom = 1.5)
